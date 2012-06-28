

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Core/Trf/RenUniq.ag)
module EH101.Core.Trf.RenUniq(RenUniqOpts (..), emptyRenUniqOpts
, cmodTrfRenUniq) where

import Data.Maybe
import Data.Char
import Control.Monad (liftM)
import qualified Data.Map as Map
import EH.Util.Utils
import EH101.Base.Common
import EH101.Base.Builtin
import EH101.Core
import EH101.Ty
import EH101.AbstractCore
import EH101.Base.Debug
import EH.Util.Pretty













data RenUniqOpts
  = RenUniqOpts
      { renuniqOptResetOnlyInLam    :: Bool             -- restart numbering in lambda only, throwing away all name modifiers previously added
      }

emptyRenUniqOpts :: RenUniqOpts
emptyRenUniqOpts = RenUniqOpts False



cmodTrfRenUniq :: RenUniqOpts -> CModule -> CModule
cmodTrfRenUniq ropts cmod
  =  let  t = wrap_CodeAGItf  (sem_CodeAGItf (CodeAGItf_AGItf cmod))
                              (Inh_CodeAGItf { gIniq_Inh_CodeAGItf = 1
                                             -- , gUniq_Inh_CodeAGItf = uidStart
                                             , ropts_Inh_CodeAGItf = ropts
                                             })
     in   cTrf_Syn_CodeAGItf t



newIniq :: Int -> Int -> (Int,[Int])
newIniq nr seed = (seed+nr, [seed .. seed + nr -1])



type NmMp = Map.Map HsName HsName
type ARenMp
  = ( NmMp          -- forward map, from old -> new
    , NmMp          -- backward map, from new -> old
    )

emptyARenMp :: ARenMp
emptyARenMp = (Map.empty, Map.empty)



-- breaks assumption that globals are qualified, locals not. Problem in future when records are used for globals and access differs.
mkUniqGlob :: NmMp -> HsName -> HsName -> Maybe Int -> (HsName,NmMp)
mkUniqGlob mb q n mbU
  = (
      (if hsnIsQual n
       then id
       else hsnSetQual q
      ) $
      maybe n (\u -> hsnUniqifyInt HsNameUniqifier_GloballyUnique u n) mbU
    , mb
    )



-- breaks assumption that globals are qualified, locals not. Problem in future when records are used for globals and access differs.
mkUniqLoc :: NmMp -> HsName -> HsName -> Maybe Int -> (HsName,NmMp)
mkUniqLoc mb q n mbU
  = ( -- tr "mkUniqLoc" (n >#< n2 >#< n3 >#< n5) $
      n5
    , Map.insert n5 n mb
    )
  where n2 = hsnStripUniqifiers $ hsnQualified n
        n3 = case hsnBaseUnpack n2 of
               Just ('_':_, _) -> mkHNm "_"
               _               -> n2
        n5 | Map.member n3 mb = hsnUniqifyInt HsNameUniqifier_Blank (panicJust "RenUniq.mkUniqLoc" mbU) $ hsnStripUniqifiers n3
           | otherwise        = n3
           -- where mbExists@(~(Just n4)) = Map.lookup n3 mb



aRenAdd :: RenUniqOpts -> Bool -> HsName -> [HsName] -> [Int] -> ARenMp -> ARenMp
aRenAdd ropts isGlob q nL uL mfb@(mf,mb)
  = foldr (\(n,u) (mf,mb) -> let (n',mb') = mkNm mb n u in (Map.insert n n' mf,mb')) mfb $ zip nL uL
  where mkNm mb n u
          | doChng && isUnqualifiedGlob
                       = mkUniq mb q n Nothing
          | doChng     = mkUniq mb q n (Just u)
          | otherwise  = (n,mb)
          where doChng
                   = isChangeable
                     || isUnqualifiedGlob
                isUnqualifiedGlob = isGlob && not onlyLam && isNothing (hsnQualifier n) && n /= hsnMain
                isChangeable = not isGlob || n `Map.member` mf
                onlyLam = renuniqOptResetOnlyInLam ropts
                mkUniq | onlyLam   = mkUniqLoc
                       | otherwise = mkUniqGlob



aRenRepl :: ARenMp -> HsName -> HsName
aRenRepl (mf,_) n = maybe n id . Map.lookup n $ mf

-- CAlt --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         protectedBindingNames : [HsName]
         ropts                : RenUniqOpts
      chained attribute:
         gIniq                : Int
   visit 1:
      inherited attributes:
         aRenMp               : ARenMp
         lev                  : Int
         moduleNm             : HsName
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Alt:
         child pat            : CPat 
         child expr           : CExpr 
         visit 0:
            local _tup1       : _
         visit 1:
            local lIniq       : _
            local aRenMp      : _
            local cTrf        : _
            intra _tup1       : _
-}
-- cata
sem_CAlt :: CAlt  ->
            T_CAlt 
sem_CAlt (CAlt_Alt _pat _expr )  =
    (sem_CAlt_Alt (sem_CPat _pat ) (sem_CExpr _expr ) )
-- semantic domain
type T_CAlt  = Int ->
               ([HsName]) ->
               RenUniqOpts ->
               ( Int,T_CAlt_1 )
type T_CAlt_1  = ARenMp ->
                 Int ->
                 HsName ->
                 ( CAlt )
sem_CAlt_Alt :: T_CPat  ->
                T_CExpr  ->
                T_CAlt 
sem_CAlt_Alt pat_ expr_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIropts) of
          { _exprOropts | _exprOropts `seq` (True) ->
          (case (_lhsIprotectedBindingNames) of
           { _exprOprotectedBindingNames | _exprOprotectedBindingNames `seq` (True) ->
           (case (_lhsIropts) of
            { _patOropts | _patOropts `seq` (True) ->
            (case (pat_ ) of
             { ( _patInmL,pat_1) | True ->
                 (case (newIniq (length _patInmL) _lhsIgIniq) of
                  { __tup1 | __tup1 `seq` (True) ->
                  (case (__tup1) of
                   { (_patOgIniq,_) | _patOgIniq `seq` (True) ->
                   (case (pat_1 _patOgIniq _patOropts ) of
                    { ( _patIgIniq,pat_2) | True ->
                        (case (_patIgIniq) of
                         { _exprOgIniq | _exprOgIniq `seq` (True) ->
                         (case (expr_ _exprOgIniq _exprOprotectedBindingNames _exprOropts ) of
                          { ( _exprIgIniq,_exprIisLamBody,expr_1) | True ->
                              (case (_exprIgIniq) of
                               { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                               (case ((let sem_CAlt_Alt_1 :: T_CAlt_1 
                                           sem_CAlt_Alt_1  =
                                               (\ _lhsIaRenMp
                                                  _lhsIlev
                                                  _lhsImoduleNm ->
                                                    (case (_lhsImoduleNm) of
                                                     { _exprOmoduleNm | _exprOmoduleNm `seq` (True) ->
                                                     (case (_lhsIlev + 1) of
                                                      { _exprOlev | _exprOlev `seq` (True) ->
                                                      (case (__tup1) of
                                                       { (_,_lIniq) | _lIniq `seq` (True) ->
                                                       (case (aRenAdd _lhsIropts False
                                                                      _lhsImoduleNm
                                                                      _patInmL _lIniq _lhsIaRenMp) of
                                                        { _exprOaccumARenMp | _exprOaccumARenMp `seq` (True) ->
                                                        (case (expr_1 _exprOaccumARenMp _exprOlev _exprOmoduleNm ) of
                                                         { ( _exprIaccumARenMp,expr_2) | True ->
                                                             (case (_exprIaccumARenMp) of
                                                              { _aRenMp | _aRenMp `seq` (True) ->
                                                              (case (_aRenMp) of
                                                               { _exprOaRenMp | _exprOaRenMp `seq` (True) ->
                                                               (case (_lhsImoduleNm) of
                                                                { _patOmoduleNm | _patOmoduleNm `seq` (True) ->
                                                                (case (_lhsIlev) of
                                                                 { _patOlev | _patOlev `seq` (True) ->
                                                                 (case (_aRenMp) of
                                                                  { _patOaRenMp | _patOaRenMp `seq` (True) ->
                                                                  (case (expr_2 _exprOaRenMp ) of
                                                                   { ( _exprIcTrf) | True ->
                                                                       (case (pat_2 _patOaRenMp _patOlev _patOmoduleNm ) of
                                                                        { ( _patIcTrf,_patIfldNmL) | True ->
                                                                            (case (CAlt_Alt _patIcTrf _exprIcTrf) of
                                                                             { _cTrf | _cTrf `seq` (True) ->
                                                                             (case (_cTrf) of
                                                                              { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                              ( _lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                       in  sem_CAlt_Alt_1)) of
                                { ( sem_CAlt_1) | True ->
                                ( _lhsOgIniq,sem_CAlt_1) }) }) }) }) }) }) }) }) }) }) }))
-- CAltL -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         protectedBindingNames : [HsName]
         ropts                : RenUniqOpts
      chained attribute:
         gIniq                : Int
   visit 1:
      inherited attributes:
         aRenMp               : ARenMp
         lev                  : Int
         moduleNm             : HsName
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : CAlt 
         child tl             : CAltL 
         visit 1:
            local cTrf        : _
      alternative Nil:
         visit 1:
            local cTrf        : _
-}
-- cata
sem_CAltL :: CAltL  ->
             T_CAltL 
sem_CAltL list  =
    (Prelude.foldr sem_CAltL_Cons sem_CAltL_Nil (Prelude.map sem_CAlt list) )
-- semantic domain
type T_CAltL  = Int ->
                ([HsName]) ->
                RenUniqOpts ->
                ( Int,T_CAltL_1 )
type T_CAltL_1  = ARenMp ->
                  Int ->
                  HsName ->
                  ( CAltL )
sem_CAltL_Cons :: T_CAlt  ->
                  T_CAltL  ->
                  T_CAltL 
sem_CAltL_Cons hd_ tl_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIropts) of
          { _tlOropts | _tlOropts `seq` (True) ->
          (case (_lhsIprotectedBindingNames) of
           { _tlOprotectedBindingNames | _tlOprotectedBindingNames `seq` (True) ->
           (case (_lhsIropts) of
            { _hdOropts | _hdOropts `seq` (True) ->
            (case (_lhsIprotectedBindingNames) of
             { _hdOprotectedBindingNames | _hdOprotectedBindingNames `seq` (True) ->
             (case (_lhsIgIniq) of
              { _hdOgIniq | _hdOgIniq `seq` (True) ->
              (case (hd_ _hdOgIniq _hdOprotectedBindingNames _hdOropts ) of
               { ( _hdIgIniq,hd_1) | True ->
                   (case (_hdIgIniq) of
                    { _tlOgIniq | _tlOgIniq `seq` (True) ->
                    (case (tl_ _tlOgIniq _tlOprotectedBindingNames _tlOropts ) of
                     { ( _tlIgIniq,tl_1) | True ->
                         (case (_tlIgIniq) of
                          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                          (case ((let sem_CAltL_Cons_1 :: T_CAltL_1 
                                      sem_CAltL_Cons_1  =
                                          (\ _lhsIaRenMp
                                             _lhsIlev
                                             _lhsImoduleNm ->
                                               (case (_lhsImoduleNm) of
                                                { _tlOmoduleNm | _tlOmoduleNm `seq` (True) ->
                                                (case (_lhsIlev) of
                                                 { _tlOlev | _tlOlev `seq` (True) ->
                                                 (case (_lhsIaRenMp) of
                                                  { _tlOaRenMp | _tlOaRenMp `seq` (True) ->
                                                  (case (_lhsImoduleNm) of
                                                   { _hdOmoduleNm | _hdOmoduleNm `seq` (True) ->
                                                   (case (_lhsIlev) of
                                                    { _hdOlev | _hdOlev `seq` (True) ->
                                                    (case (_lhsIaRenMp) of
                                                     { _hdOaRenMp | _hdOaRenMp `seq` (True) ->
                                                     (case (tl_1 _tlOaRenMp _tlOlev _tlOmoduleNm ) of
                                                      { ( _tlIcTrf) | True ->
                                                          (case (hd_1 _hdOaRenMp _hdOlev _hdOmoduleNm ) of
                                                           { ( _hdIcTrf) | True ->
                                                               (case ((:) _hdIcTrf _tlIcTrf) of
                                                                { _cTrf | _cTrf `seq` (True) ->
                                                                (case (_cTrf) of
                                                                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                 ( _lhsOcTrf) }) }) }) }) }) }) }) }) }) }))
                                  in  sem_CAltL_Cons_1)) of
                           { ( sem_CAltL_1) | True ->
                           ( _lhsOgIniq,sem_CAltL_1) }) }) }) }) }) }) }) }) }) }))
sem_CAltL_Nil :: T_CAltL 
sem_CAltL_Nil  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case ((let sem_CAltL_Nil_1 :: T_CAltL_1 
                      sem_CAltL_Nil_1  =
                          (\ _lhsIaRenMp
                             _lhsIlev
                             _lhsImoduleNm ->
                               (case ([]) of
                                { _cTrf | _cTrf `seq` (True) ->
                                (case (_cTrf) of
                                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                 ( _lhsOcTrf) }) }))
                  in  sem_CAltL_Nil_1)) of
           { ( sem_CAltL_1) | True ->
           ( _lhsOgIniq,sem_CAltL_1) }) }))
-- CBind -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         protectedBindingNames : [HsName]
      synthesized attributes:
         nm                   : HsName
         nmL                  : [HsName]
   visit 1:
      inherited attribute:
         ropts                : RenUniqOpts
      chained attribute:
         gIniq                : Int
   visit 2:
      inherited attributes:
         aRenMp               : ARenMp
         lev                  : Int
         moduleNm             : HsName
      synthesized attribute:
         cTrf                 : SELF 
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
type T_CBind  = ([HsName]) ->
                ( HsName,([HsName]),T_CBind_1 )
type T_CBind_1  = Int ->
                  RenUniqOpts ->
                  ( Int,T_CBind_2 )
type T_CBind_2  = ARenMp ->
                  Int ->
                  HsName ->
                  ( CBind )
sem_CBind_Bind :: HsName ->
                  T_CBoundL  ->
                  T_CBind 
sem_CBind_Bind nm_ bindAspects_  =
    (\ _lhsIprotectedBindingNames ->
         (case (nm_) of
          { _lhsOnm | _lhsOnm `seq` (True) ->
          (case (if   nm_ `elem` _lhsIprotectedBindingNames
                 then []
                 else [nm_]) of
           { _lhsOnmL | _lhsOnmL `seq` (True) ->
           (case ((let sem_CBind_Bind_1 :: T_CBind_1 
                       sem_CBind_Bind_1  =
                           (\ _lhsIgIniq
                              _lhsIropts ->
                                (case (_lhsIropts) of
                                 { _bindAspectsOropts | _bindAspectsOropts `seq` (True) ->
                                 (case (_lhsIprotectedBindingNames) of
                                  { _bindAspectsOprotectedBindingNames | _bindAspectsOprotectedBindingNames `seq` (True) ->
                                  (case (_lhsIgIniq) of
                                   { _bindAspectsOgIniq | _bindAspectsOgIniq `seq` (True) ->
                                   (case (bindAspects_ _bindAspectsOgIniq _bindAspectsOprotectedBindingNames _bindAspectsOropts ) of
                                    { ( _bindAspectsIgIniq,bindAspects_1) | True ->
                                        (case (_bindAspectsIgIniq) of
                                         { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                                         (case ((let sem_CBind_Bind_2 :: T_CBind_2 
                                                     sem_CBind_Bind_2  =
                                                         (\ _lhsIaRenMp
                                                            _lhsIlev
                                                            _lhsImoduleNm ->
                                                              (case (_lhsImoduleNm) of
                                                               { _bindAspectsOmoduleNm | _bindAspectsOmoduleNm `seq` (True) ->
                                                               (case (_lhsIlev) of
                                                                { _bindAspectsOlev | _bindAspectsOlev `seq` (True) ->
                                                                (case (_lhsIaRenMp) of
                                                                 { _bindAspectsOaRenMp | _bindAspectsOaRenMp `seq` (True) ->
                                                                 (case (nm_) of
                                                                  { _bindAspectsOnm | _bindAspectsOnm `seq` (True) ->
                                                                  (case (bindAspects_1 _bindAspectsOaRenMp _bindAspectsOlev _bindAspectsOmoduleNm _bindAspectsOnm ) of
                                                                   { ( _bindAspectsIcTrf,_bindAspectsInmL) | True ->
                                                                       (case (CBind_Bind (aRenRepl _lhsIaRenMp nm_) _bindAspectsIcTrf) of
                                                                        { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                        ( _lhsOcTrf) }) }) }) }) }) }))
                                                 in  sem_CBind_Bind_2)) of
                                          { ( sem_CBind_2) | True ->
                                          ( _lhsOgIniq,sem_CBind_2) }) }) }) }) }) }))
                   in  sem_CBind_Bind_1)) of
            { ( sem_CBind_1) | True ->
            ( _lhsOnm,_lhsOnmL,sem_CBind_1) }) }) }))
-- CBindAnn ----------------------------------------------------
{-
   visit 0:
      chained attribute:
         gIniq                : Int
   visit 1:
      inherited attributes:
         aRenMp               : ARenMp
         lev                  : Int
         moduleNm             : HsName
         ropts                : RenUniqOpts
      synthesized attributes:
         cTrf                 : SELF 
         nmL                  : [HsName]
   alternatives:
      alternative Coe:
         child coe            : {RelevCoe}
         visit 1:
            local cTrf        : _
-}
-- cata
sem_CBindAnn :: CBindAnn  ->
                T_CBindAnn 
sem_CBindAnn (CBindAnn_Coe _coe )  =
    (sem_CBindAnn_Coe _coe )
-- semantic domain
type T_CBindAnn  = Int ->
                   ( Int,T_CBindAnn_1 )
type T_CBindAnn_1  = ARenMp ->
                     Int ->
                     HsName ->
                     RenUniqOpts ->
                     ( CBindAnn ,([HsName]))
sem_CBindAnn_Coe :: RelevCoe ->
                    T_CBindAnn 
sem_CBindAnn_Coe coe_  =
    (\ _lhsIgIniq ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case ((let sem_CBindAnn_Coe_1 :: T_CBindAnn_1 
                      sem_CBindAnn_Coe_1  =
                          (\ _lhsIaRenMp
                             _lhsIlev
                             _lhsImoduleNm
                             _lhsIropts ->
                               (case (CBindAnn_Coe coe_) of
                                { _cTrf | _cTrf `seq` (True) ->
                                (case (_cTrf) of
                                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                 (case ([]) of
                                  { _lhsOnmL | _lhsOnmL `seq` (True) ->
                                  ( _lhsOcTrf,_lhsOnmL) }) }) }))
                  in  sem_CBindAnn_Coe_1)) of
           { ( sem_CBindAnn_1) | True ->
           ( _lhsOgIniq,sem_CBindAnn_1) }) }))
-- CBindAnnL ---------------------------------------------------
{-
   visit 0:
      chained attribute:
         gIniq                : Int
   visit 1:
      inherited attributes:
         aRenMp               : ARenMp
         lev                  : Int
         moduleNm             : HsName
         ropts                : RenUniqOpts
      synthesized attributes:
         cTrf                 : SELF 
         nmL                  : [HsName]
   alternatives:
      alternative Cons:
         child hd             : CBindAnn 
         child tl             : CBindAnnL 
         visit 1:
            local cTrf        : _
      alternative Nil:
         visit 1:
            local cTrf        : _
-}
-- cata
sem_CBindAnnL :: CBindAnnL  ->
                 T_CBindAnnL 
sem_CBindAnnL list  =
    (Prelude.foldr sem_CBindAnnL_Cons sem_CBindAnnL_Nil (Prelude.map sem_CBindAnn list) )
-- semantic domain
type T_CBindAnnL  = Int ->
                    ( Int,T_CBindAnnL_1 )
type T_CBindAnnL_1  = ARenMp ->
                      Int ->
                      HsName ->
                      RenUniqOpts ->
                      ( CBindAnnL ,([HsName]))
sem_CBindAnnL_Cons :: T_CBindAnn  ->
                      T_CBindAnnL  ->
                      T_CBindAnnL 
sem_CBindAnnL_Cons hd_ tl_  =
    (\ _lhsIgIniq ->
         (case (_lhsIgIniq) of
          { _hdOgIniq | _hdOgIniq `seq` (True) ->
          (case (hd_ _hdOgIniq ) of
           { ( _hdIgIniq,hd_1) | True ->
               (case (_hdIgIniq) of
                { _tlOgIniq | _tlOgIniq `seq` (True) ->
                (case (tl_ _tlOgIniq ) of
                 { ( _tlIgIniq,tl_1) | True ->
                     (case (_tlIgIniq) of
                      { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                      (case ((let sem_CBindAnnL_Cons_1 :: T_CBindAnnL_1 
                                  sem_CBindAnnL_Cons_1  =
                                      (\ _lhsIaRenMp
                                         _lhsIlev
                                         _lhsImoduleNm
                                         _lhsIropts ->
                                           (case (_lhsIropts) of
                                            { _tlOropts | _tlOropts `seq` (True) ->
                                            (case (_lhsImoduleNm) of
                                             { _tlOmoduleNm | _tlOmoduleNm `seq` (True) ->
                                             (case (_lhsIlev) of
                                              { _tlOlev | _tlOlev `seq` (True) ->
                                              (case (_lhsIaRenMp) of
                                               { _tlOaRenMp | _tlOaRenMp `seq` (True) ->
                                               (case (tl_1 _tlOaRenMp _tlOlev _tlOmoduleNm _tlOropts ) of
                                                { ( _tlIcTrf,_tlInmL) | True ->
                                                    (case (_lhsIropts) of
                                                     { _hdOropts | _hdOropts `seq` (True) ->
                                                     (case (_lhsImoduleNm) of
                                                      { _hdOmoduleNm | _hdOmoduleNm `seq` (True) ->
                                                      (case (_lhsIlev) of
                                                       { _hdOlev | _hdOlev `seq` (True) ->
                                                       (case (_lhsIaRenMp) of
                                                        { _hdOaRenMp | _hdOaRenMp `seq` (True) ->
                                                        (case (hd_1 _hdOaRenMp _hdOlev _hdOmoduleNm _hdOropts ) of
                                                         { ( _hdIcTrf,_hdInmL) | True ->
                                                             (case ((:) _hdIcTrf _tlIcTrf) of
                                                              { _cTrf | _cTrf `seq` (True) ->
                                                              (case (_cTrf) of
                                                               { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                               (case (_hdInmL ++ _tlInmL) of
                                                                { _lhsOnmL | _lhsOnmL `seq` (True) ->
                                                                ( _lhsOcTrf,_lhsOnmL) }) }) }) }) }) }) }) }) }) }) }) }) }))
                              in  sem_CBindAnnL_Cons_1)) of
                       { ( sem_CBindAnnL_1) | True ->
                       ( _lhsOgIniq,sem_CBindAnnL_1) }) }) }) }) }) }))
sem_CBindAnnL_Nil :: T_CBindAnnL 
sem_CBindAnnL_Nil  =
    (\ _lhsIgIniq ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case ((let sem_CBindAnnL_Nil_1 :: T_CBindAnnL_1 
                      sem_CBindAnnL_Nil_1  =
                          (\ _lhsIaRenMp
                             _lhsIlev
                             _lhsImoduleNm
                             _lhsIropts ->
                               (case ([]) of
                                { _cTrf | _cTrf `seq` (True) ->
                                (case (_cTrf) of
                                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                 (case ([]) of
                                  { _lhsOnmL | _lhsOnmL `seq` (True) ->
                                  ( _lhsOcTrf,_lhsOnmL) }) }) }))
                  in  sem_CBindAnnL_Nil_1)) of
           { ( sem_CBindAnnL_1) | True ->
           ( _lhsOgIniq,sem_CBindAnnL_1) }) }))
-- CBindL ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         protectedBindingNames : [HsName]
      synthesized attribute:
         nmL                  : [HsName]
   visit 1:
      inherited attribute:
         ropts                : RenUniqOpts
      chained attribute:
         gIniq                : Int
   visit 2:
      inherited attributes:
         aRenMp               : ARenMp
         lev                  : Int
         moduleNm             : HsName
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : CBind 
         child tl             : CBindL 
         visit 2:
            local cTrf        : _
      alternative Nil:
         visit 2:
            local cTrf        : _
-}
-- cata
sem_CBindL :: CBindL  ->
              T_CBindL 
sem_CBindL list  =
    (Prelude.foldr sem_CBindL_Cons sem_CBindL_Nil (Prelude.map sem_CBind list) )
-- semantic domain
type T_CBindL  = ([HsName]) ->
                 ( ([HsName]),T_CBindL_1 )
type T_CBindL_1  = Int ->
                   RenUniqOpts ->
                   ( Int,T_CBindL_2 )
type T_CBindL_2  = ARenMp ->
                   Int ->
                   HsName ->
                   ( CBindL )
sem_CBindL_Cons :: T_CBind  ->
                   T_CBindL  ->
                   T_CBindL 
sem_CBindL_Cons hd_ tl_  =
    (\ _lhsIprotectedBindingNames ->
         (case (_lhsIprotectedBindingNames) of
          { _tlOprotectedBindingNames | _tlOprotectedBindingNames `seq` (True) ->
          (case (_lhsIprotectedBindingNames) of
           { _hdOprotectedBindingNames | _hdOprotectedBindingNames `seq` (True) ->
           (case (tl_ _tlOprotectedBindingNames ) of
            { ( _tlInmL,tl_1) | True ->
                (case (hd_ _hdOprotectedBindingNames ) of
                 { ( _hdInm,_hdInmL,hd_1) | True ->
                     (case (_hdInmL ++ _tlInmL) of
                      { _lhsOnmL | _lhsOnmL `seq` (True) ->
                      (case ((let sem_CBindL_Cons_1 :: T_CBindL_1 
                                  sem_CBindL_Cons_1  =
                                      (\ _lhsIgIniq
                                         _lhsIropts ->
                                           (case (_lhsIropts) of
                                            { _tlOropts | _tlOropts `seq` (True) ->
                                            (case (_lhsIropts) of
                                             { _hdOropts | _hdOropts `seq` (True) ->
                                             (case (_lhsIgIniq) of
                                              { _hdOgIniq | _hdOgIniq `seq` (True) ->
                                              (case (hd_1 _hdOgIniq _hdOropts ) of
                                               { ( _hdIgIniq,hd_2) | True ->
                                                   (case (_hdIgIniq) of
                                                    { _tlOgIniq | _tlOgIniq `seq` (True) ->
                                                    (case (tl_1 _tlOgIniq _tlOropts ) of
                                                     { ( _tlIgIniq,tl_2) | True ->
                                                         (case (_tlIgIniq) of
                                                          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                                                          (case ((let sem_CBindL_Cons_2 :: T_CBindL_2 
                                                                      sem_CBindL_Cons_2  =
                                                                          (\ _lhsIaRenMp
                                                                             _lhsIlev
                                                                             _lhsImoduleNm ->
                                                                               (case (_lhsImoduleNm) of
                                                                                { _tlOmoduleNm | _tlOmoduleNm `seq` (True) ->
                                                                                (case (_lhsIlev) of
                                                                                 { _tlOlev | _tlOlev `seq` (True) ->
                                                                                 (case (_lhsIaRenMp) of
                                                                                  { _tlOaRenMp | _tlOaRenMp `seq` (True) ->
                                                                                  (case (_lhsImoduleNm) of
                                                                                   { _hdOmoduleNm | _hdOmoduleNm `seq` (True) ->
                                                                                   (case (_lhsIlev) of
                                                                                    { _hdOlev | _hdOlev `seq` (True) ->
                                                                                    (case (_lhsIaRenMp) of
                                                                                     { _hdOaRenMp | _hdOaRenMp `seq` (True) ->
                                                                                     (case (tl_2 _tlOaRenMp _tlOlev _tlOmoduleNm ) of
                                                                                      { ( _tlIcTrf) | True ->
                                                                                          (case (hd_2 _hdOaRenMp _hdOlev _hdOmoduleNm ) of
                                                                                           { ( _hdIcTrf) | True ->
                                                                                               (case ((:) _hdIcTrf _tlIcTrf) of
                                                                                                { _cTrf | _cTrf `seq` (True) ->
                                                                                                (case (_cTrf) of
                                                                                                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                                                 ( _lhsOcTrf) }) }) }) }) }) }) }) }) }) }))
                                                                  in  sem_CBindL_Cons_2)) of
                                                           { ( sem_CBindL_2) | True ->
                                                           ( _lhsOgIniq,sem_CBindL_2) }) }) }) }) }) }) }) }))
                              in  sem_CBindL_Cons_1)) of
                       { ( sem_CBindL_1) | True ->
                       ( _lhsOnmL,sem_CBindL_1) }) }) }) }) }) }))
sem_CBindL_Nil :: T_CBindL 
sem_CBindL_Nil  =
    (\ _lhsIprotectedBindingNames ->
         (case ([]) of
          { _lhsOnmL | _lhsOnmL `seq` (True) ->
          (case ((let sem_CBindL_Nil_1 :: T_CBindL_1 
                      sem_CBindL_Nil_1  =
                          (\ _lhsIgIniq
                             _lhsIropts ->
                               (case (_lhsIgIniq) of
                                { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                                (case ((let sem_CBindL_Nil_2 :: T_CBindL_2 
                                            sem_CBindL_Nil_2  =
                                                (\ _lhsIaRenMp
                                                   _lhsIlev
                                                   _lhsImoduleNm ->
                                                     (case ([]) of
                                                      { _cTrf | _cTrf `seq` (True) ->
                                                      (case (_cTrf) of
                                                       { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                       ( _lhsOcTrf) }) }))
                                        in  sem_CBindL_Nil_2)) of
                                 { ( sem_CBindL_2) | True ->
                                 ( _lhsOgIniq,sem_CBindL_2) }) }))
                  in  sem_CBindL_Nil_1)) of
           { ( sem_CBindL_1) | True ->
           ( _lhsOnmL,sem_CBindL_1) }) }))
-- CBound ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         protectedBindingNames : [HsName]
         ropts                : RenUniqOpts
      chained attribute:
         gIniq                : Int
   visit 1:
      inherited attributes:
         aRenMp               : ARenMp
         lev                  : Int
         moduleNm             : HsName
         nm                   : HsName
      synthesized attributes:
         cTrf                 : SELF 
         nmL                  : [HsName]
   alternatives:
      alternative Bind:
         child bindMeta       : CMetas 
         child expr           : CExpr 
         visit 1:
            local aRenMp      : _
            local cTrf        : _
      alternative FFE:
         child callconv       : {FFIWay}
         child expEnt         : {ForeignEnt}
         child expr           : CExpr 
         child ty             : {Ty}
         visit 1:
            local aRenMp      : _
            local cTrf        : _
      alternative Meta:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child cmetas         : CMetas 
         visit 1:
            local cTrf        : _
      alternative RelevTy:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child relevTy        : {RelevTy}
         visit 1:
            local cTrf        : _
      alternative Ty:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child ty             : {Ty}
         visit 1:
            local cTrf        : _
      alternative Val:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child expr           : CExpr 
         visit 1:
            local aRenMp      : _
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
type T_CBound  = Int ->
                 ([HsName]) ->
                 RenUniqOpts ->
                 ( Int,T_CBound_1 )
type T_CBound_1  = ARenMp ->
                   Int ->
                   HsName ->
                   HsName ->
                   ( CBound ,([HsName]))
sem_CBound_Bind :: T_CMetas  ->
                   T_CExpr  ->
                   T_CBound 
sem_CBound_Bind bindMeta_ expr_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIropts) of
          { _exprOropts | _exprOropts `seq` (True) ->
          (case (_lhsIgIniq) of
           { _bindMetaOgIniq | _bindMetaOgIniq `seq` (True) ->
           (case (bindMeta_ _bindMetaOgIniq ) of
            { ( _bindMetaIgIniq,_bindMetaIprotectableBindingNames,bindMeta_1) | True ->
                (case (_bindMetaIprotectableBindingNames ++ _lhsIprotectedBindingNames) of
                 { _exprOprotectedBindingNames | _exprOprotectedBindingNames `seq` (True) ->
                 (case (if   renuniqOptResetOnlyInLam _lhsIropts
                        then 1
                        else _lhsIgIniq) of
                  { _exprOgIniq | _exprOgIniq `seq` (True) ->
                  (case (expr_ _exprOgIniq _exprOprotectedBindingNames _exprOropts ) of
                   { ( _exprIgIniq,_exprIisLamBody,expr_1) | True ->
                       (case (if   renuniqOptResetOnlyInLam _lhsIropts
                              then _lhsIgIniq
                              else _exprIgIniq) of
                        { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                        (case ((let sem_CBound_Bind_1 :: T_CBound_1 
                                    sem_CBound_Bind_1  =
                                        (\ _lhsIaRenMp
                                           _lhsIlev
                                           _lhsImoduleNm
                                           _lhsInm ->
                                             (case (_lhsImoduleNm) of
                                              { _exprOmoduleNm | _exprOmoduleNm `seq` (True) ->
                                              (case (_lhsIlev) of
                                               { _exprOlev | _exprOlev `seq` (True) ->
                                               (case (_lhsIaRenMp) of
                                                { _exprOaccumARenMp | _exprOaccumARenMp `seq` (True) ->
                                                (case (expr_1 _exprOaccumARenMp _exprOlev _exprOmoduleNm ) of
                                                 { ( _exprIaccumARenMp,expr_2) | True ->
                                                     (case (_exprIaccumARenMp) of
                                                      { _aRenMp | _aRenMp `seq` (True) ->
                                                      (case (_aRenMp) of
                                                       { _exprOaRenMp | _exprOaRenMp `seq` (True) ->
                                                       (case (expr_2 _exprOaRenMp ) of
                                                        { ( _exprIcTrf) | True ->
                                                            (case (_lhsIropts) of
                                                             { _bindMetaOropts | _bindMetaOropts `seq` (True) ->
                                                             (case (_lhsImoduleNm) of
                                                              { _bindMetaOmoduleNm | _bindMetaOmoduleNm `seq` (True) ->
                                                              (case (_lhsIlev) of
                                                               { _bindMetaOlev | _bindMetaOlev `seq` (True) ->
                                                               (case (_aRenMp) of
                                                                { _bindMetaOaRenMp | _bindMetaOaRenMp `seq` (True) ->
                                                                (case (bindMeta_1 _bindMetaOaRenMp _bindMetaOlev _bindMetaOmoduleNm _bindMetaOropts ) of
                                                                 { ( _bindMetaIcTrf) | True ->
                                                                     (case (CBound_Bind _bindMetaIcTrf _exprIcTrf) of
                                                                      { _cTrf | _cTrf `seq` (True) ->
                                                                      (case (_cTrf) of
                                                                       { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                       (case ([]) of
                                                                        { _lhsOnmL | _lhsOnmL `seq` (True) ->
                                                                        ( _lhsOcTrf,_lhsOnmL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                in  sem_CBound_Bind_1)) of
                         { ( sem_CBound_1) | True ->
                         ( _lhsOgIniq,sem_CBound_1) }) }) }) }) }) }) }) }))
sem_CBound_FFE :: FFIWay ->
                  ForeignEnt ->
                  T_CExpr  ->
                  Ty ->
                  T_CBound 
sem_CBound_FFE callconv_ expEnt_ expr_ ty_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIropts) of
          { _exprOropts | _exprOropts `seq` (True) ->
          (case (_lhsIprotectedBindingNames) of
           { _exprOprotectedBindingNames | _exprOprotectedBindingNames `seq` (True) ->
           (case (_lhsIgIniq) of
            { _exprOgIniq | _exprOgIniq `seq` (True) ->
            (case (expr_ _exprOgIniq _exprOprotectedBindingNames _exprOropts ) of
             { ( _exprIgIniq,_exprIisLamBody,expr_1) | True ->
                 (case (_exprIgIniq) of
                  { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                  (case ((let sem_CBound_FFE_1 :: T_CBound_1 
                              sem_CBound_FFE_1  =
                                  (\ _lhsIaRenMp
                                     _lhsIlev
                                     _lhsImoduleNm
                                     _lhsInm ->
                                       (case (_lhsImoduleNm) of
                                        { _exprOmoduleNm | _exprOmoduleNm `seq` (True) ->
                                        (case (_lhsIlev) of
                                         { _exprOlev | _exprOlev `seq` (True) ->
                                         (case (_lhsIaRenMp) of
                                          { _exprOaccumARenMp | _exprOaccumARenMp `seq` (True) ->
                                          (case (expr_1 _exprOaccumARenMp _exprOlev _exprOmoduleNm ) of
                                           { ( _exprIaccumARenMp,expr_2) | True ->
                                               (case (_exprIaccumARenMp) of
                                                { _aRenMp | _aRenMp `seq` (True) ->
                                                (case (_aRenMp) of
                                                 { _exprOaRenMp | _exprOaRenMp `seq` (True) ->
                                                 (case (expr_2 _exprOaRenMp ) of
                                                  { ( _exprIcTrf) | True ->
                                                      (case (CBound_FFE callconv_ expEnt_ _exprIcTrf ty_) of
                                                       { _cTrf | _cTrf `seq` (True) ->
                                                       (case (_cTrf) of
                                                        { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                        (case ([]) of
                                                         { _lhsOnmL | _lhsOnmL `seq` (True) ->
                                                         ( _lhsOcTrf,_lhsOnmL) }) }) }) }) }) }) }) }) }) }))
                          in  sem_CBound_FFE_1)) of
                   { ( sem_CBound_1) | True ->
                   ( _lhsOgIniq,sem_CBound_1) }) }) }) }) }) }))
sem_CBound_Meta :: ACoreBindAspectKeyS ->
                   T_CMetas  ->
                   T_CBound 
sem_CBound_Meta aspectKeyS_ cmetas_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIgIniq) of
          { _cmetasOgIniq | _cmetasOgIniq `seq` (True) ->
          (case (cmetas_ _cmetasOgIniq ) of
           { ( _cmetasIgIniq,_cmetasIprotectableBindingNames,cmetas_1) | True ->
               (case (_cmetasIgIniq) of
                { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                (case ((let sem_CBound_Meta_1 :: T_CBound_1 
                            sem_CBound_Meta_1  =
                                (\ _lhsIaRenMp
                                   _lhsIlev
                                   _lhsImoduleNm
                                   _lhsInm ->
                                     (case (_lhsIropts) of
                                      { _cmetasOropts | _cmetasOropts `seq` (True) ->
                                      (case (_lhsImoduleNm) of
                                       { _cmetasOmoduleNm | _cmetasOmoduleNm `seq` (True) ->
                                       (case (_lhsIlev) of
                                        { _cmetasOlev | _cmetasOlev `seq` (True) ->
                                        (case (_lhsIaRenMp) of
                                         { _cmetasOaRenMp | _cmetasOaRenMp `seq` (True) ->
                                         (case (cmetas_1 _cmetasOaRenMp _cmetasOlev _cmetasOmoduleNm _cmetasOropts ) of
                                          { ( _cmetasIcTrf) | True ->
                                              (case (CBound_Meta aspectKeyS_ _cmetasIcTrf) of
                                               { _cTrf | _cTrf `seq` (True) ->
                                               (case (_cTrf) of
                                                { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                (case ([]) of
                                                 { _lhsOnmL | _lhsOnmL `seq` (True) ->
                                                 ( _lhsOcTrf,_lhsOnmL) }) }) }) }) }) }) }) }))
                        in  sem_CBound_Meta_1)) of
                 { ( sem_CBound_1) | True ->
                 ( _lhsOgIniq,sem_CBound_1) }) }) }) }))
sem_CBound_RelevTy :: ACoreBindAspectKeyS ->
                      RelevTy ->
                      T_CBound 
sem_CBound_RelevTy aspectKeyS_ relevTy_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case ((let sem_CBound_RelevTy_1 :: T_CBound_1 
                      sem_CBound_RelevTy_1  =
                          (\ _lhsIaRenMp
                             _lhsIlev
                             _lhsImoduleNm
                             _lhsInm ->
                               (case (CBound_RelevTy aspectKeyS_ relevTy_) of
                                { _cTrf | _cTrf `seq` (True) ->
                                (case (_cTrf) of
                                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                 (case ([]) of
                                  { _lhsOnmL | _lhsOnmL `seq` (True) ->
                                  ( _lhsOcTrf,_lhsOnmL) }) }) }))
                  in  sem_CBound_RelevTy_1)) of
           { ( sem_CBound_1) | True ->
           ( _lhsOgIniq,sem_CBound_1) }) }))
sem_CBound_Ty :: ACoreBindAspectKeyS ->
                 Ty ->
                 T_CBound 
sem_CBound_Ty aspectKeyS_ ty_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case ((let sem_CBound_Ty_1 :: T_CBound_1 
                      sem_CBound_Ty_1  =
                          (\ _lhsIaRenMp
                             _lhsIlev
                             _lhsImoduleNm
                             _lhsInm ->
                               (case (CBound_Ty aspectKeyS_ ty_) of
                                { _cTrf | _cTrf `seq` (True) ->
                                (case (_cTrf) of
                                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                 (case ([]) of
                                  { _lhsOnmL | _lhsOnmL `seq` (True) ->
                                  ( _lhsOcTrf,_lhsOnmL) }) }) }))
                  in  sem_CBound_Ty_1)) of
           { ( sem_CBound_1) | True ->
           ( _lhsOgIniq,sem_CBound_1) }) }))
sem_CBound_Val :: ACoreBindAspectKeyS ->
                  T_CExpr  ->
                  T_CBound 
sem_CBound_Val aspectKeyS_ expr_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIropts) of
          { _exprOropts | _exprOropts `seq` (True) ->
          (case (_lhsIprotectedBindingNames) of
           { _exprOprotectedBindingNames | _exprOprotectedBindingNames `seq` (True) ->
           (case (if   renuniqOptResetOnlyInLam _lhsIropts
                  then 1
                  else _lhsIgIniq) of
            { _exprOgIniq | _exprOgIniq `seq` (True) ->
            (case (expr_ _exprOgIniq _exprOprotectedBindingNames _exprOropts ) of
             { ( _exprIgIniq,_exprIisLamBody,expr_1) | True ->
                 (case (if   renuniqOptResetOnlyInLam _lhsIropts
                        then _lhsIgIniq
                        else _exprIgIniq) of
                  { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                  (case ((let sem_CBound_Val_1 :: T_CBound_1 
                              sem_CBound_Val_1  =
                                  (\ _lhsIaRenMp
                                     _lhsIlev
                                     _lhsImoduleNm
                                     _lhsInm ->
                                       (case (_lhsImoduleNm) of
                                        { _exprOmoduleNm | _exprOmoduleNm `seq` (True) ->
                                        (case (_lhsIlev) of
                                         { _exprOlev | _exprOlev `seq` (True) ->
                                         (case (_lhsIaRenMp) of
                                          { _exprOaccumARenMp | _exprOaccumARenMp `seq` (True) ->
                                          (case (expr_1 _exprOaccumARenMp _exprOlev _exprOmoduleNm ) of
                                           { ( _exprIaccumARenMp,expr_2) | True ->
                                               (case (_exprIaccumARenMp) of
                                                { _aRenMp | _aRenMp `seq` (True) ->
                                                (case (_aRenMp) of
                                                 { _exprOaRenMp | _exprOaRenMp `seq` (True) ->
                                                 (case (expr_2 _exprOaRenMp ) of
                                                  { ( _exprIcTrf) | True ->
                                                      (case (CBound_Val aspectKeyS_ _exprIcTrf) of
                                                       { _cTrf | _cTrf `seq` (True) ->
                                                       (case (_cTrf) of
                                                        { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                        (case ([]) of
                                                         { _lhsOnmL | _lhsOnmL `seq` (True) ->
                                                         ( _lhsOcTrf,_lhsOnmL) }) }) }) }) }) }) }) }) }) }))
                          in  sem_CBound_Val_1)) of
                   { ( sem_CBound_1) | True ->
                   ( _lhsOgIniq,sem_CBound_1) }) }) }) }) }) }))
-- CBoundL -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         protectedBindingNames : [HsName]
         ropts                : RenUniqOpts
      chained attribute:
         gIniq                : Int
   visit 1:
      inherited attributes:
         aRenMp               : ARenMp
         lev                  : Int
         moduleNm             : HsName
         nm                   : HsName
      synthesized attributes:
         cTrf                 : SELF 
         nmL                  : [HsName]
   alternatives:
      alternative Cons:
         child hd             : CBound 
         child tl             : CBoundL 
         visit 1:
            local cTrf        : _
      alternative Nil:
         visit 1:
            local cTrf        : _
-}
-- cata
sem_CBoundL :: CBoundL  ->
               T_CBoundL 
sem_CBoundL list  =
    (Prelude.foldr sem_CBoundL_Cons sem_CBoundL_Nil (Prelude.map sem_CBound list) )
-- semantic domain
type T_CBoundL  = Int ->
                  ([HsName]) ->
                  RenUniqOpts ->
                  ( Int,T_CBoundL_1 )
type T_CBoundL_1  = ARenMp ->
                    Int ->
                    HsName ->
                    HsName ->
                    ( CBoundL ,([HsName]))
sem_CBoundL_Cons :: T_CBound  ->
                    T_CBoundL  ->
                    T_CBoundL 
sem_CBoundL_Cons hd_ tl_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIropts) of
          { _tlOropts | _tlOropts `seq` (True) ->
          (case (_lhsIprotectedBindingNames) of
           { _tlOprotectedBindingNames | _tlOprotectedBindingNames `seq` (True) ->
           (case (_lhsIropts) of
            { _hdOropts | _hdOropts `seq` (True) ->
            (case (_lhsIprotectedBindingNames) of
             { _hdOprotectedBindingNames | _hdOprotectedBindingNames `seq` (True) ->
             (case (_lhsIgIniq) of
              { _hdOgIniq | _hdOgIniq `seq` (True) ->
              (case (hd_ _hdOgIniq _hdOprotectedBindingNames _hdOropts ) of
               { ( _hdIgIniq,hd_1) | True ->
                   (case (_hdIgIniq) of
                    { _tlOgIniq | _tlOgIniq `seq` (True) ->
                    (case (tl_ _tlOgIniq _tlOprotectedBindingNames _tlOropts ) of
                     { ( _tlIgIniq,tl_1) | True ->
                         (case (_tlIgIniq) of
                          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                          (case ((let sem_CBoundL_Cons_1 :: T_CBoundL_1 
                                      sem_CBoundL_Cons_1  =
                                          (\ _lhsIaRenMp
                                             _lhsIlev
                                             _lhsImoduleNm
                                             _lhsInm ->
                                               (case (_lhsImoduleNm) of
                                                { _tlOmoduleNm | _tlOmoduleNm `seq` (True) ->
                                                (case (_lhsIlev) of
                                                 { _tlOlev | _tlOlev `seq` (True) ->
                                                 (case (_lhsIaRenMp) of
                                                  { _tlOaRenMp | _tlOaRenMp `seq` (True) ->
                                                  (case (_lhsImoduleNm) of
                                                   { _hdOmoduleNm | _hdOmoduleNm `seq` (True) ->
                                                   (case (_lhsIlev) of
                                                    { _hdOlev | _hdOlev `seq` (True) ->
                                                    (case (_lhsIaRenMp) of
                                                     { _hdOaRenMp | _hdOaRenMp `seq` (True) ->
                                                     (case (_lhsInm) of
                                                      { _tlOnm | _tlOnm `seq` (True) ->
                                                      (case (tl_1 _tlOaRenMp _tlOlev _tlOmoduleNm _tlOnm ) of
                                                       { ( _tlIcTrf,_tlInmL) | True ->
                                                           (case (_lhsInm) of
                                                            { _hdOnm | _hdOnm `seq` (True) ->
                                                            (case (hd_1 _hdOaRenMp _hdOlev _hdOmoduleNm _hdOnm ) of
                                                             { ( _hdIcTrf,_hdInmL) | True ->
                                                                 (case ((:) _hdIcTrf _tlIcTrf) of
                                                                  { _cTrf | _cTrf `seq` (True) ->
                                                                  (case (_cTrf) of
                                                                   { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                   (case (_hdInmL ++ _tlInmL) of
                                                                    { _lhsOnmL | _lhsOnmL `seq` (True) ->
                                                                    ( _lhsOcTrf,_lhsOnmL) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                  in  sem_CBoundL_Cons_1)) of
                           { ( sem_CBoundL_1) | True ->
                           ( _lhsOgIniq,sem_CBoundL_1) }) }) }) }) }) }) }) }) }) }))
sem_CBoundL_Nil :: T_CBoundL 
sem_CBoundL_Nil  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case ((let sem_CBoundL_Nil_1 :: T_CBoundL_1 
                      sem_CBoundL_Nil_1  =
                          (\ _lhsIaRenMp
                             _lhsIlev
                             _lhsImoduleNm
                             _lhsInm ->
                               (case ([]) of
                                { _cTrf | _cTrf `seq` (True) ->
                                (case (_cTrf) of
                                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                 (case ([]) of
                                  { _lhsOnmL | _lhsOnmL `seq` (True) ->
                                  ( _lhsOcTrf,_lhsOnmL) }) }) }))
                  in  sem_CBoundL_Nil_1)) of
           { ( sem_CBoundL_1) | True ->
           ( _lhsOgIniq,sem_CBoundL_1) }) }))
-- CExpr -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         protectedBindingNames : [HsName]
         ropts                : RenUniqOpts
      chained attribute:
         gIniq                : Int
      synthesized attribute:
         isLamBody            : Bool
   visit 1:
      inherited attributes:
         lev                  : Int
         moduleNm             : HsName
      chained attribute:
         accumARenMp          : ARenMp
   visit 2:
      inherited attribute:
         aRenMp               : ARenMp
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Ann:
         child ann            : CExprAnn 
         child expr           : CExpr 
         visit 2:
            local cTrf        : _
      alternative App:
         child func           : CExpr 
         child arg            : CBound 
         visit 2:
            local aRenMp      : _
            local cTrf        : _
      alternative Case:
         child expr           : CExpr 
         child alts           : CAltL 
         child dflt           : CExpr 
         visit 2:
            local accumARenMp : _
            local aRenMp      : _
            local cTrf        : _
      alternative CaseAltFail:
         child failReason     : {CaseAltFailReason}
         child errorExpr      : CExpr 
         visit 2:
            local aRenMp      : _
            local cTrf        : _
      alternative Char:
         child char           : {Char}
         visit 2:
            local cTrf        : _
      alternative CoeArg:
         visit 2:
            local cTrf        : _
      alternative FFI:
         child callconv       : {FFIWay}
         child safety         : {String}
         child impEnt         : {ForeignEnt}
         child ty             : {Ty}
         visit 2:
            local cTrf        : _
      alternative Hole:
         child uid            : {UID}
         visit 2:
            local cTrf        : _
      alternative HoleLet:
         child bindsUid       : {UID}
         child body           : CExpr 
         visit 2:
            local aRenMp      : _
            local cTrf        : _
      alternative ImplsApp:
         child func           : CExpr 
         child uid            : {ImplsVarId}
         visit 2:
            local aRenMp      : _
            local cTrf        : _
      alternative ImplsLam:
         child uid            : {ImplsVarId}
         child body           : CExpr 
         visit 2:
            local aRenMp      : _
            local cTrf        : _
      alternative Int:
         child int            : {Int}
         visit 2:
            local cTrf        : _
      alternative Integer:
         child integer        : {Integer}
         visit 2:
            local cTrf        : _
      alternative Lam:
         child bind           : CBind 
         child body           : CExpr 
         visit 0:
            local _tup2       : _
         visit 1:
            intra _tup2       : _
         visit 2:
            local argNm       : _
            local lIniq       : _
            local aRenMp      : _
            intra _tup2       : _
      alternative Let:
         child categ          : {CBindCateg}
         child binds          : CBindL 
         child body           : CExpr 
         visit 0:
            local _tup3       : _
         visit 1:
            local isGlobal    : _
            local lIniq       : _
            local addToARenMp : _
            intra _tup3       : _
         visit 2:
            local aRenMp      : _
            local cTrf        : _
            intra addToARenMp : _
            intra isGlobal    : _
      alternative String:
         child str            : {String}
         visit 2:
            local cTrf        : _
      alternative Tup:
         child tag            : {CTag}
         visit 2:
            local cTrf        : _
      alternative TupDel:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         visit 2:
            local accumARenMp : _
            local aRenMp      : _
            local cTrf        : _
      alternative TupIns:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 2:
            local accumARenMp : _
            local aRenMp      : _
            local cTrf        : _
      alternative TupUpd:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 2:
            local accumARenMp : _
            local aRenMp      : _
            local cTrf        : _
      alternative Var:
         child ref            : {ACoreBindRef}
         visit 2:
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
type T_CExpr  = Int ->
                ([HsName]) ->
                RenUniqOpts ->
                ( Int,Bool,T_CExpr_1 )
type T_CExpr_1  = ARenMp ->
                  Int ->
                  HsName ->
                  ( ARenMp,T_CExpr_2 )
type T_CExpr_2  = ARenMp ->
                  ( CExpr )
sem_CExpr_Ann :: T_CExprAnn  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Ann ann_ expr_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIropts) of
          { _exprOropts | _exprOropts `seq` (True) ->
          (case (_lhsIprotectedBindingNames) of
           { _exprOprotectedBindingNames | _exprOprotectedBindingNames `seq` (True) ->
           (case (_lhsIgIniq) of
            { _annOgIniq | _annOgIniq `seq` (True) ->
            (case (ann_ _annOgIniq ) of
             { ( _annIgIniq,ann_1) | True ->
                 (case (_annIgIniq) of
                  { _exprOgIniq | _exprOgIniq `seq` (True) ->
                  (case (expr_ _exprOgIniq _exprOprotectedBindingNames _exprOropts ) of
                   { ( _exprIgIniq,_exprIisLamBody,expr_1) | True ->
                       (case (_exprIgIniq) of
                        { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                        (case (_exprIisLamBody) of
                         { _lhsOisLamBody | _lhsOisLamBody `seq` (True) ->
                         (case ((let sem_CExpr_Ann_1 :: T_CExpr_1 
                                     sem_CExpr_Ann_1  =
                                         (\ _lhsIaccumARenMp
                                            _lhsIlev
                                            _lhsImoduleNm ->
                                              (case (_lhsImoduleNm) of
                                               { _exprOmoduleNm | _exprOmoduleNm `seq` (True) ->
                                               (case (_lhsIlev) of
                                                { _exprOlev | _exprOlev `seq` (True) ->
                                                (case (_lhsIaccumARenMp) of
                                                 { _exprOaccumARenMp | _exprOaccumARenMp `seq` (True) ->
                                                 (case (expr_1 _exprOaccumARenMp _exprOlev _exprOmoduleNm ) of
                                                  { ( _exprIaccumARenMp,expr_2) | True ->
                                                      (case (_exprIaccumARenMp) of
                                                       { _lhsOaccumARenMp | _lhsOaccumARenMp `seq` (True) ->
                                                       (case ((let sem_CExpr_Ann_2 :: T_CExpr_2 
                                                                   sem_CExpr_Ann_2  =
                                                                       (\ _lhsIaRenMp ->
                                                                            (case (_lhsIaRenMp) of
                                                                             { _exprOaRenMp | _exprOaRenMp `seq` (True) ->
                                                                             (case (expr_2 _exprOaRenMp ) of
                                                                              { ( _exprIcTrf) | True ->
                                                                                  (case (_lhsIropts) of
                                                                                   { _annOropts | _annOropts `seq` (True) ->
                                                                                   (case (_lhsImoduleNm) of
                                                                                    { _annOmoduleNm | _annOmoduleNm `seq` (True) ->
                                                                                    (case (_lhsIlev) of
                                                                                     { _annOlev | _annOlev `seq` (True) ->
                                                                                     (case (_lhsIaRenMp) of
                                                                                      { _annOaRenMp | _annOaRenMp `seq` (True) ->
                                                                                      (case (ann_1 _annOaRenMp _annOlev _annOmoduleNm _annOropts ) of
                                                                                       { ( _annIcTrf) | True ->
                                                                                           (case (CExpr_Ann _annIcTrf _exprIcTrf) of
                                                                                            { _cTrf | _cTrf `seq` (True) ->
                                                                                            (case (_cTrf) of
                                                                                             { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                                             ( _lhsOcTrf) }) }) }) }) }) }) }) }) }))
                                                               in  sem_CExpr_Ann_2)) of
                                                        { ( sem_CExpr_2) | True ->
                                                        ( _lhsOaccumARenMp,sem_CExpr_2) }) }) }) }) }) }))
                                 in  sem_CExpr_Ann_1)) of
                          { ( sem_CExpr_1) | True ->
                          ( _lhsOgIniq,_lhsOisLamBody,sem_CExpr_1) }) }) }) }) }) }) }) }) }))
sem_CExpr_App :: T_CExpr  ->
                 T_CBound  ->
                 T_CExpr 
sem_CExpr_App func_ arg_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIropts) of
          { _argOropts | _argOropts `seq` (True) ->
          (case (_lhsIprotectedBindingNames) of
           { _argOprotectedBindingNames | _argOprotectedBindingNames `seq` (True) ->
           (case (_lhsIropts) of
            { _funcOropts | _funcOropts `seq` (True) ->
            (case (_lhsIprotectedBindingNames) of
             { _funcOprotectedBindingNames | _funcOprotectedBindingNames `seq` (True) ->
             (case (_lhsIgIniq) of
              { _funcOgIniq | _funcOgIniq `seq` (True) ->
              (case (func_ _funcOgIniq _funcOprotectedBindingNames _funcOropts ) of
               { ( _funcIgIniq,_funcIisLamBody,func_1) | True ->
                   (case (_funcIgIniq) of
                    { _argOgIniq | _argOgIniq `seq` (True) ->
                    (case (arg_ _argOgIniq _argOprotectedBindingNames _argOropts ) of
                     { ( _argIgIniq,arg_1) | True ->
                         (case (_argIgIniq) of
                          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                          (case (True) of
                           { _lhsOisLamBody | _lhsOisLamBody `seq` (True) ->
                           (case ((let sem_CExpr_App_1 :: T_CExpr_1 
                                       sem_CExpr_App_1  =
                                           (\ _lhsIaccumARenMp
                                              _lhsIlev
                                              _lhsImoduleNm ->
                                                (case (_lhsIaccumARenMp) of
                                                 { _lhsOaccumARenMp | _lhsOaccumARenMp `seq` (True) ->
                                                 (case ((let sem_CExpr_App_2 :: T_CExpr_2 
                                                             sem_CExpr_App_2  =
                                                                 (\ _lhsIaRenMp ->
                                                                      (case (_lhsImoduleNm) of
                                                                       { _argOmoduleNm | _argOmoduleNm `seq` (True) ->
                                                                       (case (_lhsIlev) of
                                                                        { _argOlev | _argOlev `seq` (True) ->
                                                                        (case (_lhsImoduleNm) of
                                                                         { _funcOmoduleNm | _funcOmoduleNm `seq` (True) ->
                                                                         (case (_lhsIlev) of
                                                                          { _funcOlev | _funcOlev `seq` (True) ->
                                                                          (case (_lhsIaRenMp) of
                                                                           { _funcOaccumARenMp | _funcOaccumARenMp `seq` (True) ->
                                                                           (case (func_1 _funcOaccumARenMp _funcOlev _funcOmoduleNm ) of
                                                                            { ( _funcIaccumARenMp,func_2) | True ->
                                                                                (case (_funcIaccumARenMp) of
                                                                                 { _aRenMp | _aRenMp `seq` (True) ->
                                                                                 (case (_aRenMp) of
                                                                                  { _argOaRenMp | _argOaRenMp `seq` (True) ->
                                                                                  (case (_aRenMp) of
                                                                                   { _funcOaRenMp | _funcOaRenMp `seq` (True) ->
                                                                                   (case (hsnUnknown) of
                                                                                    { _argOnm | _argOnm `seq` (True) ->
                                                                                    (case (arg_1 _argOaRenMp _argOlev _argOmoduleNm _argOnm ) of
                                                                                     { ( _argIcTrf,_argInmL) | True ->
                                                                                         (case (func_2 _funcOaRenMp ) of
                                                                                          { ( _funcIcTrf) | True ->
                                                                                              (case (CExpr_App _funcIcTrf _argIcTrf) of
                                                                                               { _cTrf | _cTrf `seq` (True) ->
                                                                                               (case (_cTrf) of
                                                                                                { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                                                ( _lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                                         in  sem_CExpr_App_2)) of
                                                  { ( sem_CExpr_2) | True ->
                                                  ( _lhsOaccumARenMp,sem_CExpr_2) }) }))
                                   in  sem_CExpr_App_1)) of
                            { ( sem_CExpr_1) | True ->
                            ( _lhsOgIniq,_lhsOisLamBody,sem_CExpr_1) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_Case :: T_CExpr  ->
                  T_CAltL  ->
                  T_CExpr  ->
                  T_CExpr 
sem_CExpr_Case expr_ alts_ dflt_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIropts) of
          { _dfltOropts | _dfltOropts `seq` (True) ->
          (case (_lhsIprotectedBindingNames) of
           { _dfltOprotectedBindingNames | _dfltOprotectedBindingNames `seq` (True) ->
           (case (_lhsIropts) of
            { _altsOropts | _altsOropts `seq` (True) ->
            (case (_lhsIprotectedBindingNames) of
             { _altsOprotectedBindingNames | _altsOprotectedBindingNames `seq` (True) ->
             (case (_lhsIropts) of
              { _exprOropts | _exprOropts `seq` (True) ->
              (case (_lhsIprotectedBindingNames) of
               { _exprOprotectedBindingNames | _exprOprotectedBindingNames `seq` (True) ->
               (case (_lhsIgIniq) of
                { _exprOgIniq | _exprOgIniq `seq` (True) ->
                (case (expr_ _exprOgIniq _exprOprotectedBindingNames _exprOropts ) of
                 { ( _exprIgIniq,_exprIisLamBody,expr_1) | True ->
                     (case (_exprIgIniq) of
                      { _altsOgIniq | _altsOgIniq `seq` (True) ->
                      (case (alts_ _altsOgIniq _altsOprotectedBindingNames _altsOropts ) of
                       { ( _altsIgIniq,alts_1) | True ->
                           (case (_altsIgIniq) of
                            { _dfltOgIniq | _dfltOgIniq `seq` (True) ->
                            (case (dflt_ _dfltOgIniq _dfltOprotectedBindingNames _dfltOropts ) of
                             { ( _dfltIgIniq,_dfltIisLamBody,dflt_1) | True ->
                                 (case (_dfltIgIniq) of
                                  { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                                  (case (True) of
                                   { _lhsOisLamBody | _lhsOisLamBody `seq` (True) ->
                                   (case ((let sem_CExpr_Case_1 :: T_CExpr_1 
                                               sem_CExpr_Case_1  =
                                                   (\ _lhsIaccumARenMp
                                                      _lhsIlev
                                                      _lhsImoduleNm ->
                                                        (case (_lhsIaccumARenMp) of
                                                         { _lhsOaccumARenMp | _lhsOaccumARenMp `seq` (True) ->
                                                         (case ((let sem_CExpr_Case_2 :: T_CExpr_2 
                                                                     sem_CExpr_Case_2  =
                                                                         (\ _lhsIaRenMp ->
                                                                              (case (_lhsImoduleNm) of
                                                                               { _dfltOmoduleNm | _dfltOmoduleNm `seq` (True) ->
                                                                               (case (_lhsIlev) of
                                                                                { _dfltOlev | _dfltOlev `seq` (True) ->
                                                                                (case (_lhsIaRenMp) of
                                                                                 { _accumARenMp | _accumARenMp `seq` (True) ->
                                                                                 (case (_accumARenMp) of
                                                                                  { _dfltOaccumARenMp | _dfltOaccumARenMp `seq` (True) ->
                                                                                  (case (dflt_1 _dfltOaccumARenMp _dfltOlev _dfltOmoduleNm ) of
                                                                                   { ( _dfltIaccumARenMp,dflt_2) | True ->
                                                                                       (case (_dfltIaccumARenMp) of
                                                                                        { _aRenMp | _aRenMp `seq` (True) ->
                                                                                        (case (_aRenMp) of
                                                                                         { _dfltOaRenMp | _dfltOaRenMp `seq` (True) ->
                                                                                         (case (_lhsImoduleNm) of
                                                                                          { _altsOmoduleNm | _altsOmoduleNm `seq` (True) ->
                                                                                          (case (_lhsIlev) of
                                                                                           { _altsOlev | _altsOlev `seq` (True) ->
                                                                                           (case (_aRenMp) of
                                                                                            { _altsOaRenMp | _altsOaRenMp `seq` (True) ->
                                                                                            (case (_lhsImoduleNm) of
                                                                                             { _exprOmoduleNm | _exprOmoduleNm `seq` (True) ->
                                                                                             (case (_lhsIlev) of
                                                                                              { _exprOlev | _exprOlev `seq` (True) ->
                                                                                              (case (_aRenMp) of
                                                                                               { _exprOaRenMp | _exprOaRenMp `seq` (True) ->
                                                                                               (case (_lhsIaRenMp) of
                                                                                                { _exprOaccumARenMp | _exprOaccumARenMp `seq` (True) ->
                                                                                                (case (dflt_2 _dfltOaRenMp ) of
                                                                                                 { ( _dfltIcTrf) | True ->
                                                                                                     (case (alts_1 _altsOaRenMp _altsOlev _altsOmoduleNm ) of
                                                                                                      { ( _altsIcTrf) | True ->
                                                                                                          (case (expr_1 _exprOaccumARenMp _exprOlev _exprOmoduleNm ) of
                                                                                                           { ( _exprIaccumARenMp,expr_2) | True ->
                                                                                                               (case (expr_2 _exprOaRenMp ) of
                                                                                                                { ( _exprIcTrf) | True ->
                                                                                                                    (case (CExpr_Case _exprIcTrf _altsIcTrf _dfltIcTrf) of
                                                                                                                     { _cTrf | _cTrf `seq` (True) ->
                                                                                                                     (case (_cTrf) of
                                                                                                                      { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                                                                      ( _lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                                                 in  sem_CExpr_Case_2)) of
                                                          { ( sem_CExpr_2) | True ->
                                                          ( _lhsOaccumARenMp,sem_CExpr_2) }) }))
                                           in  sem_CExpr_Case_1)) of
                                    { ( sem_CExpr_1) | True ->
                                    ( _lhsOgIniq,_lhsOisLamBody,sem_CExpr_1) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_CaseAltFail :: CaseAltFailReason ->
                         T_CExpr  ->
                         T_CExpr 
sem_CExpr_CaseAltFail failReason_ errorExpr_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIropts) of
          { _errorExprOropts | _errorExprOropts `seq` (True) ->
          (case (_lhsIprotectedBindingNames) of
           { _errorExprOprotectedBindingNames | _errorExprOprotectedBindingNames `seq` (True) ->
           (case (_lhsIgIniq) of
            { _errorExprOgIniq | _errorExprOgIniq `seq` (True) ->
            (case (errorExpr_ _errorExprOgIniq _errorExprOprotectedBindingNames _errorExprOropts ) of
             { ( _errorExprIgIniq,_errorExprIisLamBody,errorExpr_1) | True ->
                 (case (_errorExprIgIniq) of
                  { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                  (case (True) of
                   { _lhsOisLamBody | _lhsOisLamBody `seq` (True) ->
                   (case ((let sem_CExpr_CaseAltFail_1 :: T_CExpr_1 
                               sem_CExpr_CaseAltFail_1  =
                                   (\ _lhsIaccumARenMp
                                      _lhsIlev
                                      _lhsImoduleNm ->
                                        (case (_lhsIaccumARenMp) of
                                         { _lhsOaccumARenMp | _lhsOaccumARenMp `seq` (True) ->
                                         (case ((let sem_CExpr_CaseAltFail_2 :: T_CExpr_2 
                                                     sem_CExpr_CaseAltFail_2  =
                                                         (\ _lhsIaRenMp ->
                                                              (case (_lhsImoduleNm) of
                                                               { _errorExprOmoduleNm | _errorExprOmoduleNm `seq` (True) ->
                                                               (case (_lhsIlev) of
                                                                { _errorExprOlev | _errorExprOlev `seq` (True) ->
                                                                (case (_lhsIaRenMp) of
                                                                 { _errorExprOaccumARenMp | _errorExprOaccumARenMp `seq` (True) ->
                                                                 (case (errorExpr_1 _errorExprOaccumARenMp _errorExprOlev _errorExprOmoduleNm ) of
                                                                  { ( _errorExprIaccumARenMp,errorExpr_2) | True ->
                                                                      (case (_errorExprIaccumARenMp) of
                                                                       { _aRenMp | _aRenMp `seq` (True) ->
                                                                       (case (_aRenMp) of
                                                                        { _errorExprOaRenMp | _errorExprOaRenMp `seq` (True) ->
                                                                        (case (errorExpr_2 _errorExprOaRenMp ) of
                                                                         { ( _errorExprIcTrf) | True ->
                                                                             (case (CExpr_CaseAltFail failReason_ _errorExprIcTrf) of
                                                                              { _cTrf | _cTrf `seq` (True) ->
                                                                              (case (_cTrf) of
                                                                               { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                               ( _lhsOcTrf) }) }) }) }) }) }) }) }) }))
                                                 in  sem_CExpr_CaseAltFail_2)) of
                                          { ( sem_CExpr_2) | True ->
                                          ( _lhsOaccumARenMp,sem_CExpr_2) }) }))
                           in  sem_CExpr_CaseAltFail_1)) of
                    { ( sem_CExpr_1) | True ->
                    ( _lhsOgIniq,_lhsOisLamBody,sem_CExpr_1) }) }) }) }) }) }) }))
sem_CExpr_Char :: Char ->
                  T_CExpr 
sem_CExpr_Char char_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case (True) of
           { _lhsOisLamBody | _lhsOisLamBody `seq` (True) ->
           (case ((let sem_CExpr_Char_1 :: T_CExpr_1 
                       sem_CExpr_Char_1  =
                           (\ _lhsIaccumARenMp
                              _lhsIlev
                              _lhsImoduleNm ->
                                (case (_lhsIaccumARenMp) of
                                 { _lhsOaccumARenMp | _lhsOaccumARenMp `seq` (True) ->
                                 (case ((let sem_CExpr_Char_2 :: T_CExpr_2 
                                             sem_CExpr_Char_2  =
                                                 (\ _lhsIaRenMp ->
                                                      (case (CExpr_Char char_) of
                                                       { _cTrf | _cTrf `seq` (True) ->
                                                       (case (_cTrf) of
                                                        { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                        ( _lhsOcTrf) }) }))
                                         in  sem_CExpr_Char_2)) of
                                  { ( sem_CExpr_2) | True ->
                                  ( _lhsOaccumARenMp,sem_CExpr_2) }) }))
                   in  sem_CExpr_Char_1)) of
            { ( sem_CExpr_1) | True ->
            ( _lhsOgIniq,_lhsOisLamBody,sem_CExpr_1) }) }) }))
sem_CExpr_CoeArg :: T_CExpr 
sem_CExpr_CoeArg  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case (True) of
           { _lhsOisLamBody | _lhsOisLamBody `seq` (True) ->
           (case ((let sem_CExpr_CoeArg_1 :: T_CExpr_1 
                       sem_CExpr_CoeArg_1  =
                           (\ _lhsIaccumARenMp
                              _lhsIlev
                              _lhsImoduleNm ->
                                (case (_lhsIaccumARenMp) of
                                 { _lhsOaccumARenMp | _lhsOaccumARenMp `seq` (True) ->
                                 (case ((let sem_CExpr_CoeArg_2 :: T_CExpr_2 
                                             sem_CExpr_CoeArg_2  =
                                                 (\ _lhsIaRenMp ->
                                                      (case (CExpr_CoeArg) of
                                                       { _cTrf | _cTrf `seq` (True) ->
                                                       (case (_cTrf) of
                                                        { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                        ( _lhsOcTrf) }) }))
                                         in  sem_CExpr_CoeArg_2)) of
                                  { ( sem_CExpr_2) | True ->
                                  ( _lhsOaccumARenMp,sem_CExpr_2) }) }))
                   in  sem_CExpr_CoeArg_1)) of
            { ( sem_CExpr_1) | True ->
            ( _lhsOgIniq,_lhsOisLamBody,sem_CExpr_1) }) }) }))
sem_CExpr_FFI :: FFIWay ->
                 String ->
                 ForeignEnt ->
                 Ty ->
                 T_CExpr 
sem_CExpr_FFI callconv_ safety_ impEnt_ ty_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case (True) of
           { _lhsOisLamBody | _lhsOisLamBody `seq` (True) ->
           (case ((let sem_CExpr_FFI_1 :: T_CExpr_1 
                       sem_CExpr_FFI_1  =
                           (\ _lhsIaccumARenMp
                              _lhsIlev
                              _lhsImoduleNm ->
                                (case (_lhsIaccumARenMp) of
                                 { _lhsOaccumARenMp | _lhsOaccumARenMp `seq` (True) ->
                                 (case ((let sem_CExpr_FFI_2 :: T_CExpr_2 
                                             sem_CExpr_FFI_2  =
                                                 (\ _lhsIaRenMp ->
                                                      (case (CExpr_FFI callconv_ safety_ impEnt_ ty_) of
                                                       { _cTrf | _cTrf `seq` (True) ->
                                                       (case (_cTrf) of
                                                        { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                        ( _lhsOcTrf) }) }))
                                         in  sem_CExpr_FFI_2)) of
                                  { ( sem_CExpr_2) | True ->
                                  ( _lhsOaccumARenMp,sem_CExpr_2) }) }))
                   in  sem_CExpr_FFI_1)) of
            { ( sem_CExpr_1) | True ->
            ( _lhsOgIniq,_lhsOisLamBody,sem_CExpr_1) }) }) }))
sem_CExpr_Hole :: UID ->
                  T_CExpr 
sem_CExpr_Hole uid_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case (True) of
           { _lhsOisLamBody | _lhsOisLamBody `seq` (True) ->
           (case ((let sem_CExpr_Hole_1 :: T_CExpr_1 
                       sem_CExpr_Hole_1  =
                           (\ _lhsIaccumARenMp
                              _lhsIlev
                              _lhsImoduleNm ->
                                (case (_lhsIaccumARenMp) of
                                 { _lhsOaccumARenMp | _lhsOaccumARenMp `seq` (True) ->
                                 (case ((let sem_CExpr_Hole_2 :: T_CExpr_2 
                                             sem_CExpr_Hole_2  =
                                                 (\ _lhsIaRenMp ->
                                                      (case (CExpr_Hole uid_) of
                                                       { _cTrf | _cTrf `seq` (True) ->
                                                       (case (_cTrf) of
                                                        { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                        ( _lhsOcTrf) }) }))
                                         in  sem_CExpr_Hole_2)) of
                                  { ( sem_CExpr_2) | True ->
                                  ( _lhsOaccumARenMp,sem_CExpr_2) }) }))
                   in  sem_CExpr_Hole_1)) of
            { ( sem_CExpr_1) | True ->
            ( _lhsOgIniq,_lhsOisLamBody,sem_CExpr_1) }) }) }))
sem_CExpr_HoleLet :: UID ->
                     T_CExpr  ->
                     T_CExpr 
sem_CExpr_HoleLet bindsUid_ body_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIropts) of
          { _bodyOropts | _bodyOropts `seq` (True) ->
          (case (_lhsIprotectedBindingNames) of
           { _bodyOprotectedBindingNames | _bodyOprotectedBindingNames `seq` (True) ->
           (case (_lhsIgIniq) of
            { _bodyOgIniq | _bodyOgIniq `seq` (True) ->
            (case (body_ _bodyOgIniq _bodyOprotectedBindingNames _bodyOropts ) of
             { ( _bodyIgIniq,_bodyIisLamBody,body_1) | True ->
                 (case (_bodyIgIniq) of
                  { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                  (case (True) of
                   { _lhsOisLamBody | _lhsOisLamBody `seq` (True) ->
                   (case ((let sem_CExpr_HoleLet_1 :: T_CExpr_1 
                               sem_CExpr_HoleLet_1  =
                                   (\ _lhsIaccumARenMp
                                      _lhsIlev
                                      _lhsImoduleNm ->
                                        (case (_lhsIaccumARenMp) of
                                         { _lhsOaccumARenMp | _lhsOaccumARenMp `seq` (True) ->
                                         (case ((let sem_CExpr_HoleLet_2 :: T_CExpr_2 
                                                     sem_CExpr_HoleLet_2  =
                                                         (\ _lhsIaRenMp ->
                                                              (case (_lhsImoduleNm) of
                                                               { _bodyOmoduleNm | _bodyOmoduleNm `seq` (True) ->
                                                               (case (_lhsIlev) of
                                                                { _bodyOlev | _bodyOlev `seq` (True) ->
                                                                (case (_lhsIaRenMp) of
                                                                 { _bodyOaccumARenMp | _bodyOaccumARenMp `seq` (True) ->
                                                                 (case (body_1 _bodyOaccumARenMp _bodyOlev _bodyOmoduleNm ) of
                                                                  { ( _bodyIaccumARenMp,body_2) | True ->
                                                                      (case (_bodyIaccumARenMp) of
                                                                       { _aRenMp | _aRenMp `seq` (True) ->
                                                                       (case (_aRenMp) of
                                                                        { _bodyOaRenMp | _bodyOaRenMp `seq` (True) ->
                                                                        (case (body_2 _bodyOaRenMp ) of
                                                                         { ( _bodyIcTrf) | True ->
                                                                             (case (CExpr_HoleLet bindsUid_ _bodyIcTrf) of
                                                                              { _cTrf | _cTrf `seq` (True) ->
                                                                              (case (_cTrf) of
                                                                               { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                               ( _lhsOcTrf) }) }) }) }) }) }) }) }) }))
                                                 in  sem_CExpr_HoleLet_2)) of
                                          { ( sem_CExpr_2) | True ->
                                          ( _lhsOaccumARenMp,sem_CExpr_2) }) }))
                           in  sem_CExpr_HoleLet_1)) of
                    { ( sem_CExpr_1) | True ->
                    ( _lhsOgIniq,_lhsOisLamBody,sem_CExpr_1) }) }) }) }) }) }) }))
sem_CExpr_ImplsApp :: T_CExpr  ->
                      ImplsVarId ->
                      T_CExpr 
sem_CExpr_ImplsApp func_ uid_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIropts) of
          { _funcOropts | _funcOropts `seq` (True) ->
          (case (_lhsIprotectedBindingNames) of
           { _funcOprotectedBindingNames | _funcOprotectedBindingNames `seq` (True) ->
           (case (_lhsIgIniq) of
            { _funcOgIniq | _funcOgIniq `seq` (True) ->
            (case (func_ _funcOgIniq _funcOprotectedBindingNames _funcOropts ) of
             { ( _funcIgIniq,_funcIisLamBody,func_1) | True ->
                 (case (_funcIgIniq) of
                  { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                  (case (True) of
                   { _lhsOisLamBody | _lhsOisLamBody `seq` (True) ->
                   (case ((let sem_CExpr_ImplsApp_1 :: T_CExpr_1 
                               sem_CExpr_ImplsApp_1  =
                                   (\ _lhsIaccumARenMp
                                      _lhsIlev
                                      _lhsImoduleNm ->
                                        (case (_lhsIaccumARenMp) of
                                         { _lhsOaccumARenMp | _lhsOaccumARenMp `seq` (True) ->
                                         (case ((let sem_CExpr_ImplsApp_2 :: T_CExpr_2 
                                                     sem_CExpr_ImplsApp_2  =
                                                         (\ _lhsIaRenMp ->
                                                              (case (_lhsImoduleNm) of
                                                               { _funcOmoduleNm | _funcOmoduleNm `seq` (True) ->
                                                               (case (_lhsIlev) of
                                                                { _funcOlev | _funcOlev `seq` (True) ->
                                                                (case (_lhsIaRenMp) of
                                                                 { _funcOaccumARenMp | _funcOaccumARenMp `seq` (True) ->
                                                                 (case (func_1 _funcOaccumARenMp _funcOlev _funcOmoduleNm ) of
                                                                  { ( _funcIaccumARenMp,func_2) | True ->
                                                                      (case (_funcIaccumARenMp) of
                                                                       { _aRenMp | _aRenMp `seq` (True) ->
                                                                       (case (_aRenMp) of
                                                                        { _funcOaRenMp | _funcOaRenMp `seq` (True) ->
                                                                        (case (func_2 _funcOaRenMp ) of
                                                                         { ( _funcIcTrf) | True ->
                                                                             (case (CExpr_ImplsApp _funcIcTrf uid_) of
                                                                              { _cTrf | _cTrf `seq` (True) ->
                                                                              (case (_cTrf) of
                                                                               { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                               ( _lhsOcTrf) }) }) }) }) }) }) }) }) }))
                                                 in  sem_CExpr_ImplsApp_2)) of
                                          { ( sem_CExpr_2) | True ->
                                          ( _lhsOaccumARenMp,sem_CExpr_2) }) }))
                           in  sem_CExpr_ImplsApp_1)) of
                    { ( sem_CExpr_1) | True ->
                    ( _lhsOgIniq,_lhsOisLamBody,sem_CExpr_1) }) }) }) }) }) }) }))
sem_CExpr_ImplsLam :: ImplsVarId ->
                      T_CExpr  ->
                      T_CExpr 
sem_CExpr_ImplsLam uid_ body_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIropts) of
          { _bodyOropts | _bodyOropts `seq` (True) ->
          (case (_lhsIprotectedBindingNames) of
           { _bodyOprotectedBindingNames | _bodyOprotectedBindingNames `seq` (True) ->
           (case (_lhsIgIniq) of
            { _bodyOgIniq | _bodyOgIniq `seq` (True) ->
            (case (body_ _bodyOgIniq _bodyOprotectedBindingNames _bodyOropts ) of
             { ( _bodyIgIniq,_bodyIisLamBody,body_1) | True ->
                 (case (_bodyIgIniq) of
                  { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                  (case (True) of
                   { _lhsOisLamBody | _lhsOisLamBody `seq` (True) ->
                   (case ((let sem_CExpr_ImplsLam_1 :: T_CExpr_1 
                               sem_CExpr_ImplsLam_1  =
                                   (\ _lhsIaccumARenMp
                                      _lhsIlev
                                      _lhsImoduleNm ->
                                        (case (_lhsIaccumARenMp) of
                                         { _lhsOaccumARenMp | _lhsOaccumARenMp `seq` (True) ->
                                         (case ((let sem_CExpr_ImplsLam_2 :: T_CExpr_2 
                                                     sem_CExpr_ImplsLam_2  =
                                                         (\ _lhsIaRenMp ->
                                                              (case (_lhsImoduleNm) of
                                                               { _bodyOmoduleNm | _bodyOmoduleNm `seq` (True) ->
                                                               (case (_lhsIlev) of
                                                                { _bodyOlev | _bodyOlev `seq` (True) ->
                                                                (case (_lhsIaRenMp) of
                                                                 { _bodyOaccumARenMp | _bodyOaccumARenMp `seq` (True) ->
                                                                 (case (body_1 _bodyOaccumARenMp _bodyOlev _bodyOmoduleNm ) of
                                                                  { ( _bodyIaccumARenMp,body_2) | True ->
                                                                      (case (_bodyIaccumARenMp) of
                                                                       { _aRenMp | _aRenMp `seq` (True) ->
                                                                       (case (_aRenMp) of
                                                                        { _bodyOaRenMp | _bodyOaRenMp `seq` (True) ->
                                                                        (case (body_2 _bodyOaRenMp ) of
                                                                         { ( _bodyIcTrf) | True ->
                                                                             (case (CExpr_ImplsLam uid_ _bodyIcTrf) of
                                                                              { _cTrf | _cTrf `seq` (True) ->
                                                                              (case (_cTrf) of
                                                                               { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                               ( _lhsOcTrf) }) }) }) }) }) }) }) }) }))
                                                 in  sem_CExpr_ImplsLam_2)) of
                                          { ( sem_CExpr_2) | True ->
                                          ( _lhsOaccumARenMp,sem_CExpr_2) }) }))
                           in  sem_CExpr_ImplsLam_1)) of
                    { ( sem_CExpr_1) | True ->
                    ( _lhsOgIniq,_lhsOisLamBody,sem_CExpr_1) }) }) }) }) }) }) }))
sem_CExpr_Int :: Int ->
                 T_CExpr 
sem_CExpr_Int int_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case (True) of
           { _lhsOisLamBody | _lhsOisLamBody `seq` (True) ->
           (case ((let sem_CExpr_Int_1 :: T_CExpr_1 
                       sem_CExpr_Int_1  =
                           (\ _lhsIaccumARenMp
                              _lhsIlev
                              _lhsImoduleNm ->
                                (case (_lhsIaccumARenMp) of
                                 { _lhsOaccumARenMp | _lhsOaccumARenMp `seq` (True) ->
                                 (case ((let sem_CExpr_Int_2 :: T_CExpr_2 
                                             sem_CExpr_Int_2  =
                                                 (\ _lhsIaRenMp ->
                                                      (case (CExpr_Int int_) of
                                                       { _cTrf | _cTrf `seq` (True) ->
                                                       (case (_cTrf) of
                                                        { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                        ( _lhsOcTrf) }) }))
                                         in  sem_CExpr_Int_2)) of
                                  { ( sem_CExpr_2) | True ->
                                  ( _lhsOaccumARenMp,sem_CExpr_2) }) }))
                   in  sem_CExpr_Int_1)) of
            { ( sem_CExpr_1) | True ->
            ( _lhsOgIniq,_lhsOisLamBody,sem_CExpr_1) }) }) }))
sem_CExpr_Integer :: Integer ->
                     T_CExpr 
sem_CExpr_Integer integer_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case (True) of
           { _lhsOisLamBody | _lhsOisLamBody `seq` (True) ->
           (case ((let sem_CExpr_Integer_1 :: T_CExpr_1 
                       sem_CExpr_Integer_1  =
                           (\ _lhsIaccumARenMp
                              _lhsIlev
                              _lhsImoduleNm ->
                                (case (_lhsIaccumARenMp) of
                                 { _lhsOaccumARenMp | _lhsOaccumARenMp `seq` (True) ->
                                 (case ((let sem_CExpr_Integer_2 :: T_CExpr_2 
                                             sem_CExpr_Integer_2  =
                                                 (\ _lhsIaRenMp ->
                                                      (case (CExpr_Integer integer_) of
                                                       { _cTrf | _cTrf `seq` (True) ->
                                                       (case (_cTrf) of
                                                        { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                        ( _lhsOcTrf) }) }))
                                         in  sem_CExpr_Integer_2)) of
                                  { ( sem_CExpr_2) | True ->
                                  ( _lhsOaccumARenMp,sem_CExpr_2) }) }))
                   in  sem_CExpr_Integer_1)) of
            { ( sem_CExpr_1) | True ->
            ( _lhsOgIniq,_lhsOisLamBody,sem_CExpr_1) }) }) }))
sem_CExpr_Lam :: T_CBind  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Lam bind_ body_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIropts) of
          { _bodyOropts | _bodyOropts `seq` (True) ->
          (case (_lhsIprotectedBindingNames) of
           { _bodyOprotectedBindingNames | _bodyOprotectedBindingNames `seq` (True) ->
           (case (newIniq 1 _lhsIgIniq) of
            { __tup2 | __tup2 `seq` (True) ->
            (case (__tup2) of
             { (_bodyOgIniq,_) | _bodyOgIniq `seq` (True) ->
             (case (body_ _bodyOgIniq _bodyOprotectedBindingNames _bodyOropts ) of
              { ( _bodyIgIniq,_bodyIisLamBody,body_1) | True ->
                  (case (_bodyIgIniq) of
                   { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                   (case (False) of
                    { _lhsOisLamBody | _lhsOisLamBody `seq` (True) ->
                    (case ((let sem_CExpr_Lam_1 :: T_CExpr_1 
                                sem_CExpr_Lam_1  =
                                    (\ _lhsIaccumARenMp
                                       _lhsIlev
                                       _lhsImoduleNm ->
                                         (case (_lhsIaccumARenMp) of
                                          { _lhsOaccumARenMp | _lhsOaccumARenMp `seq` (True) ->
                                          (case ((let sem_CExpr_Lam_2 :: T_CExpr_2 
                                                      sem_CExpr_Lam_2  =
                                                          (\ _lhsIaRenMp ->
                                                               (case (_lhsImoduleNm) of
                                                                { _bodyOmoduleNm | _bodyOmoduleNm `seq` (True) ->
                                                                (case (_lhsIprotectedBindingNames) of
                                                                 { _bindOprotectedBindingNames | _bindOprotectedBindingNames `seq` (True) ->
                                                                 (case (bind_ _bindOprotectedBindingNames ) of
                                                                  { ( _bindInm,_bindInmL,bind_1) | True ->
                                                                      (case (_bindInm) of
                                                                       { _argNm | _argNm `seq` (True) ->
                                                                       (case (if _bodyIisLamBody then _lhsIlev + 1 else _lhsIlev) of
                                                                        { _bodyOlev | _bodyOlev `seq` (True) ->
                                                                        (case (__tup2) of
                                                                         { (_,_lIniq) | _lIniq `seq` (True) ->
                                                                         (case (aRenAdd _lhsIropts False
                                                                                        _lhsImoduleNm
                                                                                        [_argNm] _lIniq _lhsIaRenMp) of
                                                                          { _bodyOaccumARenMp | _bodyOaccumARenMp `seq` (True) ->
                                                                          (case (body_1 _bodyOaccumARenMp _bodyOlev _bodyOmoduleNm ) of
                                                                           { ( _bodyIaccumARenMp,body_2) | True ->
                                                                               (case (_bodyIaccumARenMp) of
                                                                                { _aRenMp | _aRenMp `seq` (True) ->
                                                                                (case (_aRenMp) of
                                                                                 { _bodyOaRenMp | _bodyOaRenMp `seq` (True) ->
                                                                                 (case (body_2 _bodyOaRenMp ) of
                                                                                  { ( _bodyIcTrf) | True ->
                                                                                      (case (acoreLam1 (aRenRepl _aRenMp _argNm) _bodyIcTrf) of
                                                                                       { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                                       ( _lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }))
                                                  in  sem_CExpr_Lam_2)) of
                                           { ( sem_CExpr_2) | True ->
                                           ( _lhsOaccumARenMp,sem_CExpr_2) }) }))
                            in  sem_CExpr_Lam_1)) of
                     { ( sem_CExpr_1) | True ->
                     ( _lhsOgIniq,_lhsOisLamBody,sem_CExpr_1) }) }) }) }) }) }) }) }))
sem_CExpr_Let :: CBindCateg ->
                 T_CBindL  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Let categ_ binds_ body_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIropts) of
          { _bodyOropts | _bodyOropts `seq` (True) ->
          (case (_lhsIprotectedBindingNames) of
           { _bodyOprotectedBindingNames | _bodyOprotectedBindingNames `seq` (True) ->
           (case (_lhsIropts) of
            { _bindsOropts | _bindsOropts `seq` (True) ->
            (case (_lhsIprotectedBindingNames) of
             { _bindsOprotectedBindingNames | _bindsOprotectedBindingNames `seq` (True) ->
             (case (binds_ _bindsOprotectedBindingNames ) of
              { ( _bindsInmL,binds_1) | True ->
                  (case (newIniq (length _bindsInmL) _lhsIgIniq) of
                   { __tup3 | __tup3 `seq` (True) ->
                   (case (__tup3) of
                    { (_bindsOgIniq,_) | _bindsOgIniq `seq` (True) ->
                    (case (binds_1 _bindsOgIniq _bindsOropts ) of
                     { ( _bindsIgIniq,binds_2) | True ->
                         (case (_bindsIgIniq) of
                          { _bodyOgIniq | _bodyOgIniq `seq` (True) ->
                          (case (body_ _bodyOgIniq _bodyOprotectedBindingNames _bodyOropts ) of
                           { ( _bodyIgIniq,_bodyIisLamBody,body_1) | True ->
                               (case (_bodyIgIniq) of
                                { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                                (case (True) of
                                 { _lhsOisLamBody | _lhsOisLamBody `seq` (True) ->
                                 (case ((let sem_CExpr_Let_1 :: T_CExpr_1 
                                             sem_CExpr_Let_1  =
                                                 (\ _lhsIaccumARenMp
                                                    _lhsIlev
                                                    _lhsImoduleNm ->
                                                      (case (_lhsImoduleNm) of
                                                       { _bodyOmoduleNm | _bodyOmoduleNm `seq` (True) ->
                                                       (case (_lhsIlev) of
                                                        { _bodyOlev | _bodyOlev `seq` (True) ->
                                                        (case (_lhsIlev == cLevModule) of
                                                         { _isGlobal | _isGlobal `seq` (True) ->
                                                         (case (__tup3) of
                                                          { (_,_lIniq) | _lIniq `seq` (True) ->
                                                          (case (\m -> aRenAdd _lhsIropts _isGlobal
                                                                               _lhsImoduleNm
                                                                               _bindsInmL _lIniq m) of
                                                           { _addToARenMp | _addToARenMp `seq` (True) ->
                                                           (case (if   _isGlobal
                                                                  then _addToARenMp _lhsIaccumARenMp
                                                                  else              _lhsIaccumARenMp) of
                                                            { _bodyOaccumARenMp | _bodyOaccumARenMp `seq` (True) ->
                                                            (case (body_1 _bodyOaccumARenMp _bodyOlev _bodyOmoduleNm ) of
                                                             { ( _bodyIaccumARenMp,body_2) | True ->
                                                                 (case (_bodyIaccumARenMp) of
                                                                  { _lhsOaccumARenMp | _lhsOaccumARenMp `seq` (True) ->
                                                                  (case ((let sem_CExpr_Let_2 :: T_CExpr_2 
                                                                              sem_CExpr_Let_2  =
                                                                                  (\ _lhsIaRenMp ->
                                                                                       (case (if   _isGlobal
                                                                                              then              _lhsIaRenMp
                                                                                              else _addToARenMp _lhsIaRenMp) of
                                                                                        { _aRenMp | _aRenMp `seq` (True) ->
                                                                                        (case (_aRenMp) of
                                                                                         { _bodyOaRenMp | _bodyOaRenMp `seq` (True) ->
                                                                                         (case (_lhsImoduleNm) of
                                                                                          { _bindsOmoduleNm | _bindsOmoduleNm `seq` (True) ->
                                                                                          (case (_aRenMp) of
                                                                                           { _bindsOaRenMp | _bindsOaRenMp `seq` (True) ->
                                                                                           (case (_lhsIlev + 1) of
                                                                                            { _bindsOlev | _bindsOlev `seq` (True) ->
                                                                                            (case (body_2 _bodyOaRenMp ) of
                                                                                             { ( _bodyIcTrf) | True ->
                                                                                                 (case (binds_2 _bindsOaRenMp _bindsOlev _bindsOmoduleNm ) of
                                                                                                  { ( _bindsIcTrf) | True ->
                                                                                                      (case (CExpr_Let categ_ _bindsIcTrf _bodyIcTrf) of
                                                                                                       { _cTrf | _cTrf `seq` (True) ->
                                                                                                       (case (_cTrf) of
                                                                                                        { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                                                        ( _lhsOcTrf) }) }) }) }) }) }) }) }) }))
                                                                          in  sem_CExpr_Let_2)) of
                                                                   { ( sem_CExpr_2) | True ->
                                                                   ( _lhsOaccumARenMp,sem_CExpr_2) }) }) }) }) }) }) }) }) }))
                                         in  sem_CExpr_Let_1)) of
                                  { ( sem_CExpr_1) | True ->
                                  ( _lhsOgIniq,_lhsOisLamBody,sem_CExpr_1) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_String :: String ->
                    T_CExpr 
sem_CExpr_String str_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case (True) of
           { _lhsOisLamBody | _lhsOisLamBody `seq` (True) ->
           (case ((let sem_CExpr_String_1 :: T_CExpr_1 
                       sem_CExpr_String_1  =
                           (\ _lhsIaccumARenMp
                              _lhsIlev
                              _lhsImoduleNm ->
                                (case (_lhsIaccumARenMp) of
                                 { _lhsOaccumARenMp | _lhsOaccumARenMp `seq` (True) ->
                                 (case ((let sem_CExpr_String_2 :: T_CExpr_2 
                                             sem_CExpr_String_2  =
                                                 (\ _lhsIaRenMp ->
                                                      (case (CExpr_String str_) of
                                                       { _cTrf | _cTrf `seq` (True) ->
                                                       (case (_cTrf) of
                                                        { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                        ( _lhsOcTrf) }) }))
                                         in  sem_CExpr_String_2)) of
                                  { ( sem_CExpr_2) | True ->
                                  ( _lhsOaccumARenMp,sem_CExpr_2) }) }))
                   in  sem_CExpr_String_1)) of
            { ( sem_CExpr_1) | True ->
            ( _lhsOgIniq,_lhsOisLamBody,sem_CExpr_1) }) }) }))
sem_CExpr_Tup :: CTag ->
                 T_CExpr 
sem_CExpr_Tup tag_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case (True) of
           { _lhsOisLamBody | _lhsOisLamBody `seq` (True) ->
           (case ((let sem_CExpr_Tup_1 :: T_CExpr_1 
                       sem_CExpr_Tup_1  =
                           (\ _lhsIaccumARenMp
                              _lhsIlev
                              _lhsImoduleNm ->
                                (case (_lhsIaccumARenMp) of
                                 { _lhsOaccumARenMp | _lhsOaccumARenMp `seq` (True) ->
                                 (case ((let sem_CExpr_Tup_2 :: T_CExpr_2 
                                             sem_CExpr_Tup_2  =
                                                 (\ _lhsIaRenMp ->
                                                      (case (CExpr_Tup tag_) of
                                                       { _cTrf | _cTrf `seq` (True) ->
                                                       (case (_cTrf) of
                                                        { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                        ( _lhsOcTrf) }) }))
                                         in  sem_CExpr_Tup_2)) of
                                  { ( sem_CExpr_2) | True ->
                                  ( _lhsOaccumARenMp,sem_CExpr_2) }) }))
                   in  sem_CExpr_Tup_1)) of
            { ( sem_CExpr_1) | True ->
            ( _lhsOgIniq,_lhsOisLamBody,sem_CExpr_1) }) }) }))
sem_CExpr_TupDel :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupDel expr_ tag_ nm_ offset_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIropts) of
          { _offsetOropts | _offsetOropts `seq` (True) ->
          (case (_lhsIprotectedBindingNames) of
           { _offsetOprotectedBindingNames | _offsetOprotectedBindingNames `seq` (True) ->
           (case (_lhsIropts) of
            { _exprOropts | _exprOropts `seq` (True) ->
            (case (_lhsIprotectedBindingNames) of
             { _exprOprotectedBindingNames | _exprOprotectedBindingNames `seq` (True) ->
             (case (_lhsIgIniq) of
              { _exprOgIniq | _exprOgIniq `seq` (True) ->
              (case (expr_ _exprOgIniq _exprOprotectedBindingNames _exprOropts ) of
               { ( _exprIgIniq,_exprIisLamBody,expr_1) | True ->
                   (case (_exprIgIniq) of
                    { _offsetOgIniq | _offsetOgIniq `seq` (True) ->
                    (case (offset_ _offsetOgIniq _offsetOprotectedBindingNames _offsetOropts ) of
                     { ( _offsetIgIniq,_offsetIisLamBody,offset_1) | True ->
                         (case (_offsetIgIniq) of
                          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                          (case (True) of
                           { _lhsOisLamBody | _lhsOisLamBody `seq` (True) ->
                           (case ((let sem_CExpr_TupDel_1 :: T_CExpr_1 
                                       sem_CExpr_TupDel_1  =
                                           (\ _lhsIaccumARenMp
                                              _lhsIlev
                                              _lhsImoduleNm ->
                                                (case (_lhsIaccumARenMp) of
                                                 { _lhsOaccumARenMp | _lhsOaccumARenMp `seq` (True) ->
                                                 (case ((let sem_CExpr_TupDel_2 :: T_CExpr_2 
                                                             sem_CExpr_TupDel_2  =
                                                                 (\ _lhsIaRenMp ->
                                                                      (case (_lhsImoduleNm) of
                                                                       { _offsetOmoduleNm | _offsetOmoduleNm `seq` (True) ->
                                                                       (case (_lhsIlev) of
                                                                        { _offsetOlev | _offsetOlev `seq` (True) ->
                                                                        (case (_lhsIaRenMp) of
                                                                         { _accumARenMp | _accumARenMp `seq` (True) ->
                                                                         (case (_accumARenMp) of
                                                                          { _offsetOaccumARenMp | _offsetOaccumARenMp `seq` (True) ->
                                                                          (case (offset_1 _offsetOaccumARenMp _offsetOlev _offsetOmoduleNm ) of
                                                                           { ( _offsetIaccumARenMp,offset_2) | True ->
                                                                               (case (_offsetIaccumARenMp) of
                                                                                { _aRenMp | _aRenMp `seq` (True) ->
                                                                                (case (_aRenMp) of
                                                                                 { _offsetOaRenMp | _offsetOaRenMp `seq` (True) ->
                                                                                 (case (_lhsImoduleNm) of
                                                                                  { _exprOmoduleNm | _exprOmoduleNm `seq` (True) ->
                                                                                  (case (_lhsIlev) of
                                                                                   { _exprOlev | _exprOlev `seq` (True) ->
                                                                                   (case (_aRenMp) of
                                                                                    { _exprOaRenMp | _exprOaRenMp `seq` (True) ->
                                                                                    (case (_lhsIaRenMp) of
                                                                                     { _exprOaccumARenMp | _exprOaccumARenMp `seq` (True) ->
                                                                                     (case (offset_2 _offsetOaRenMp ) of
                                                                                      { ( _offsetIcTrf) | True ->
                                                                                          (case (expr_1 _exprOaccumARenMp _exprOlev _exprOmoduleNm ) of
                                                                                           { ( _exprIaccumARenMp,expr_2) | True ->
                                                                                               (case (expr_2 _exprOaRenMp ) of
                                                                                                { ( _exprIcTrf) | True ->
                                                                                                    (case (CExpr_TupDel _exprIcTrf tag_ nm_ _offsetIcTrf) of
                                                                                                     { _cTrf | _cTrf `seq` (True) ->
                                                                                                     (case (_cTrf) of
                                                                                                      { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                                                      ( _lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                                         in  sem_CExpr_TupDel_2)) of
                                                  { ( sem_CExpr_2) | True ->
                                                  ( _lhsOaccumARenMp,sem_CExpr_2) }) }))
                                   in  sem_CExpr_TupDel_1)) of
                            { ( sem_CExpr_1) | True ->
                            ( _lhsOgIniq,_lhsOisLamBody,sem_CExpr_1) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_TupIns :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupIns expr_ tag_ nm_ offset_ fldExpr_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIropts) of
          { _fldExprOropts | _fldExprOropts `seq` (True) ->
          (case (_lhsIprotectedBindingNames) of
           { _fldExprOprotectedBindingNames | _fldExprOprotectedBindingNames `seq` (True) ->
           (case (_lhsIropts) of
            { _offsetOropts | _offsetOropts `seq` (True) ->
            (case (_lhsIprotectedBindingNames) of
             { _offsetOprotectedBindingNames | _offsetOprotectedBindingNames `seq` (True) ->
             (case (_lhsIropts) of
              { _exprOropts | _exprOropts `seq` (True) ->
              (case (_lhsIprotectedBindingNames) of
               { _exprOprotectedBindingNames | _exprOprotectedBindingNames `seq` (True) ->
               (case (_lhsIgIniq) of
                { _exprOgIniq | _exprOgIniq `seq` (True) ->
                (case (expr_ _exprOgIniq _exprOprotectedBindingNames _exprOropts ) of
                 { ( _exprIgIniq,_exprIisLamBody,expr_1) | True ->
                     (case (_exprIgIniq) of
                      { _offsetOgIniq | _offsetOgIniq `seq` (True) ->
                      (case (offset_ _offsetOgIniq _offsetOprotectedBindingNames _offsetOropts ) of
                       { ( _offsetIgIniq,_offsetIisLamBody,offset_1) | True ->
                           (case (_offsetIgIniq) of
                            { _fldExprOgIniq | _fldExprOgIniq `seq` (True) ->
                            (case (fldExpr_ _fldExprOgIniq _fldExprOprotectedBindingNames _fldExprOropts ) of
                             { ( _fldExprIgIniq,_fldExprIisLamBody,fldExpr_1) | True ->
                                 (case (_fldExprIgIniq) of
                                  { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                                  (case (True) of
                                   { _lhsOisLamBody | _lhsOisLamBody `seq` (True) ->
                                   (case ((let sem_CExpr_TupIns_1 :: T_CExpr_1 
                                               sem_CExpr_TupIns_1  =
                                                   (\ _lhsIaccumARenMp
                                                      _lhsIlev
                                                      _lhsImoduleNm ->
                                                        (case (_lhsIaccumARenMp) of
                                                         { _lhsOaccumARenMp | _lhsOaccumARenMp `seq` (True) ->
                                                         (case ((let sem_CExpr_TupIns_2 :: T_CExpr_2 
                                                                     sem_CExpr_TupIns_2  =
                                                                         (\ _lhsIaRenMp ->
                                                                              (case (_lhsImoduleNm) of
                                                                               { _fldExprOmoduleNm | _fldExprOmoduleNm `seq` (True) ->
                                                                               (case (_lhsIlev) of
                                                                                { _fldExprOlev | _fldExprOlev `seq` (True) ->
                                                                                (case (_lhsIaRenMp) of
                                                                                 { _accumARenMp | _accumARenMp `seq` (True) ->
                                                                                 (case (_accumARenMp) of
                                                                                  { _fldExprOaccumARenMp | _fldExprOaccumARenMp `seq` (True) ->
                                                                                  (case (fldExpr_1 _fldExprOaccumARenMp _fldExprOlev _fldExprOmoduleNm ) of
                                                                                   { ( _fldExprIaccumARenMp,fldExpr_2) | True ->
                                                                                       (case (_fldExprIaccumARenMp) of
                                                                                        { _aRenMp | _aRenMp `seq` (True) ->
                                                                                        (case (_aRenMp) of
                                                                                         { _fldExprOaRenMp | _fldExprOaRenMp `seq` (True) ->
                                                                                         (case (_lhsImoduleNm) of
                                                                                          { _offsetOmoduleNm | _offsetOmoduleNm `seq` (True) ->
                                                                                          (case (_lhsIlev) of
                                                                                           { _offsetOlev | _offsetOlev `seq` (True) ->
                                                                                           (case (_accumARenMp) of
                                                                                            { _offsetOaccumARenMp | _offsetOaccumARenMp `seq` (True) ->
                                                                                            (case (_aRenMp) of
                                                                                             { _offsetOaRenMp | _offsetOaRenMp `seq` (True) ->
                                                                                             (case (_lhsImoduleNm) of
                                                                                              { _exprOmoduleNm | _exprOmoduleNm `seq` (True) ->
                                                                                              (case (_lhsIlev) of
                                                                                               { _exprOlev | _exprOlev `seq` (True) ->
                                                                                               (case (_aRenMp) of
                                                                                                { _exprOaRenMp | _exprOaRenMp `seq` (True) ->
                                                                                                (case (_lhsIaRenMp) of
                                                                                                 { _exprOaccumARenMp | _exprOaccumARenMp `seq` (True) ->
                                                                                                 (case (fldExpr_2 _fldExprOaRenMp ) of
                                                                                                  { ( _fldExprIcTrf) | True ->
                                                                                                      (case (offset_1 _offsetOaccumARenMp _offsetOlev _offsetOmoduleNm ) of
                                                                                                       { ( _offsetIaccumARenMp,offset_2) | True ->
                                                                                                           (case (offset_2 _offsetOaRenMp ) of
                                                                                                            { ( _offsetIcTrf) | True ->
                                                                                                                (case (expr_1 _exprOaccumARenMp _exprOlev _exprOmoduleNm ) of
                                                                                                                 { ( _exprIaccumARenMp,expr_2) | True ->
                                                                                                                     (case (expr_2 _exprOaRenMp ) of
                                                                                                                      { ( _exprIcTrf) | True ->
                                                                                                                          (case (CExpr_TupIns _exprIcTrf tag_ nm_ _offsetIcTrf _fldExprIcTrf) of
                                                                                                                           { _cTrf | _cTrf `seq` (True) ->
                                                                                                                           (case (_cTrf) of
                                                                                                                            { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                                                                            ( _lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                                                 in  sem_CExpr_TupIns_2)) of
                                                          { ( sem_CExpr_2) | True ->
                                                          ( _lhsOaccumARenMp,sem_CExpr_2) }) }))
                                           in  sem_CExpr_TupIns_1)) of
                                    { ( sem_CExpr_1) | True ->
                                    ( _lhsOgIniq,_lhsOisLamBody,sem_CExpr_1) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_TupUpd :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupUpd expr_ tag_ nm_ offset_ fldExpr_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIropts) of
          { _fldExprOropts | _fldExprOropts `seq` (True) ->
          (case (_lhsIprotectedBindingNames) of
           { _fldExprOprotectedBindingNames | _fldExprOprotectedBindingNames `seq` (True) ->
           (case (_lhsIropts) of
            { _offsetOropts | _offsetOropts `seq` (True) ->
            (case (_lhsIprotectedBindingNames) of
             { _offsetOprotectedBindingNames | _offsetOprotectedBindingNames `seq` (True) ->
             (case (_lhsIropts) of
              { _exprOropts | _exprOropts `seq` (True) ->
              (case (_lhsIprotectedBindingNames) of
               { _exprOprotectedBindingNames | _exprOprotectedBindingNames `seq` (True) ->
               (case (_lhsIgIniq) of
                { _exprOgIniq | _exprOgIniq `seq` (True) ->
                (case (expr_ _exprOgIniq _exprOprotectedBindingNames _exprOropts ) of
                 { ( _exprIgIniq,_exprIisLamBody,expr_1) | True ->
                     (case (_exprIgIniq) of
                      { _offsetOgIniq | _offsetOgIniq `seq` (True) ->
                      (case (offset_ _offsetOgIniq _offsetOprotectedBindingNames _offsetOropts ) of
                       { ( _offsetIgIniq,_offsetIisLamBody,offset_1) | True ->
                           (case (_offsetIgIniq) of
                            { _fldExprOgIniq | _fldExprOgIniq `seq` (True) ->
                            (case (fldExpr_ _fldExprOgIniq _fldExprOprotectedBindingNames _fldExprOropts ) of
                             { ( _fldExprIgIniq,_fldExprIisLamBody,fldExpr_1) | True ->
                                 (case (_fldExprIgIniq) of
                                  { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                                  (case (True) of
                                   { _lhsOisLamBody | _lhsOisLamBody `seq` (True) ->
                                   (case ((let sem_CExpr_TupUpd_1 :: T_CExpr_1 
                                               sem_CExpr_TupUpd_1  =
                                                   (\ _lhsIaccumARenMp
                                                      _lhsIlev
                                                      _lhsImoduleNm ->
                                                        (case (_lhsIaccumARenMp) of
                                                         { _lhsOaccumARenMp | _lhsOaccumARenMp `seq` (True) ->
                                                         (case ((let sem_CExpr_TupUpd_2 :: T_CExpr_2 
                                                                     sem_CExpr_TupUpd_2  =
                                                                         (\ _lhsIaRenMp ->
                                                                              (case (_lhsImoduleNm) of
                                                                               { _fldExprOmoduleNm | _fldExprOmoduleNm `seq` (True) ->
                                                                               (case (_lhsIlev) of
                                                                                { _fldExprOlev | _fldExprOlev `seq` (True) ->
                                                                                (case (_lhsIaRenMp) of
                                                                                 { _accumARenMp | _accumARenMp `seq` (True) ->
                                                                                 (case (_accumARenMp) of
                                                                                  { _fldExprOaccumARenMp | _fldExprOaccumARenMp `seq` (True) ->
                                                                                  (case (fldExpr_1 _fldExprOaccumARenMp _fldExprOlev _fldExprOmoduleNm ) of
                                                                                   { ( _fldExprIaccumARenMp,fldExpr_2) | True ->
                                                                                       (case (_fldExprIaccumARenMp) of
                                                                                        { _aRenMp | _aRenMp `seq` (True) ->
                                                                                        (case (_aRenMp) of
                                                                                         { _fldExprOaRenMp | _fldExprOaRenMp `seq` (True) ->
                                                                                         (case (_lhsImoduleNm) of
                                                                                          { _offsetOmoduleNm | _offsetOmoduleNm `seq` (True) ->
                                                                                          (case (_lhsIlev) of
                                                                                           { _offsetOlev | _offsetOlev `seq` (True) ->
                                                                                           (case (_accumARenMp) of
                                                                                            { _offsetOaccumARenMp | _offsetOaccumARenMp `seq` (True) ->
                                                                                            (case (_aRenMp) of
                                                                                             { _offsetOaRenMp | _offsetOaRenMp `seq` (True) ->
                                                                                             (case (_lhsImoduleNm) of
                                                                                              { _exprOmoduleNm | _exprOmoduleNm `seq` (True) ->
                                                                                              (case (_lhsIlev) of
                                                                                               { _exprOlev | _exprOlev `seq` (True) ->
                                                                                               (case (_aRenMp) of
                                                                                                { _exprOaRenMp | _exprOaRenMp `seq` (True) ->
                                                                                                (case (_lhsIaRenMp) of
                                                                                                 { _exprOaccumARenMp | _exprOaccumARenMp `seq` (True) ->
                                                                                                 (case (fldExpr_2 _fldExprOaRenMp ) of
                                                                                                  { ( _fldExprIcTrf) | True ->
                                                                                                      (case (offset_1 _offsetOaccumARenMp _offsetOlev _offsetOmoduleNm ) of
                                                                                                       { ( _offsetIaccumARenMp,offset_2) | True ->
                                                                                                           (case (offset_2 _offsetOaRenMp ) of
                                                                                                            { ( _offsetIcTrf) | True ->
                                                                                                                (case (expr_1 _exprOaccumARenMp _exprOlev _exprOmoduleNm ) of
                                                                                                                 { ( _exprIaccumARenMp,expr_2) | True ->
                                                                                                                     (case (expr_2 _exprOaRenMp ) of
                                                                                                                      { ( _exprIcTrf) | True ->
                                                                                                                          (case (CExpr_TupUpd _exprIcTrf tag_ nm_ _offsetIcTrf _fldExprIcTrf) of
                                                                                                                           { _cTrf | _cTrf `seq` (True) ->
                                                                                                                           (case (_cTrf) of
                                                                                                                            { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                                                                            ( _lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                                                 in  sem_CExpr_TupUpd_2)) of
                                                          { ( sem_CExpr_2) | True ->
                                                          ( _lhsOaccumARenMp,sem_CExpr_2) }) }))
                                           in  sem_CExpr_TupUpd_1)) of
                                    { ( sem_CExpr_1) | True ->
                                    ( _lhsOgIniq,_lhsOisLamBody,sem_CExpr_1) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_Var :: ACoreBindRef ->
                 T_CExpr 
sem_CExpr_Var ref_  =
    (\ _lhsIgIniq
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case (True) of
           { _lhsOisLamBody | _lhsOisLamBody `seq` (True) ->
           (case ((let sem_CExpr_Var_1 :: T_CExpr_1 
                       sem_CExpr_Var_1  =
                           (\ _lhsIaccumARenMp
                              _lhsIlev
                              _lhsImoduleNm ->
                                (case (_lhsIaccumARenMp) of
                                 { _lhsOaccumARenMp | _lhsOaccumARenMp `seq` (True) ->
                                 (case ((let sem_CExpr_Var_2 :: T_CExpr_2 
                                             sem_CExpr_Var_2  =
                                                 (\ _lhsIaRenMp ->
                                                      (case (acbrefNm ref_) of
                                                       { _nm | _nm `seq` (True) ->
                                                       (case (acoreVar (aRenRepl _lhsIaRenMp _nm)) of
                                                        { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                        ( _lhsOcTrf) }) }))
                                         in  sem_CExpr_Var_2)) of
                                  { ( sem_CExpr_2) | True ->
                                  ( _lhsOaccumARenMp,sem_CExpr_2) }) }))
                   in  sem_CExpr_Var_1)) of
            { ( sem_CExpr_1) | True ->
            ( _lhsOgIniq,_lhsOisLamBody,sem_CExpr_1) }) }) }))
-- CExprAnn ----------------------------------------------------
{-
   visit 0:
      chained attribute:
         gIniq                : Int
   visit 1:
      inherited attributes:
         aRenMp               : ARenMp
         lev                  : Int
         moduleNm             : HsName
         ropts                : RenUniqOpts
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Coe:
         child coe            : {RelevCoe}
         visit 1:
            local cTrf        : _
      alternative Debug:
         child info           : {String}
         visit 1:
            local cTrf        : _
      alternative Ty:
         child ty             : {Ty}
         visit 1:
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
                   ( Int,T_CExprAnn_1 )
type T_CExprAnn_1  = ARenMp ->
                     Int ->
                     HsName ->
                     RenUniqOpts ->
                     ( CExprAnn )
sem_CExprAnn_Coe :: RelevCoe ->
                    T_CExprAnn 
sem_CExprAnn_Coe coe_  =
    (\ _lhsIgIniq ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case ((let sem_CExprAnn_Coe_1 :: T_CExprAnn_1 
                      sem_CExprAnn_Coe_1  =
                          (\ _lhsIaRenMp
                             _lhsIlev
                             _lhsImoduleNm
                             _lhsIropts ->
                               (case (CExprAnn_Coe coe_) of
                                { _cTrf | _cTrf `seq` (True) ->
                                (case (_cTrf) of
                                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                 ( _lhsOcTrf) }) }))
                  in  sem_CExprAnn_Coe_1)) of
           { ( sem_CExprAnn_1) | True ->
           ( _lhsOgIniq,sem_CExprAnn_1) }) }))
sem_CExprAnn_Debug :: String ->
                      T_CExprAnn 
sem_CExprAnn_Debug info_  =
    (\ _lhsIgIniq ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case ((let sem_CExprAnn_Debug_1 :: T_CExprAnn_1 
                      sem_CExprAnn_Debug_1  =
                          (\ _lhsIaRenMp
                             _lhsIlev
                             _lhsImoduleNm
                             _lhsIropts ->
                               (case (CExprAnn_Debug info_) of
                                { _cTrf | _cTrf `seq` (True) ->
                                (case (_cTrf) of
                                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                 ( _lhsOcTrf) }) }))
                  in  sem_CExprAnn_Debug_1)) of
           { ( sem_CExprAnn_1) | True ->
           ( _lhsOgIniq,sem_CExprAnn_1) }) }))
sem_CExprAnn_Ty :: Ty ->
                   T_CExprAnn 
sem_CExprAnn_Ty ty_  =
    (\ _lhsIgIniq ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case ((let sem_CExprAnn_Ty_1 :: T_CExprAnn_1 
                      sem_CExprAnn_Ty_1  =
                          (\ _lhsIaRenMp
                             _lhsIlev
                             _lhsImoduleNm
                             _lhsIropts ->
                               (case (CExprAnn_Ty ty_) of
                                { _cTrf | _cTrf `seq` (True) ->
                                (case (_cTrf) of
                                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                 ( _lhsOcTrf) }) }))
                  in  sem_CExprAnn_Ty_1)) of
           { ( sem_CExprAnn_1) | True ->
           ( _lhsOgIniq,sem_CExprAnn_1) }) }))
-- CMetaBind ---------------------------------------------------
{-
   visit 0:
      chained attribute:
         gIniq                : Int
   visit 1:
      inherited attributes:
         aRenMp               : ARenMp
         lev                  : Int
         moduleNm             : HsName
         ropts                : RenUniqOpts
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Apply0:
         visit 1:
            local cTrf        : _
      alternative Function0:
         visit 1:
            local cTrf        : _
      alternative Function1:
         visit 1:
            local cTrf        : _
      alternative Plain:
         visit 1:
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
                    ( Int,T_CMetaBind_1 )
type T_CMetaBind_1  = ARenMp ->
                      Int ->
                      HsName ->
                      RenUniqOpts ->
                      ( CMetaBind )
sem_CMetaBind_Apply0 :: T_CMetaBind 
sem_CMetaBind_Apply0  =
    (\ _lhsIgIniq ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case ((let sem_CMetaBind_Apply0_1 :: T_CMetaBind_1 
                      sem_CMetaBind_Apply0_1  =
                          (\ _lhsIaRenMp
                             _lhsIlev
                             _lhsImoduleNm
                             _lhsIropts ->
                               (case (CMetaBind_Apply0) of
                                { _cTrf | _cTrf `seq` (True) ->
                                (case (_cTrf) of
                                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                 ( _lhsOcTrf) }) }))
                  in  sem_CMetaBind_Apply0_1)) of
           { ( sem_CMetaBind_1) | True ->
           ( _lhsOgIniq,sem_CMetaBind_1) }) }))
sem_CMetaBind_Function0 :: T_CMetaBind 
sem_CMetaBind_Function0  =
    (\ _lhsIgIniq ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case ((let sem_CMetaBind_Function0_1 :: T_CMetaBind_1 
                      sem_CMetaBind_Function0_1  =
                          (\ _lhsIaRenMp
                             _lhsIlev
                             _lhsImoduleNm
                             _lhsIropts ->
                               (case (CMetaBind_Function0) of
                                { _cTrf | _cTrf `seq` (True) ->
                                (case (_cTrf) of
                                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                 ( _lhsOcTrf) }) }))
                  in  sem_CMetaBind_Function0_1)) of
           { ( sem_CMetaBind_1) | True ->
           ( _lhsOgIniq,sem_CMetaBind_1) }) }))
sem_CMetaBind_Function1 :: T_CMetaBind 
sem_CMetaBind_Function1  =
    (\ _lhsIgIniq ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case ((let sem_CMetaBind_Function1_1 :: T_CMetaBind_1 
                      sem_CMetaBind_Function1_1  =
                          (\ _lhsIaRenMp
                             _lhsIlev
                             _lhsImoduleNm
                             _lhsIropts ->
                               (case (CMetaBind_Function1) of
                                { _cTrf | _cTrf `seq` (True) ->
                                (case (_cTrf) of
                                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                 ( _lhsOcTrf) }) }))
                  in  sem_CMetaBind_Function1_1)) of
           { ( sem_CMetaBind_1) | True ->
           ( _lhsOgIniq,sem_CMetaBind_1) }) }))
sem_CMetaBind_Plain :: T_CMetaBind 
sem_CMetaBind_Plain  =
    (\ _lhsIgIniq ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case ((let sem_CMetaBind_Plain_1 :: T_CMetaBind_1 
                      sem_CMetaBind_Plain_1  =
                          (\ _lhsIaRenMp
                             _lhsIlev
                             _lhsImoduleNm
                             _lhsIropts ->
                               (case (CMetaBind_Plain) of
                                { _cTrf | _cTrf `seq` (True) ->
                                (case (_cTrf) of
                                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                 ( _lhsOcTrf) }) }))
                  in  sem_CMetaBind_Plain_1)) of
           { ( sem_CMetaBind_1) | True ->
           ( _lhsOgIniq,sem_CMetaBind_1) }) }))
-- CMetaVal ----------------------------------------------------
{-
   visit 0:
      chained attribute:
         gIniq                : Int
      synthesized attribute:
         protectableBindingNames : [HsName]
   visit 1:
      inherited attributes:
         aRenMp               : ARenMp
         lev                  : Int
         moduleNm             : HsName
         ropts                : RenUniqOpts
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Dict:
         visit 1:
            local cTrf        : _
      alternative DictClass:
         child tracks         : {[Track]}
         visit 1:
            local cTrf        : _
      alternative DictInstance:
         child tracks         : {[Track]}
         visit 1:
            local cTrf        : _
      alternative Track:
         child track          : {Track}
         visit 1:
            local cTrf        : _
      alternative Val:
         visit 1:
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
                   ( Int,([HsName]),T_CMetaVal_1 )
type T_CMetaVal_1  = ARenMp ->
                     Int ->
                     HsName ->
                     RenUniqOpts ->
                     ( CMetaVal )
sem_CMetaVal_Dict :: T_CMetaVal 
sem_CMetaVal_Dict  =
    (\ _lhsIgIniq ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case ([]) of
           { _lhsOprotectableBindingNames | _lhsOprotectableBindingNames `seq` (True) ->
           (case ((let sem_CMetaVal_Dict_1 :: T_CMetaVal_1 
                       sem_CMetaVal_Dict_1  =
                           (\ _lhsIaRenMp
                              _lhsIlev
                              _lhsImoduleNm
                              _lhsIropts ->
                                (case (CMetaVal_Dict) of
                                 { _cTrf | _cTrf `seq` (True) ->
                                 (case (_cTrf) of
                                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                  ( _lhsOcTrf) }) }))
                   in  sem_CMetaVal_Dict_1)) of
            { ( sem_CMetaVal_1) | True ->
            ( _lhsOgIniq,_lhsOprotectableBindingNames,sem_CMetaVal_1) }) }) }))
sem_CMetaVal_DictClass :: ([Track]) ->
                          T_CMetaVal 
sem_CMetaVal_DictClass tracks_  =
    (\ _lhsIgIniq ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case ([ nm  | TrackVarApply nm _ <- tracks_ ]) of
           { _lhsOprotectableBindingNames | _lhsOprotectableBindingNames `seq` (True) ->
           (case ((let sem_CMetaVal_DictClass_1 :: T_CMetaVal_1 
                       sem_CMetaVal_DictClass_1  =
                           (\ _lhsIaRenMp
                              _lhsIlev
                              _lhsImoduleNm
                              _lhsIropts ->
                                (case (CMetaVal_DictClass tracks_) of
                                 { _cTrf | _cTrf `seq` (True) ->
                                 (case (_cTrf) of
                                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                  ( _lhsOcTrf) }) }))
                   in  sem_CMetaVal_DictClass_1)) of
            { ( sem_CMetaVal_1) | True ->
            ( _lhsOgIniq,_lhsOprotectableBindingNames,sem_CMetaVal_1) }) }) }))
sem_CMetaVal_DictInstance :: ([Track]) ->
                             T_CMetaVal 
sem_CMetaVal_DictInstance tracks_  =
    (\ _lhsIgIniq ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case ([ nm  | TrackVarApply nm _ <- tracks_ ]) of
           { _lhsOprotectableBindingNames | _lhsOprotectableBindingNames `seq` (True) ->
           (case ((let sem_CMetaVal_DictInstance_1 :: T_CMetaVal_1 
                       sem_CMetaVal_DictInstance_1  =
                           (\ _lhsIaRenMp
                              _lhsIlev
                              _lhsImoduleNm
                              _lhsIropts ->
                                (case (CMetaVal_DictInstance tracks_) of
                                 { _cTrf | _cTrf `seq` (True) ->
                                 (case (_cTrf) of
                                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                  ( _lhsOcTrf) }) }))
                   in  sem_CMetaVal_DictInstance_1)) of
            { ( sem_CMetaVal_1) | True ->
            ( _lhsOgIniq,_lhsOprotectableBindingNames,sem_CMetaVal_1) }) }) }))
sem_CMetaVal_Track :: Track ->
                      T_CMetaVal 
sem_CMetaVal_Track track_  =
    (\ _lhsIgIniq ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case ([]) of
           { _lhsOprotectableBindingNames | _lhsOprotectableBindingNames `seq` (True) ->
           (case ((let sem_CMetaVal_Track_1 :: T_CMetaVal_1 
                       sem_CMetaVal_Track_1  =
                           (\ _lhsIaRenMp
                              _lhsIlev
                              _lhsImoduleNm
                              _lhsIropts ->
                                (case (CMetaVal_Track track_) of
                                 { _cTrf | _cTrf `seq` (True) ->
                                 (case (_cTrf) of
                                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                  ( _lhsOcTrf) }) }))
                   in  sem_CMetaVal_Track_1)) of
            { ( sem_CMetaVal_1) | True ->
            ( _lhsOgIniq,_lhsOprotectableBindingNames,sem_CMetaVal_1) }) }) }))
sem_CMetaVal_Val :: T_CMetaVal 
sem_CMetaVal_Val  =
    (\ _lhsIgIniq ->
         (case (_lhsIgIniq) of
          { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
          (case ([]) of
           { _lhsOprotectableBindingNames | _lhsOprotectableBindingNames `seq` (True) ->
           (case ((let sem_CMetaVal_Val_1 :: T_CMetaVal_1 
                       sem_CMetaVal_Val_1  =
                           (\ _lhsIaRenMp
                              _lhsIlev
                              _lhsImoduleNm
                              _lhsIropts ->
                                (case (CMetaVal_Val) of
                                 { _cTrf | _cTrf `seq` (True) ->
                                 (case (_cTrf) of
                                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                  ( _lhsOcTrf) }) }))
                   in  sem_CMetaVal_Val_1)) of
            { ( sem_CMetaVal_1) | True ->
            ( _lhsOgIniq,_lhsOprotectableBindingNames,sem_CMetaVal_1) }) }) }))
-- CMetas ------------------------------------------------------
{-
   visit 0:
      chained attribute:
         gIniq                : Int
      synthesized attribute:
         protectableBindingNames : [HsName]
   visit 1:
      inherited attributes:
         aRenMp               : ARenMp
         lev                  : Int
         moduleNm             : HsName
         ropts                : RenUniqOpts
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Tuple:
         child x1             : CMetaBind 
         child x2             : CMetaVal 
         visit 1:
            local cTrf        : _
-}
-- cata
sem_CMetas :: CMetas  ->
              T_CMetas 
sem_CMetas ( x1,x2)  =
    (sem_CMetas_Tuple (sem_CMetaBind x1 ) (sem_CMetaVal x2 ) )
-- semantic domain
type T_CMetas  = Int ->
                 ( Int,([HsName]),T_CMetas_1 )
type T_CMetas_1  = ARenMp ->
                   Int ->
                   HsName ->
                   RenUniqOpts ->
                   ( CMetas )
sem_CMetas_Tuple :: T_CMetaBind  ->
                    T_CMetaVal  ->
                    T_CMetas 
sem_CMetas_Tuple x1_ x2_  =
    (\ _lhsIgIniq ->
         (case (_lhsIgIniq) of
          { _x1OgIniq | _x1OgIniq `seq` (True) ->
          (case (x1_ _x1OgIniq ) of
           { ( _x1IgIniq,x1_1) | True ->
               (case (_x1IgIniq) of
                { _x2OgIniq | _x2OgIniq `seq` (True) ->
                (case (x2_ _x2OgIniq ) of
                 { ( _x2IgIniq,_x2IprotectableBindingNames,x2_1) | True ->
                     (case (_x2IgIniq) of
                      { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                      (case (_x2IprotectableBindingNames) of
                       { _lhsOprotectableBindingNames | _lhsOprotectableBindingNames `seq` (True) ->
                       (case ((let sem_CMetas_Tuple_1 :: T_CMetas_1 
                                   sem_CMetas_Tuple_1  =
                                       (\ _lhsIaRenMp
                                          _lhsIlev
                                          _lhsImoduleNm
                                          _lhsIropts ->
                                            (case (_lhsIropts) of
                                             { _x2Oropts | _x2Oropts `seq` (True) ->
                                             (case (_lhsImoduleNm) of
                                              { _x2OmoduleNm | _x2OmoduleNm `seq` (True) ->
                                              (case (_lhsIlev) of
                                               { _x2Olev | _x2Olev `seq` (True) ->
                                               (case (_lhsIaRenMp) of
                                                { _x2OaRenMp | _x2OaRenMp `seq` (True) ->
                                                (case (x2_1 _x2OaRenMp _x2Olev _x2OmoduleNm _x2Oropts ) of
                                                 { ( _x2IcTrf) | True ->
                                                     (case (_lhsIropts) of
                                                      { _x1Oropts | _x1Oropts `seq` (True) ->
                                                      (case (_lhsImoduleNm) of
                                                       { _x1OmoduleNm | _x1OmoduleNm `seq` (True) ->
                                                       (case (_lhsIlev) of
                                                        { _x1Olev | _x1Olev `seq` (True) ->
                                                        (case (_lhsIaRenMp) of
                                                         { _x1OaRenMp | _x1OaRenMp `seq` (True) ->
                                                         (case (x1_1 _x1OaRenMp _x1Olev _x1OmoduleNm _x1Oropts ) of
                                                          { ( _x1IcTrf) | True ->
                                                              (case ((_x1IcTrf,_x2IcTrf)) of
                                                               { _cTrf | _cTrf `seq` (True) ->
                                                               (case (_cTrf) of
                                                                { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                ( _lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }))
                               in  sem_CMetas_Tuple_1)) of
                        { ( sem_CMetas_1) | True ->
                        ( _lhsOgIniq,_lhsOprotectableBindingNames,sem_CMetas_1) }) }) }) }) }) }) }))
-- CModule -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         aRenMp               : ARenMp
         lev                  : Int
         ropts                : RenUniqOpts
      chained attribute:
         gIniq                : Int
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Mod:
         child moduleNm       : {HsName}
         child expr           : CExpr 
         child ctagsMp        : {CTagsMp}
         visit 0:
            local aRenMp      : _
            local cTrf        : _
-}
-- cata
sem_CModule :: CModule  ->
               T_CModule 
sem_CModule (CModule_Mod _moduleNm _expr _ctagsMp )  =
    (sem_CModule_Mod _moduleNm (sem_CExpr _expr ) _ctagsMp )
-- semantic domain
type T_CModule  = ARenMp ->
                  Int ->
                  Int ->
                  RenUniqOpts ->
                  ( CModule ,Int)
sem_CModule_Mod :: HsName ->
                   T_CExpr  ->
                   CTagsMp ->
                   T_CModule 
sem_CModule_Mod moduleNm_ expr_ ctagsMp_  =
    (\ _lhsIaRenMp
       _lhsIgIniq
       _lhsIlev
       _lhsIropts ->
         (case (_lhsIropts) of
          { _exprOropts | _exprOropts `seq` (True) ->
          (case (_lhsIlev) of
           { _exprOlev | _exprOlev `seq` (True) ->
           (case (_lhsIgIniq) of
            { _exprOgIniq | _exprOgIniq `seq` (True) ->
            (case ([]) of
             { _exprOprotectedBindingNames | _exprOprotectedBindingNames `seq` (True) ->
             (case (_lhsIaRenMp) of
              { _exprOaccumARenMp | _exprOaccumARenMp `seq` (True) ->
              (case (moduleNm_) of
               { _exprOmoduleNm | _exprOmoduleNm `seq` (True) ->
               (case (expr_ _exprOgIniq _exprOprotectedBindingNames _exprOropts ) of
                { ( _exprIgIniq,_exprIisLamBody,expr_1) | True ->
                    (case (expr_1 _exprOaccumARenMp _exprOlev _exprOmoduleNm ) of
                     { ( _exprIaccumARenMp,expr_2) | True ->
                         (case (_exprIaccumARenMp) of
                          { _aRenMp | _aRenMp `seq` (True) ->
                          (case (_aRenMp) of
                           { _exprOaRenMp | _exprOaRenMp `seq` (True) ->
                           (case (expr_2 _exprOaRenMp ) of
                            { ( _exprIcTrf) | True ->
                                (case (CModule_Mod moduleNm_ _exprIcTrf ctagsMp_) of
                                 { _cTrf | _cTrf `seq` (True) ->
                                 (case (_cTrf) of
                                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                  (case (_exprIgIniq) of
                                   { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                                   ( _lhsOcTrf,_lhsOgIniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- CPat --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         nmL                  : [HsName]
   visit 1:
      inherited attribute:
         ropts                : RenUniqOpts
      chained attribute:
         gIniq                : Int
   visit 2:
      inherited attributes:
         aRenMp               : ARenMp
         lev                  : Int
         moduleNm             : HsName
      synthesized attributes:
         cTrf                 : SELF 
         fldNmL               : [HsName]
   alternatives:
      alternative BoolExpr:
         child cexpr          : {CExpr}
         visit 2:
            local cTrf        : _
      alternative Char:
         child char           : {Char}
         visit 2:
            local cTrf        : _
      alternative Con:
         child tag            : {CTag}
         child rest           : CPatRest 
         child binds          : CPatFldL 
         visit 2:
            local cTrf        : _
      alternative Int:
         child int            : {Int}
         visit 2:
            local cTrf        : _
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
type T_CPat  = ( ([HsName]),T_CPat_1 )
type T_CPat_1  = Int ->
                 RenUniqOpts ->
                 ( Int,T_CPat_2 )
type T_CPat_2  = ARenMp ->
                 Int ->
                 HsName ->
                 ( CPat ,([HsName]))
sem_CPat_BoolExpr :: CExpr ->
                     T_CPat 
sem_CPat_BoolExpr cexpr_  =
    (case ([]) of
     { _lhsOnmL | _lhsOnmL `seq` (True) ->
     (case ((let sem_CPat_BoolExpr_1 :: T_CPat_1 
                 sem_CPat_BoolExpr_1  =
                     (\ _lhsIgIniq
                        _lhsIropts ->
                          (case (_lhsIgIniq) of
                           { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                           (case ((let sem_CPat_BoolExpr_2 :: T_CPat_2 
                                       sem_CPat_BoolExpr_2  =
                                           (\ _lhsIaRenMp
                                              _lhsIlev
                                              _lhsImoduleNm ->
                                                (case (CPat_BoolExpr cexpr_) of
                                                 { _cTrf | _cTrf `seq` (True) ->
                                                 (case (_cTrf) of
                                                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                  (case ([]) of
                                                   { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
                                                   ( _lhsOcTrf,_lhsOfldNmL) }) }) }))
                                   in  sem_CPat_BoolExpr_2)) of
                            { ( sem_CPat_2) | True ->
                            ( _lhsOgIniq,sem_CPat_2) }) }))
             in  sem_CPat_BoolExpr_1)) of
      { ( sem_CPat_1) | True ->
      ( _lhsOnmL,sem_CPat_1) }) })
sem_CPat_Char :: Char ->
                 T_CPat 
sem_CPat_Char char_  =
    (case ([]) of
     { _lhsOnmL | _lhsOnmL `seq` (True) ->
     (case ((let sem_CPat_Char_1 :: T_CPat_1 
                 sem_CPat_Char_1  =
                     (\ _lhsIgIniq
                        _lhsIropts ->
                          (case (_lhsIgIniq) of
                           { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                           (case ((let sem_CPat_Char_2 :: T_CPat_2 
                                       sem_CPat_Char_2  =
                                           (\ _lhsIaRenMp
                                              _lhsIlev
                                              _lhsImoduleNm ->
                                                (case (CPat_Char char_) of
                                                 { _cTrf | _cTrf `seq` (True) ->
                                                 (case (_cTrf) of
                                                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                  (case ([]) of
                                                   { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
                                                   ( _lhsOcTrf,_lhsOfldNmL) }) }) }))
                                   in  sem_CPat_Char_2)) of
                            { ( sem_CPat_2) | True ->
                            ( _lhsOgIniq,sem_CPat_2) }) }))
             in  sem_CPat_Char_1)) of
      { ( sem_CPat_1) | True ->
      ( _lhsOnmL,sem_CPat_1) }) })
sem_CPat_Con :: CTag ->
                T_CPatRest  ->
                T_CPatFldL  ->
                T_CPat 
sem_CPat_Con tag_ rest_ binds_  =
    (case (binds_ ) of
     { ( _bindsInmL,binds_1) | True ->
         (case (rest_ ) of
          { ( _restInmL,rest_1) | True ->
              (case (_restInmL ++ _bindsInmL) of
               { _lhsOnmL | _lhsOnmL `seq` (True) ->
               (case ((let sem_CPat_Con_1 :: T_CPat_1 
                           sem_CPat_Con_1  =
                               (\ _lhsIgIniq
                                  _lhsIropts ->
                                    (case (_lhsIropts) of
                                     { _bindsOropts | _bindsOropts `seq` (True) ->
                                     (case (_lhsIgIniq) of
                                      { _restOgIniq | _restOgIniq `seq` (True) ->
                                      (case (rest_1 _restOgIniq ) of
                                       { ( _restIgIniq,rest_2) | True ->
                                           (case (_restIgIniq) of
                                            { _bindsOgIniq | _bindsOgIniq `seq` (True) ->
                                            (case (binds_1 _bindsOgIniq _bindsOropts ) of
                                             { ( _bindsIgIniq,binds_2) | True ->
                                                 (case (_bindsIgIniq) of
                                                  { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                                                  (case ((let sem_CPat_Con_2 :: T_CPat_2 
                                                              sem_CPat_Con_2  =
                                                                  (\ _lhsIaRenMp
                                                                     _lhsIlev
                                                                     _lhsImoduleNm ->
                                                                       (case (_lhsImoduleNm) of
                                                                        { _bindsOmoduleNm | _bindsOmoduleNm `seq` (True) ->
                                                                        (case (_lhsIlev) of
                                                                         { _bindsOlev | _bindsOlev `seq` (True) ->
                                                                         (case (_lhsIaRenMp) of
                                                                          { _bindsOaRenMp | _bindsOaRenMp `seq` (True) ->
                                                                          (case (_lhsIaRenMp) of
                                                                           { _restOaRenMp | _restOaRenMp `seq` (True) ->
                                                                           (case (binds_2 _bindsOaRenMp _bindsOlev _bindsOmoduleNm ) of
                                                                            { ( _bindsIcTrf,_bindsIfldNmL) | True ->
                                                                                (case (_lhsIropts) of
                                                                                 { _restOropts | _restOropts `seq` (True) ->
                                                                                 (case (_lhsImoduleNm) of
                                                                                  { _restOmoduleNm | _restOmoduleNm `seq` (True) ->
                                                                                  (case (_lhsIlev) of
                                                                                   { _restOlev | _restOlev `seq` (True) ->
                                                                                   (case (rest_2 _restOaRenMp _restOlev _restOmoduleNm _restOropts ) of
                                                                                    { ( _restIcTrf) | True ->
                                                                                        (case (CPat_Con tag_ _restIcTrf _bindsIcTrf) of
                                                                                         { _cTrf | _cTrf `seq` (True) ->
                                                                                         (case (_cTrf) of
                                                                                          { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                                          (case (_bindsIfldNmL) of
                                                                                           { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
                                                                                           ( _lhsOcTrf,_lhsOfldNmL) }) }) }) }) }) }) }) }) }) }) }) }))
                                                          in  sem_CPat_Con_2)) of
                                                   { ( sem_CPat_2) | True ->
                                                   ( _lhsOgIniq,sem_CPat_2) }) }) }) }) }) }) }))
                       in  sem_CPat_Con_1)) of
                { ( sem_CPat_1) | True ->
                ( _lhsOnmL,sem_CPat_1) }) }) }) })
sem_CPat_Int :: Int ->
                T_CPat 
sem_CPat_Int int_  =
    (case ([]) of
     { _lhsOnmL | _lhsOnmL `seq` (True) ->
     (case ((let sem_CPat_Int_1 :: T_CPat_1 
                 sem_CPat_Int_1  =
                     (\ _lhsIgIniq
                        _lhsIropts ->
                          (case (_lhsIgIniq) of
                           { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                           (case ((let sem_CPat_Int_2 :: T_CPat_2 
                                       sem_CPat_Int_2  =
                                           (\ _lhsIaRenMp
                                              _lhsIlev
                                              _lhsImoduleNm ->
                                                (case (CPat_Int int_) of
                                                 { _cTrf | _cTrf `seq` (True) ->
                                                 (case (_cTrf) of
                                                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                  (case ([]) of
                                                   { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
                                                   ( _lhsOcTrf,_lhsOfldNmL) }) }) }))
                                   in  sem_CPat_Int_2)) of
                            { ( sem_CPat_2) | True ->
                            ( _lhsOgIniq,sem_CPat_2) }) }))
             in  sem_CPat_Int_1)) of
      { ( sem_CPat_1) | True ->
      ( _lhsOnmL,sem_CPat_1) }) })
sem_CPat_Var :: HsName ->
                T_CPat 
sem_CPat_Var pnm_  =
    (case ([pnm_]) of
     { _lhsOnmL | _lhsOnmL `seq` (True) ->
     (case ((let sem_CPat_Var_1 :: T_CPat_1 
                 sem_CPat_Var_1  =
                     (\ _lhsIgIniq
                        _lhsIropts ->
                          (case (_lhsIgIniq) of
                           { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                           (case ((let sem_CPat_Var_2 :: T_CPat_2 
                                       sem_CPat_Var_2  =
                                           (\ _lhsIaRenMp
                                              _lhsIlev
                                              _lhsImoduleNm ->
                                                (case (CPat_Var (aRenRepl _lhsIaRenMp $ pnm_)) of
                                                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                 (case ([]) of
                                                  { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
                                                  ( _lhsOcTrf,_lhsOfldNmL) }) }))
                                   in  sem_CPat_Var_2)) of
                            { ( sem_CPat_2) | True ->
                            ( _lhsOgIniq,sem_CPat_2) }) }))
             in  sem_CPat_Var_1)) of
      { ( sem_CPat_1) | True ->
      ( _lhsOnmL,sem_CPat_1) }) })
-- CPatFld -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         nmL                  : [HsName]
   visit 1:
      inherited attribute:
         ropts                : RenUniqOpts
      chained attribute:
         gIniq                : Int
   visit 2:
      inherited attributes:
         aRenMp               : ARenMp
         lev                  : Int
         moduleNm             : HsName
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
            local fldNm       : _
         visit 1:
            intra fldNm       : _
         visit 2:
            local aRenMp      : _
            intra fldNm       : _
-}
-- cata
sem_CPatFld :: CPatFld  ->
               T_CPatFld 
sem_CPatFld (CPatFld_Fld _lbl _offset _bind _fldAnns )  =
    (sem_CPatFld_Fld _lbl (sem_CExpr _offset ) (sem_CBind _bind ) (sem_CBindAnnL _fldAnns ) )
-- semantic domain
type T_CPatFld  = ( ([HsName]),T_CPatFld_1 )
type T_CPatFld_1  = Int ->
                    RenUniqOpts ->
                    ( Int,T_CPatFld_2 )
type T_CPatFld_2  = ARenMp ->
                    Int ->
                    HsName ->
                    ( CPatFld ,([HsName]))
sem_CPatFld_Fld :: HsName ->
                   T_CExpr  ->
                   T_CBind  ->
                   T_CBindAnnL  ->
                   T_CPatFld 
sem_CPatFld_Fld lbl_ offset_ bind_ fldAnns_  =
    (case ([]) of
     { _bindOprotectedBindingNames | _bindOprotectedBindingNames `seq` (True) ->
     (case (bind_ _bindOprotectedBindingNames ) of
      { ( _bindInm,_bindInmL,bind_1) | True ->
          (case (_bindInm) of
           { _fldNm | _fldNm `seq` (True) ->
           (case ([_fldNm]) of
            { _lhsOnmL | _lhsOnmL `seq` (True) ->
            (case ((let sem_CPatFld_Fld_1 :: T_CPatFld_1 
                        sem_CPatFld_Fld_1  =
                            (\ _lhsIgIniq
                               _lhsIropts ->
                                 (case (_lhsIropts) of
                                  { _bindOropts | _bindOropts `seq` (True) ->
                                  (case (_lhsIropts) of
                                   { _offsetOropts | _offsetOropts `seq` (True) ->
                                   (case (_lhsIgIniq) of
                                    { _offsetOgIniq | _offsetOgIniq `seq` (True) ->
                                    (case ([]) of
                                     { _offsetOprotectedBindingNames | _offsetOprotectedBindingNames `seq` (True) ->
                                     (case (offset_ _offsetOgIniq _offsetOprotectedBindingNames _offsetOropts ) of
                                      { ( _offsetIgIniq,_offsetIisLamBody,offset_1) | True ->
                                          (case (_offsetIgIniq) of
                                           { _bindOgIniq | _bindOgIniq `seq` (True) ->
                                           (case (bind_1 _bindOgIniq _bindOropts ) of
                                            { ( _bindIgIniq,bind_2) | True ->
                                                (case (_bindIgIniq) of
                                                 { _fldAnnsOgIniq | _fldAnnsOgIniq `seq` (True) ->
                                                 (case (fldAnns_ _fldAnnsOgIniq ) of
                                                  { ( _fldAnnsIgIniq,fldAnns_1) | True ->
                                                      (case (_fldAnnsIgIniq) of
                                                       { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                                                       (case ((let sem_CPatFld_Fld_2 :: T_CPatFld_2 
                                                                   sem_CPatFld_Fld_2  =
                                                                       (\ _lhsIaRenMp
                                                                          _lhsIlev
                                                                          _lhsImoduleNm ->
                                                                            (case (_lhsImoduleNm) of
                                                                             { _offsetOmoduleNm | _offsetOmoduleNm `seq` (True) ->
                                                                             (case (_lhsIlev) of
                                                                              { _offsetOlev | _offsetOlev `seq` (True) ->
                                                                              (case (_lhsIaRenMp) of
                                                                               { _offsetOaccumARenMp | _offsetOaccumARenMp `seq` (True) ->
                                                                               (case (offset_1 _offsetOaccumARenMp _offsetOlev _offsetOmoduleNm ) of
                                                                                { ( _offsetIaccumARenMp,offset_2) | True ->
                                                                                    (case (_offsetIaccumARenMp) of
                                                                                     { _aRenMp | _aRenMp `seq` (True) ->
                                                                                     (case (_aRenMp) of
                                                                                      { _offsetOaRenMp | _offsetOaRenMp `seq` (True) ->
                                                                                      (case (_lhsIropts) of
                                                                                       { _fldAnnsOropts | _fldAnnsOropts `seq` (True) ->
                                                                                       (case (_lhsImoduleNm) of
                                                                                        { _fldAnnsOmoduleNm | _fldAnnsOmoduleNm `seq` (True) ->
                                                                                        (case (_lhsIlev) of
                                                                                         { _fldAnnsOlev | _fldAnnsOlev `seq` (True) ->
                                                                                         (case (_aRenMp) of
                                                                                          { _fldAnnsOaRenMp | _fldAnnsOaRenMp `seq` (True) ->
                                                                                          (case (fldAnns_1 _fldAnnsOaRenMp _fldAnnsOlev _fldAnnsOmoduleNm _fldAnnsOropts ) of
                                                                                           { ( _fldAnnsIcTrf,_fldAnnsInmL) | True ->
                                                                                               (case (offset_2 _offsetOaRenMp ) of
                                                                                                { ( _offsetIcTrf) | True ->
                                                                                                    (case (CPatFld_Fld lbl_ _offsetIcTrf (acoreBind1Nm1 $ aRenRepl _aRenMp _fldNm) _fldAnnsIcTrf) of
                                                                                                     { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                                                     (case ([_fldNm]) of
                                                                                                      { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
                                                                                                      ( _lhsOcTrf,_lhsOfldNmL) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                                               in  sem_CPatFld_Fld_2)) of
                                                        { ( sem_CPatFld_2) | True ->
                                                        ( _lhsOgIniq,sem_CPatFld_2) }) }) }) }) }) }) }) }) }) }) }))
                    in  sem_CPatFld_Fld_1)) of
             { ( sem_CPatFld_1) | True ->
             ( _lhsOnmL,sem_CPatFld_1) }) }) }) }) })
-- CPatFldL ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         nmL                  : [HsName]
   visit 1:
      inherited attribute:
         ropts                : RenUniqOpts
      chained attribute:
         gIniq                : Int
   visit 2:
      inherited attributes:
         aRenMp               : ARenMp
         lev                  : Int
         moduleNm             : HsName
      synthesized attributes:
         cTrf                 : SELF 
         fldNmL               : [HsName]
   alternatives:
      alternative Cons:
         child hd             : CPatFld 
         child tl             : CPatFldL 
         visit 2:
            local cTrf        : _
      alternative Nil:
         visit 2:
            local cTrf        : _
-}
-- cata
sem_CPatFldL :: CPatFldL  ->
                T_CPatFldL 
sem_CPatFldL list  =
    (Prelude.foldr sem_CPatFldL_Cons sem_CPatFldL_Nil (Prelude.map sem_CPatFld list) )
-- semantic domain
type T_CPatFldL  = ( ([HsName]),T_CPatFldL_1 )
type T_CPatFldL_1  = Int ->
                     RenUniqOpts ->
                     ( Int,T_CPatFldL_2 )
type T_CPatFldL_2  = ARenMp ->
                     Int ->
                     HsName ->
                     ( CPatFldL ,([HsName]))
sem_CPatFldL_Cons :: T_CPatFld  ->
                     T_CPatFldL  ->
                     T_CPatFldL 
sem_CPatFldL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlInmL,tl_1) | True ->
         (case (hd_ ) of
          { ( _hdInmL,hd_1) | True ->
              (case (_hdInmL ++ _tlInmL) of
               { _lhsOnmL | _lhsOnmL `seq` (True) ->
               (case ((let sem_CPatFldL_Cons_1 :: T_CPatFldL_1 
                           sem_CPatFldL_Cons_1  =
                               (\ _lhsIgIniq
                                  _lhsIropts ->
                                    (case (_lhsIropts) of
                                     { _tlOropts | _tlOropts `seq` (True) ->
                                     (case (_lhsIropts) of
                                      { _hdOropts | _hdOropts `seq` (True) ->
                                      (case (_lhsIgIniq) of
                                       { _hdOgIniq | _hdOgIniq `seq` (True) ->
                                       (case (hd_1 _hdOgIniq _hdOropts ) of
                                        { ( _hdIgIniq,hd_2) | True ->
                                            (case (_hdIgIniq) of
                                             { _tlOgIniq | _tlOgIniq `seq` (True) ->
                                             (case (tl_1 _tlOgIniq _tlOropts ) of
                                              { ( _tlIgIniq,tl_2) | True ->
                                                  (case (_tlIgIniq) of
                                                   { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                                                   (case ((let sem_CPatFldL_Cons_2 :: T_CPatFldL_2 
                                                               sem_CPatFldL_Cons_2  =
                                                                   (\ _lhsIaRenMp
                                                                      _lhsIlev
                                                                      _lhsImoduleNm ->
                                                                        (case (_lhsImoduleNm) of
                                                                         { _tlOmoduleNm | _tlOmoduleNm `seq` (True) ->
                                                                         (case (_lhsIlev) of
                                                                          { _tlOlev | _tlOlev `seq` (True) ->
                                                                          (case (_lhsIaRenMp) of
                                                                           { _tlOaRenMp | _tlOaRenMp `seq` (True) ->
                                                                           (case (_lhsImoduleNm) of
                                                                            { _hdOmoduleNm | _hdOmoduleNm `seq` (True) ->
                                                                            (case (_lhsIlev) of
                                                                             { _hdOlev | _hdOlev `seq` (True) ->
                                                                             (case (_lhsIaRenMp) of
                                                                              { _hdOaRenMp | _hdOaRenMp `seq` (True) ->
                                                                              (case (tl_2 _tlOaRenMp _tlOlev _tlOmoduleNm ) of
                                                                               { ( _tlIcTrf,_tlIfldNmL) | True ->
                                                                                   (case (hd_2 _hdOaRenMp _hdOlev _hdOmoduleNm ) of
                                                                                    { ( _hdIcTrf,_hdIfldNmL) | True ->
                                                                                        (case ((:) _hdIcTrf _tlIcTrf) of
                                                                                         { _cTrf | _cTrf `seq` (True) ->
                                                                                         (case (_cTrf) of
                                                                                          { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                                          (case (_hdIfldNmL ++ _tlIfldNmL) of
                                                                                           { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
                                                                                           ( _lhsOcTrf,_lhsOfldNmL) }) }) }) }) }) }) }) }) }) }) }))
                                                           in  sem_CPatFldL_Cons_2)) of
                                                    { ( sem_CPatFldL_2) | True ->
                                                    ( _lhsOgIniq,sem_CPatFldL_2) }) }) }) }) }) }) }) }))
                       in  sem_CPatFldL_Cons_1)) of
                { ( sem_CPatFldL_1) | True ->
                ( _lhsOnmL,sem_CPatFldL_1) }) }) }) })
sem_CPatFldL_Nil :: T_CPatFldL 
sem_CPatFldL_Nil  =
    (case ([]) of
     { _lhsOnmL | _lhsOnmL `seq` (True) ->
     (case ((let sem_CPatFldL_Nil_1 :: T_CPatFldL_1 
                 sem_CPatFldL_Nil_1  =
                     (\ _lhsIgIniq
                        _lhsIropts ->
                          (case (_lhsIgIniq) of
                           { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                           (case ((let sem_CPatFldL_Nil_2 :: T_CPatFldL_2 
                                       sem_CPatFldL_Nil_2  =
                                           (\ _lhsIaRenMp
                                              _lhsIlev
                                              _lhsImoduleNm ->
                                                (case ([]) of
                                                 { _cTrf | _cTrf `seq` (True) ->
                                                 (case (_cTrf) of
                                                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                  (case ([]) of
                                                   { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
                                                   ( _lhsOcTrf,_lhsOfldNmL) }) }) }))
                                   in  sem_CPatFldL_Nil_2)) of
                            { ( sem_CPatFldL_2) | True ->
                            ( _lhsOgIniq,sem_CPatFldL_2) }) }))
             in  sem_CPatFldL_Nil_1)) of
      { ( sem_CPatFldL_1) | True ->
      ( _lhsOnmL,sem_CPatFldL_1) }) })
-- CPatRest ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         nmL                  : [HsName]
   visit 1:
      chained attribute:
         gIniq                : Int
   visit 2:
      inherited attributes:
         aRenMp               : ARenMp
         lev                  : Int
         moduleNm             : HsName
         ropts                : RenUniqOpts
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Empty:
         visit 2:
            local cTrf        : _
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
type T_CPatRest  = ( ([HsName]),T_CPatRest_1 )
type T_CPatRest_1  = Int ->
                     ( Int,T_CPatRest_2 )
type T_CPatRest_2  = ARenMp ->
                     Int ->
                     HsName ->
                     RenUniqOpts ->
                     ( CPatRest )
sem_CPatRest_Empty :: T_CPatRest 
sem_CPatRest_Empty  =
    (case ([]) of
     { _lhsOnmL | _lhsOnmL `seq` (True) ->
     (case ((let sem_CPatRest_Empty_1 :: T_CPatRest_1 
                 sem_CPatRest_Empty_1  =
                     (\ _lhsIgIniq ->
                          (case (_lhsIgIniq) of
                           { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                           (case ((let sem_CPatRest_Empty_2 :: T_CPatRest_2 
                                       sem_CPatRest_Empty_2  =
                                           (\ _lhsIaRenMp
                                              _lhsIlev
                                              _lhsImoduleNm
                                              _lhsIropts ->
                                                (case (CPatRest_Empty) of
                                                 { _cTrf | _cTrf `seq` (True) ->
                                                 (case (_cTrf) of
                                                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                  ( _lhsOcTrf) }) }))
                                   in  sem_CPatRest_Empty_2)) of
                            { ( sem_CPatRest_2) | True ->
                            ( _lhsOgIniq,sem_CPatRest_2) }) }))
             in  sem_CPatRest_Empty_1)) of
      { ( sem_CPatRest_1) | True ->
      ( _lhsOnmL,sem_CPatRest_1) }) })
sem_CPatRest_Var :: HsName ->
                    T_CPatRest 
sem_CPatRest_Var nm_  =
    (case ([nm_]) of
     { _lhsOnmL | _lhsOnmL `seq` (True) ->
     (case ((let sem_CPatRest_Var_1 :: T_CPatRest_1 
                 sem_CPatRest_Var_1  =
                     (\ _lhsIgIniq ->
                          (case (_lhsIgIniq) of
                           { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                           (case ((let sem_CPatRest_Var_2 :: T_CPatRest_2 
                                       sem_CPatRest_Var_2  =
                                           (\ _lhsIaRenMp
                                              _lhsIlev
                                              _lhsImoduleNm
                                              _lhsIropts ->
                                                (case (CPatRest_Var (aRenRepl _lhsIaRenMp nm_)) of
                                                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                 ( _lhsOcTrf) }))
                                   in  sem_CPatRest_Var_2)) of
                            { ( sem_CPatRest_2) | True ->
                            ( _lhsOgIniq,sem_CPatRest_2) }) }))
             in  sem_CPatRest_Var_1)) of
      { ( sem_CPatRest_1) | True ->
      ( _lhsOnmL,sem_CPatRest_1) }) })
-- CodeAGItf ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         gIniq                : Int
         ropts                : RenUniqOpts
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
type T_CodeAGItf  = Int ->
                    RenUniqOpts ->
                    ( CModule )
data Inh_CodeAGItf  = Inh_CodeAGItf {gIniq_Inh_CodeAGItf :: !(Int),ropts_Inh_CodeAGItf :: !(RenUniqOpts)}
data Syn_CodeAGItf  = Syn_CodeAGItf {cTrf_Syn_CodeAGItf :: !(CModule )}
wrap_CodeAGItf :: T_CodeAGItf  ->
                  Inh_CodeAGItf  ->
                  Syn_CodeAGItf 
wrap_CodeAGItf sem (Inh_CodeAGItf _lhsIgIniq _lhsIropts )  =
    (let ( _lhsOcTrf) | True = sem _lhsIgIniq _lhsIropts 
     in  (Syn_CodeAGItf _lhsOcTrf ))
sem_CodeAGItf_AGItf :: T_CModule  ->
                       T_CodeAGItf 
sem_CodeAGItf_AGItf module_  =
    (\ _lhsIgIniq
       _lhsIropts ->
         (case (_lhsIropts) of
          { _moduleOropts | _moduleOropts `seq` (True) ->
          (case (_lhsIgIniq) of
           { _moduleOgIniq | _moduleOgIniq `seq` (True) ->
           (case (cLevModule) of
            { _moduleOlev | _moduleOlev `seq` (True) ->
            (case (emptyARenMp) of
             { _moduleOaRenMp | _moduleOaRenMp `seq` (True) ->
             (case (module_ _moduleOaRenMp _moduleOgIniq _moduleOlev _moduleOropts ) of
              { ( _moduleIcTrf,_moduleIgIniq) | True ->
                  (case (_moduleIcTrf) of
                   { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                   ( _lhsOcTrf) }) }) }) }) }) }))
-- MbCExpr -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         aRenMp               : ARenMp
         lev                  : Int
         moduleNm             : HsName
         protectedBindingNames : [HsName]
         ropts                : RenUniqOpts
      chained attribute:
         gIniq                : Int
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Just:
         child just           : CExpr 
         visit 0:
            local aRenMp      : _
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
type T_MbCExpr  = ARenMp ->
                  Int ->
                  Int ->
                  HsName ->
                  ([HsName]) ->
                  RenUniqOpts ->
                  ( MbCExpr ,Int)
sem_MbCExpr_Just :: T_CExpr  ->
                    T_MbCExpr 
sem_MbCExpr_Just just_  =
    (\ _lhsIaRenMp
       _lhsIgIniq
       _lhsIlev
       _lhsImoduleNm
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (_lhsIropts) of
          { _justOropts | _justOropts `seq` (True) ->
          (case (_lhsIprotectedBindingNames) of
           { _justOprotectedBindingNames | _justOprotectedBindingNames `seq` (True) ->
           (case (_lhsImoduleNm) of
            { _justOmoduleNm | _justOmoduleNm `seq` (True) ->
            (case (_lhsIlev) of
             { _justOlev | _justOlev `seq` (True) ->
             (case (_lhsIgIniq) of
              { _justOgIniq | _justOgIniq `seq` (True) ->
              (case (_lhsIaRenMp) of
               { _justOaccumARenMp | _justOaccumARenMp `seq` (True) ->
               (case (just_ _justOgIniq _justOprotectedBindingNames _justOropts ) of
                { ( _justIgIniq,_justIisLamBody,just_1) | True ->
                    (case (just_1 _justOaccumARenMp _justOlev _justOmoduleNm ) of
                     { ( _justIaccumARenMp,just_2) | True ->
                         (case (_justIaccumARenMp) of
                          { _aRenMp | _aRenMp `seq` (True) ->
                          (case (_aRenMp) of
                           { _justOaRenMp | _justOaRenMp `seq` (True) ->
                           (case (just_2 _justOaRenMp ) of
                            { ( _justIcTrf) | True ->
                                (case (Just _justIcTrf) of
                                 { _cTrf | _cTrf `seq` (True) ->
                                 (case (_cTrf) of
                                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                  (case (_justIgIniq) of
                                   { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
                                   ( _lhsOcTrf,_lhsOgIniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_MbCExpr_Nothing :: T_MbCExpr 
sem_MbCExpr_Nothing  =
    (\ _lhsIaRenMp
       _lhsIgIniq
       _lhsIlev
       _lhsImoduleNm
       _lhsIprotectedBindingNames
       _lhsIropts ->
         (case (Nothing) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (_lhsIgIniq) of
            { _lhsOgIniq | _lhsOgIniq `seq` (True) ->
            ( _lhsOcTrf,_lhsOgIniq) }) }) }))
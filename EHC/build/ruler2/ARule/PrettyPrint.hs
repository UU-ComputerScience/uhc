

-- UUAGC 0.9.39.1 (build/ruler2/ARule/PrettyPrint.ag)
module ARule.PrettyPrint(ppARule, ppExpr, ppECGam) where

import qualified Data.Map as Map
import EH.Util.Utils
import EH.Util.Pretty
import Common
import LaTeXFmtUtils
import Expr.Expr
import ARule.ARule
import ECnstrGam
import Config (cfgStrSel)











ppARule :: ARule -> PP_Doc
ppARule r
  = pp_Syn_AGARuleItf r2
  where r1 = sem_AGARuleItf (AGARuleItf_AGItf r)
        r2 = wrap_AGARuleItf r1
                (Inh_AGARuleItf)

ppExpr :: Expr -> PP_Doc
ppExpr e
  = pp_Syn_AGExprItf r2
  where r1 = sem_AGExprItf (AGExprItf_AGItf e)
        r2 = wrap_AGExprItf r1
                (Inh_AGExprItf)

ppECGam :: ECnstrGam -> PP_Doc
ppECGam g = vlist [ pp (Expr_Cnstr n v) | (n,v) <- gamAssocs g]

instance Show ARule where
  show _ = "ARule"

instance PP ARule where
  pp = ppARule

instance Show Expr where
  show _ = "Expr"

instance PP Expr where
  pp = ppExpr

-- AEqn --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         mbDstWd              : Maybe (Int,Int)
         mbPrevNdStr          : Maybe String
      synthesized attributes:
         mxDstAtWd            : Int
         mxDstNdWd            : Int
         ndStr                : String
         pp                   : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Eqn:
         child dest           : AEqnDest 
         child val            : AExpr 
         visit 0:
            local self        : _
      alternative Err:
         child expr           : Expr 
         visit 0:
            local self        : _
-}
-- cata
sem_AEqn :: AEqn  ->
            T_AEqn 
sem_AEqn (AEqn_Eqn _dest _val )  =
    (sem_AEqn_Eqn (sem_AEqnDest _dest ) (sem_AExpr _val ) )
sem_AEqn (AEqn_Err _expr )  =
    (sem_AEqn_Err (sem_Expr _expr ) )
-- semantic domain
type T_AEqn  = (Maybe (Int,Int)) ->
               (Maybe String) ->
               ( Int,Int,String,PP_Doc,AEqn )
sem_AEqn_Eqn :: T_AEqnDest  ->
                T_AExpr  ->
                T_AEqn 
sem_AEqn_Eqn dest_ val_  =
    (\ _lhsImbDstWd
       _lhsImbPrevNdStr ->
         (let _lhsOpp :: PP_Doc
              _lhsOmxDstAtWd :: Int
              _lhsOmxDstNdWd :: Int
              _lhsOself :: AEqn 
              _lhsOndStr :: String
              _destOmbDstWd :: (Maybe (Int,Int))
              _destOmbPrevNdStr :: (Maybe String)
              _valOmbDstWd :: (Maybe (Int,Int))
              _destIdstWd :: Int
              _destIisComposite :: Bool
              _destImxDstAtWd :: Int
              _destImxDstNdWd :: Int
              _destIndStr :: String
              _destIpp :: PP_Doc
              _destIself :: AEqnDest 
              _valImxDstAtWd :: Int
              _valImxDstNdWd :: Int
              _valIpp :: PP_Doc
              _valIself :: AExpr 
              -- "build/ruler2/ARule/PrettyPrintAG.ag"(line 13, column 21)
              _lhsOpp =
                  let (w1,w2) = fromJust _lhsImbDstWd
                      w = w1 + w2 + atDstFillLen
                      cmb l r = if _destIisComposite
                                then if _destIdstWd > w - atLhs2texDist
                                     then l >-< strWhite w >|< r
                                     else l >|< strWhite (w - _destIdstWd) >|< r
                                else l >|< r
                  in  cmb _destIpp ("=" >|< strWhite atLhs2texDist >|< _valIpp)
              -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 44, column 77)
              _lhsOmxDstAtWd =
                  _destImxDstAtWd `max` _valImxDstAtWd
              -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 44, column 77)
              _lhsOmxDstNdWd =
                  _destImxDstNdWd `max` _valImxDstNdWd
              -- self rule
              _self =
                  AEqn_Eqn _destIself _valIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOndStr =
                  _destIndStr
              -- copy rule (down)
              _destOmbDstWd =
                  _lhsImbDstWd
              -- copy rule (down)
              _destOmbPrevNdStr =
                  _lhsImbPrevNdStr
              -- copy rule (down)
              _valOmbDstWd =
                  _lhsImbDstWd
              ( _destIdstWd,_destIisComposite,_destImxDstAtWd,_destImxDstNdWd,_destIndStr,_destIpp,_destIself) =
                  dest_ _destOmbDstWd _destOmbPrevNdStr 
              ( _valImxDstAtWd,_valImxDstNdWd,_valIpp,_valIself) =
                  val_ _valOmbDstWd 
          in  ( _lhsOmxDstAtWd,_lhsOmxDstNdWd,_lhsOndStr,_lhsOpp,_lhsOself)))
sem_AEqn_Err :: T_Expr  ->
                T_AEqn 
sem_AEqn_Err expr_  =
    (\ _lhsImbDstWd
       _lhsImbPrevNdStr ->
         (let _lhsOpp :: PP_Doc
              _lhsOndStr :: String
              _lhsOmxDstAtWd :: Int
              _lhsOmxDstNdWd :: Int
              _lhsOself :: AEqn 
              _exprIpp :: PP_Doc
              _exprIself :: Expr 
              -- "build/ruler2/ARule/PrettyPrintAG.ag"(line 21, column 21)
              _lhsOpp =
                  "ERR" >#< _exprIpp
              -- "build/ruler2/ARule/PrettyPrintAG.ag"(line 78, column 21)
              _lhsOndStr =
                  "??"
              -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 44, column 77)
              _lhsOmxDstAtWd =
                  0
              -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 44, column 77)
              _lhsOmxDstNdWd =
                  0
              -- self rule
              _self =
                  AEqn_Err _exprIself
              -- self rule
              _lhsOself =
                  _self
              ( _exprIpp,_exprIself) =
                  expr_ 
          in  ( _lhsOmxDstAtWd,_lhsOmxDstNdWd,_lhsOndStr,_lhsOpp,_lhsOself)))
-- AEqnDest ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         mbDstWd              : Maybe (Int,Int)
         mbPrevNdStr          : Maybe String
      synthesized attributes:
         dstWd                : Int
         isComposite          : Bool
         mxDstAtWd            : Int
         mxDstNdWd            : Int
         ndStr                : String
         pp                   : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Many:
         child dests          : AEqnDests 
         visit 0:
            local mbPrevNdStr : _
            local self        : _
      alternative One:
         child anm            : ANm 
         visit 0:
            local self        : _
-}
-- cata
sem_AEqnDest :: AEqnDest  ->
                T_AEqnDest 
sem_AEqnDest (AEqnDest_Many _dests )  =
    (sem_AEqnDest_Many (sem_AEqnDests _dests ) )
sem_AEqnDest (AEqnDest_One _anm )  =
    (sem_AEqnDest_One (sem_ANm _anm ) )
-- semantic domain
type T_AEqnDest  = (Maybe (Int,Int)) ->
                   (Maybe String) ->
                   ( Int,Bool,Int,Int,String,PP_Doc,AEqnDest )
sem_AEqnDest_Many :: T_AEqnDests  ->
                     T_AEqnDest 
sem_AEqnDest_Many dests_  =
    (\ _lhsImbDstWd
       _lhsImbPrevNdStr ->
         (let _lhsOpp :: PP_Doc
              _destsOmbDstWd :: (Maybe (Int,Int))
              _lhsOdstWd :: Int
              _lhsOndStr :: String
              _lhsOisComposite :: Bool
              _lhsOmxDstAtWd :: Int
              _lhsOmxDstNdWd :: Int
              _lhsOself :: AEqnDest 
              _destsIdstWdL :: ([Int])
              _destsImxDstAtWd :: Int
              _destsImxDstNdWd :: Int
              _destsIpp :: PP_Doc
              _destsIppL :: ([PP_Doc])
              _destsIself :: AEqnDests 
              -- "build/ruler2/ARule/PrettyPrintAG.ag"(line 24, column 21)
              _lhsOpp =
                  ppParensCommas _destsIppL
              -- "build/ruler2/ARule/PrettyPrintAG.ag"(line 50, column 21)
              _destsOmbDstWd =
                  Nothing
              -- "build/ruler2/ARule/PrettyPrintAG.ag"(line 60, column 21)
              _lhsOdstWd =
                  sum _destsIdstWdL + length _destsIdstWdL + 1
              -- "build/ruler2/ARule/PrettyPrintAG.ag"(line 75, column 21)
              _lhsOndStr =
                  "??"
              -- "build/ruler2/ARule/PrettyPrintAG.ag"(line 90, column 21)
              _mbPrevNdStr =
                  Nothing
              -- "build/ruler2/ARule/PrettyPrintAG.ag"(line 99, column 21)
              _lhsOisComposite =
                  True
              -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 44, column 77)
              _lhsOmxDstAtWd =
                  _destsImxDstAtWd
              -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 44, column 77)
              _lhsOmxDstNdWd =
                  _destsImxDstNdWd
              -- self rule
              _self =
                  AEqnDest_Many _destsIself
              -- self rule
              _lhsOself =
                  _self
              ( _destsIdstWdL,_destsImxDstAtWd,_destsImxDstNdWd,_destsIpp,_destsIppL,_destsIself) =
                  dests_ _destsOmbDstWd 
          in  ( _lhsOdstWd,_lhsOisComposite,_lhsOmxDstAtWd,_lhsOmxDstNdWd,_lhsOndStr,_lhsOpp,_lhsOself)))
sem_AEqnDest_One :: T_ANm  ->
                    T_AEqnDest 
sem_AEqnDest_One anm_  =
    (\ _lhsImbDstWd
       _lhsImbPrevNdStr ->
         (let _anmOisDest :: Bool
              _lhsOdstWd :: Int
              _lhsOisComposite :: Bool
              _lhsOmxDstAtWd :: Int
              _lhsOmxDstNdWd :: Int
              _lhsOpp :: PP_Doc
              _lhsOself :: AEqnDest 
              _lhsOndStr :: String
              _anmOmbDstWd :: (Maybe (Int,Int))
              _anmOmbPrevNdStr :: (Maybe String)
              _anmImxDstAtWd :: Int
              _anmImxDstNdWd :: Int
              _anmIndStr :: String
              _anmIpp :: PP_Doc
              _anmIself :: ANm 
              -- "build/ruler2/ARule/PrettyPrintAG.ag"(line 38, column 21)
              _anmOisDest =
                  True
              -- "build/ruler2/ARule/PrettyPrintAG.ag"(line 61, column 21)
              _lhsOdstWd =
                  _anmImxDstNdWd + _anmImxDstAtWd + (if _anmImxDstNdWd > 0 then 1 else 0)
              -- "build/ruler2/ARule/PrettyPrintAG.ag"(line 100, column 21)
              _lhsOisComposite =
                  False
              -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 44, column 77)
              _lhsOmxDstAtWd =
                  _anmImxDstAtWd
              -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 44, column 77)
              _lhsOmxDstNdWd =
                  _anmImxDstNdWd
              -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 6, column 35)
              _lhsOpp =
                  _anmIpp
              -- self rule
              _self =
                  AEqnDest_One _anmIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOndStr =
                  _anmIndStr
              -- copy rule (down)
              _anmOmbDstWd =
                  _lhsImbDstWd
              -- copy rule (down)
              _anmOmbPrevNdStr =
                  _lhsImbPrevNdStr
              ( _anmImxDstAtWd,_anmImxDstNdWd,_anmIndStr,_anmIpp,_anmIself) =
                  anm_ _anmOisDest _anmOmbDstWd _anmOmbPrevNdStr 
          in  ( _lhsOdstWd,_lhsOisComposite,_lhsOmxDstAtWd,_lhsOmxDstNdWd,_lhsOndStr,_lhsOpp,_lhsOself)))
-- AEqnDests ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         mbDstWd              : Maybe (Int,Int)
      synthesized attributes:
         dstWdL               : [Int]
         mxDstAtWd            : Int
         mxDstNdWd            : Int
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
         self                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : AEqnDest 
         child tl             : AEqnDests 
         visit 0:
            local mbPrevNdStr : _
            local self        : _
      alternative Nil:
         visit 0:
            local self        : _
-}
-- cata
sem_AEqnDests :: AEqnDests  ->
                 T_AEqnDests 
sem_AEqnDests list  =
    (Prelude.foldr sem_AEqnDests_Cons sem_AEqnDests_Nil (Prelude.map sem_AEqnDest list) )
-- semantic domain
type T_AEqnDests  = (Maybe (Int,Int)) ->
                    ( ([Int]),Int,Int,PP_Doc,([PP_Doc]),AEqnDests )
sem_AEqnDests_Cons :: T_AEqnDest  ->
                      T_AEqnDests  ->
                      T_AEqnDests 
sem_AEqnDests_Cons hd_ tl_  =
    (\ _lhsImbDstWd ->
         (let _lhsOppL :: ([PP_Doc])
              _lhsOdstWdL :: ([Int])
              _lhsOmxDstAtWd :: Int
              _lhsOmxDstNdWd :: Int
              _lhsOpp :: PP_Doc
              _lhsOself :: AEqnDests 
              _hdOmbDstWd :: (Maybe (Int,Int))
              _hdOmbPrevNdStr :: (Maybe String)
              _tlOmbDstWd :: (Maybe (Int,Int))
              _hdIdstWd :: Int
              _hdIisComposite :: Bool
              _hdImxDstAtWd :: Int
              _hdImxDstNdWd :: Int
              _hdIndStr :: String
              _hdIpp :: PP_Doc
              _hdIself :: AEqnDest 
              _tlIdstWdL :: ([Int])
              _tlImxDstAtWd :: Int
              _tlImxDstNdWd :: Int
              _tlIpp :: PP_Doc
              _tlIppL :: ([PP_Doc])
              _tlIself :: AEqnDests 
              -- "build/ruler2/ARule/PrettyPrintAG.ag"(line 30, column 21)
              _lhsOppL =
                  _hdIpp : _tlIppL
              -- "build/ruler2/ARule/PrettyPrintAG.ag"(line 64, column 21)
              _lhsOdstWdL =
                  _hdIdstWd : _tlIdstWdL
              -- "build/ruler2/ARule/PrettyPrintAG.ag"(line 87, column 21)
              _mbPrevNdStr =
                  Nothing
              -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 44, column 77)
              _lhsOmxDstAtWd =
                  _hdImxDstAtWd `max` _tlImxDstAtWd
              -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 44, column 77)
              _lhsOmxDstNdWd =
                  _hdImxDstNdWd `max` _tlImxDstNdWd
              -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 6, column 35)
              _lhsOpp =
                  _hdIpp >-< _tlIpp
              -- self rule
              _self =
                  (:) _hdIself _tlIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _hdOmbDstWd =
                  _lhsImbDstWd
              -- copy rule (from local)
              _hdOmbPrevNdStr =
                  _mbPrevNdStr
              -- copy rule (down)
              _tlOmbDstWd =
                  _lhsImbDstWd
              ( _hdIdstWd,_hdIisComposite,_hdImxDstAtWd,_hdImxDstNdWd,_hdIndStr,_hdIpp,_hdIself) =
                  hd_ _hdOmbDstWd _hdOmbPrevNdStr 
              ( _tlIdstWdL,_tlImxDstAtWd,_tlImxDstNdWd,_tlIpp,_tlIppL,_tlIself) =
                  tl_ _tlOmbDstWd 
          in  ( _lhsOdstWdL,_lhsOmxDstAtWd,_lhsOmxDstNdWd,_lhsOpp,_lhsOppL,_lhsOself)))
sem_AEqnDests_Nil :: T_AEqnDests 
sem_AEqnDests_Nil  =
    (\ _lhsImbDstWd ->
         (let _lhsOppL :: ([PP_Doc])
              _lhsOdstWdL :: ([Int])
              _lhsOmxDstAtWd :: Int
              _lhsOmxDstNdWd :: Int
              _lhsOpp :: PP_Doc
              _lhsOself :: AEqnDests 
              -- "build/ruler2/ARule/PrettyPrintAG.ag"(line 31, column 21)
              _lhsOppL =
                  []
              -- "build/ruler2/ARule/PrettyPrintAG.ag"(line 65, column 21)
              _lhsOdstWdL =
                  []
              -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 44, column 77)
              _lhsOmxDstAtWd =
                  0
              -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 44, column 77)
              _lhsOmxDstNdWd =
                  0
              -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 6, column 35)
              _lhsOpp =
                  empty
              -- self rule
              _self =
                  []
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOdstWdL,_lhsOmxDstAtWd,_lhsOmxDstNdWd,_lhsOpp,_lhsOppL,_lhsOself)))
-- AEqns -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         mbDstWd              : Maybe (Int,Int)
         mbPrevNdStr          : Maybe String
      synthesized attributes:
         mxDstAtWd            : Int
         mxDstNdWd            : Int
         pp                   : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : AEqn 
         child tl             : AEqns 
         visit 0:
            local self        : _
      alternative Nil:
         visit 0:
            local self        : _
-}
-- cata
sem_AEqns :: AEqns  ->
             T_AEqns 
sem_AEqns list  =
    (Prelude.foldr sem_AEqns_Cons sem_AEqns_Nil (Prelude.map sem_AEqn list) )
-- semantic domain
type T_AEqns  = (Maybe (Int,Int)) ->
                (Maybe String) ->
                ( Int,Int,PP_Doc,AEqns )
sem_AEqns_Cons :: T_AEqn  ->
                  T_AEqns  ->
                  T_AEqns 
sem_AEqns_Cons hd_ tl_  =
    (\ _lhsImbDstWd
       _lhsImbPrevNdStr ->
         (let _tlOmbPrevNdStr :: (Maybe String)
              _lhsOmxDstAtWd :: Int
              _lhsOmxDstNdWd :: Int
              _lhsOpp :: PP_Doc
              _lhsOself :: AEqns 
              _hdOmbDstWd :: (Maybe (Int,Int))
              _hdOmbPrevNdStr :: (Maybe String)
              _tlOmbDstWd :: (Maybe (Int,Int))
              _hdImxDstAtWd :: Int
              _hdImxDstNdWd :: Int
              _hdIndStr :: String
              _hdIpp :: PP_Doc
              _hdIself :: AEqn 
              _tlImxDstAtWd :: Int
              _tlImxDstNdWd :: Int
              _tlIpp :: PP_Doc
              _tlIself :: AEqns 
              -- "build/ruler2/ARule/PrettyPrintAG.ag"(line 81, column 21)
              _tlOmbPrevNdStr =
                  fmap (const _hdIndStr) _lhsImbDstWd
              -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 44, column 77)
              _lhsOmxDstAtWd =
                  _hdImxDstAtWd `max` _tlImxDstAtWd
              -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 44, column 77)
              _lhsOmxDstNdWd =
                  _hdImxDstNdWd `max` _tlImxDstNdWd
              -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 6, column 35)
              _lhsOpp =
                  _hdIpp >-< _tlIpp
              -- self rule
              _self =
                  (:) _hdIself _tlIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _hdOmbDstWd =
                  _lhsImbDstWd
              -- copy rule (down)
              _hdOmbPrevNdStr =
                  _lhsImbPrevNdStr
              -- copy rule (down)
              _tlOmbDstWd =
                  _lhsImbDstWd
              ( _hdImxDstAtWd,_hdImxDstNdWd,_hdIndStr,_hdIpp,_hdIself) =
                  hd_ _hdOmbDstWd _hdOmbPrevNdStr 
              ( _tlImxDstAtWd,_tlImxDstNdWd,_tlIpp,_tlIself) =
                  tl_ _tlOmbDstWd _tlOmbPrevNdStr 
          in  ( _lhsOmxDstAtWd,_lhsOmxDstNdWd,_lhsOpp,_lhsOself)))
sem_AEqns_Nil :: T_AEqns 
sem_AEqns_Nil  =
    (\ _lhsImbDstWd
       _lhsImbPrevNdStr ->
         (let _lhsOmxDstAtWd :: Int
              _lhsOmxDstNdWd :: Int
              _lhsOpp :: PP_Doc
              _lhsOself :: AEqns 
              -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 44, column 77)
              _lhsOmxDstAtWd =
                  0
              -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 44, column 77)
              _lhsOmxDstNdWd =
                  0
              -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 6, column 35)
              _lhsOpp =
                  empty
              -- self rule
              _self =
                  []
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOmxDstAtWd,_lhsOmxDstNdWd,_lhsOpp,_lhsOself)))
-- AExpr -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         mbDstWd              : Maybe (Int,Int)
      synthesized attributes:
         mxDstAtWd            : Int
         mxDstNdWd            : Int
         pp                   : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Expr:
         child expr           : Expr 
         visit 0:
            local self        : _
-}
-- cata
sem_AExpr :: AExpr  ->
             T_AExpr 
sem_AExpr (AExpr_Expr _expr )  =
    (sem_AExpr_Expr (sem_Expr _expr ) )
-- semantic domain
type T_AExpr  = (Maybe (Int,Int)) ->
                ( Int,Int,PP_Doc,AExpr )
sem_AExpr_Expr :: T_Expr  ->
                  T_AExpr 
sem_AExpr_Expr expr_  =
    (\ _lhsImbDstWd ->
         (let _lhsOpp :: PP_Doc
              _lhsOmxDstAtWd :: Int
              _lhsOmxDstNdWd :: Int
              _lhsOself :: AExpr 
              _exprIpp :: PP_Doc
              _exprIself :: Expr 
              -- "build/ruler2/ARule/PrettyPrintAG.ag"(line 27, column 21)
              _lhsOpp =
                  exprNeedPar ParCtxtOther nmUnk _exprIself _exprIpp
              -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 44, column 77)
              _lhsOmxDstAtWd =
                  0
              -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 44, column 77)
              _lhsOmxDstNdWd =
                  0
              -- self rule
              _self =
                  AExpr_Expr _exprIself
              -- self rule
              _lhsOself =
                  _self
              ( _exprIpp,_exprIself) =
                  expr_ 
          in  ( _lhsOmxDstAtWd,_lhsOmxDstNdWd,_lhsOpp,_lhsOself)))
-- AGARuleItf --------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         pp                   : PP_Doc
         self                 : ARule 
   alternatives:
      alternative AGItf:
         child rule           : ARule 
-}
-- cata
sem_AGARuleItf :: AGARuleItf  ->
                  T_AGARuleItf 
sem_AGARuleItf (AGARuleItf_AGItf _rule )  =
    (sem_AGARuleItf_AGItf (sem_ARule _rule ) )
-- semantic domain
type T_AGARuleItf  = ( PP_Doc,ARule )
data Inh_AGARuleItf  = Inh_AGARuleItf {}
data Syn_AGARuleItf  = Syn_AGARuleItf {pp_Syn_AGARuleItf :: PP_Doc,self_Syn_AGARuleItf :: ARule }
wrap_AGARuleItf :: T_AGARuleItf  ->
                   Inh_AGARuleItf  ->
                   Syn_AGARuleItf 
wrap_AGARuleItf sem (Inh_AGARuleItf )  =
    (let ( _lhsOpp,_lhsOself) = sem 
     in  (Syn_AGARuleItf _lhsOpp _lhsOself ))
sem_AGARuleItf_AGItf :: T_ARule  ->
                        T_AGARuleItf 
sem_AGARuleItf_AGItf rule_  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: ARule 
         _ruleIpp :: PP_Doc
         _ruleIself :: ARule 
         -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 6, column 35)
         _lhsOpp =
             _ruleIpp
         -- copy rule (up)
         _lhsOself =
             _ruleIself
         ( _ruleIpp,_ruleIself) =
             rule_ 
     in  ( _lhsOpp,_lhsOself))
-- AGExprItf ---------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         pp                   : PP_Doc
         self                 : Expr 
   alternatives:
      alternative AGItf:
         child expr           : Expr 
-}
-- cata
sem_AGExprItf :: AGExprItf  ->
                 T_AGExprItf 
sem_AGExprItf (AGExprItf_AGItf _expr )  =
    (sem_AGExprItf_AGItf (sem_Expr _expr ) )
-- semantic domain
type T_AGExprItf  = ( PP_Doc,Expr )
data Inh_AGExprItf  = Inh_AGExprItf {}
data Syn_AGExprItf  = Syn_AGExprItf {pp_Syn_AGExprItf :: PP_Doc,self_Syn_AGExprItf :: Expr }
wrap_AGExprItf :: T_AGExprItf  ->
                  Inh_AGExprItf  ->
                  Syn_AGExprItf 
wrap_AGExprItf sem (Inh_AGExprItf )  =
    (let ( _lhsOpp,_lhsOself) = sem 
     in  (Syn_AGExprItf _lhsOpp _lhsOself ))
sem_AGExprItf_AGItf :: T_Expr  ->
                       T_AGExprItf 
sem_AGExprItf_AGItf expr_  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: Expr 
         _exprIpp :: PP_Doc
         _exprIself :: Expr 
         -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 6, column 33)
         _lhsOpp =
             _exprIpp
         -- copy rule (up)
         _lhsOself =
             _exprIself
         ( _exprIpp,_exprIself) =
             expr_ 
     in  ( _lhsOpp,_lhsOself))
-- ANm ---------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         isDest               : Bool
         mbDstWd              : Maybe (Int,Int)
         mbPrevNdStr          : Maybe String
      synthesized attributes:
         mxDstAtWd            : Int
         mxDstNdWd            : Int
         ndStr                : String
         pp                   : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Fld:
         child nm             : {Nm}
         visit 0:
            local ndStr       : _
            local self        : _
      alternative Lhs:
         child nm             : {Nm}
         child props          : {[AtProp]}
         visit 0:
            local ndStr       : _
            local self        : _
      alternative Loc:
         child nm             : {Nm}
         child props          : {[AtProp]}
         visit 0:
            local ndStr       : _
            local self        : _
      alternative Node:
         child ndNm           : {Nm}
         child nm             : {Nm}
         visit 0:
            local ndStr       : _
            local self        : _
      alternative Wild:
         visit 0:
            local nm          : _
            local ndStr       : _
            local self        : _
-}
-- cata
sem_ANm :: ANm  ->
           T_ANm 
sem_ANm (ANm_Fld _nm )  =
    (sem_ANm_Fld _nm )
sem_ANm (ANm_Lhs _nm _props )  =
    (sem_ANm_Lhs _nm _props )
sem_ANm (ANm_Loc _nm _props )  =
    (sem_ANm_Loc _nm _props )
sem_ANm (ANm_Node _ndNm _nm )  =
    (sem_ANm_Node _ndNm _nm )
sem_ANm (ANm_Wild )  =
    (sem_ANm_Wild )
-- semantic domain
type T_ANm  = Bool ->
              (Maybe (Int,Int)) ->
              (Maybe String) ->
              ( Int,Int,String,PP_Doc,ANm )
sem_ANm_Fld :: Nm ->
               T_ANm 
sem_ANm_Fld nm_  =
    (\ _lhsIisDest
       _lhsImbDstWd
       _lhsImbPrevNdStr ->
         (let _lhsOpp :: PP_Doc
              _lhsOmxDstAtWd :: Int
              _lhsOmxDstNdWd :: Int
              _lhsOself :: ANm 
              _lhsOndStr :: String
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 51, column 21)
              _lhsOpp =
                  ppDest "F" _lhsIisDest _lhsImbDstWd _lhsImbPrevNdStr "??"   _ndStr nm_
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 73, column 21)
              _lhsOmxDstAtWd =
                  length . show $ nm_
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 89, column 21)
              _ndStr =
                  ""
              -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 67, column 64)
              _lhsOmxDstNdWd =
                  0
              -- self rule
              _self =
                  ANm_Fld nm_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (from local)
              _lhsOndStr =
                  _ndStr
          in  ( _lhsOmxDstAtWd,_lhsOmxDstNdWd,_lhsOndStr,_lhsOpp,_lhsOself)))
sem_ANm_Lhs :: Nm ->
               ([AtProp]) ->
               T_ANm 
sem_ANm_Lhs nm_ props_  =
    (\ _lhsIisDest
       _lhsImbDstWd
       _lhsImbPrevNdStr ->
         (let _lhsOpp :: PP_Doc
              _lhsOmxDstNdWd :: Int
              _lhsOmxDstAtWd :: Int
              _lhsOself :: ANm 
              _lhsOndStr :: String
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 50, column 21)
              _lhsOpp =
                  ppDest "P" _lhsIisDest _lhsImbDstWd _lhsImbPrevNdStr _ndStr _ndStr nm_
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 70, column 21)
              _lhsOmxDstNdWd =
                  3
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 73, column 21)
              _lhsOmxDstAtWd =
                  length . show $ nm_
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 87, column 21)
              _ndStr =
                  if AtRetain `elem` props_ then strLoc else strLhs
              -- self rule
              _self =
                  ANm_Lhs nm_ props_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (from local)
              _lhsOndStr =
                  _ndStr
          in  ( _lhsOmxDstAtWd,_lhsOmxDstNdWd,_lhsOndStr,_lhsOpp,_lhsOself)))
sem_ANm_Loc :: Nm ->
               ([AtProp]) ->
               T_ANm 
sem_ANm_Loc nm_ props_  =
    (\ _lhsIisDest
       _lhsImbDstWd
       _lhsImbPrevNdStr ->
         (let _lhsOpp :: PP_Doc
              _lhsOmxDstNdWd :: Int
              _lhsOmxDstAtWd :: Int
              _lhsOself :: ANm 
              _lhsOndStr :: String
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 49, column 21)
              _lhsOpp =
                  ppDest "L" _lhsIisDest _lhsImbDstWd _lhsImbPrevNdStr _ndStr ""     nm_
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 70, column 21)
              _lhsOmxDstNdWd =
                  3
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 73, column 21)
              _lhsOmxDstAtWd =
                  length . show $ nm_
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 86, column 21)
              _ndStr =
                  strLoc
              -- self rule
              _self =
                  ANm_Loc nm_ props_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (from local)
              _lhsOndStr =
                  _ndStr
          in  ( _lhsOmxDstAtWd,_lhsOmxDstNdWd,_lhsOndStr,_lhsOpp,_lhsOself)))
sem_ANm_Node :: Nm ->
                Nm ->
                T_ANm 
sem_ANm_Node ndNm_ nm_  =
    (\ _lhsIisDest
       _lhsImbDstWd
       _lhsImbPrevNdStr ->
         (let _lhsOpp :: PP_Doc
              _lhsOmxDstNdWd :: Int
              _lhsOmxDstAtWd :: Int
              _lhsOself :: ANm 
              _lhsOndStr :: String
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 52, column 21)
              _lhsOpp =
                  ppDest "N" _lhsIisDest _lhsImbDstWd _lhsImbPrevNdStr _ndStr _ndStr nm_
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 72, column 21)
              _lhsOmxDstNdWd =
                  length _ndStr
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 73, column 21)
              _lhsOmxDstAtWd =
                  length . show $ nm_
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 88, column 21)
              _ndStr =
                  show ndNm_
              -- self rule
              _self =
                  ANm_Node ndNm_ nm_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (from local)
              _lhsOndStr =
                  _ndStr
          in  ( _lhsOmxDstAtWd,_lhsOmxDstNdWd,_lhsOndStr,_lhsOpp,_lhsOself)))
sem_ANm_Wild :: T_ANm 
sem_ANm_Wild  =
    (\ _lhsIisDest
       _lhsImbDstWd
       _lhsImbPrevNdStr ->
         (let _lhsOpp :: PP_Doc
              _lhsOmxDstNdWd :: Int
              _lhsOmxDstAtWd :: Int
              _lhsOself :: ANm 
              _lhsOndStr :: String
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 47, column 21)
              _nm =
                  nmWild
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 48, column 21)
              _lhsOpp =
                  ppDest "W" _lhsIisDest _lhsImbDstWd _lhsImbPrevNdStr ""     ""     _nm
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 71, column 21)
              _lhsOmxDstNdWd =
                  0
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 73, column 21)
              _lhsOmxDstAtWd =
                  length . show $ _nm
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 89, column 21)
              _ndStr =
                  ""
              -- self rule
              _self =
                  ANm_Wild
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (from local)
              _lhsOndStr =
                  _ndStr
          in  ( _lhsOmxDstAtWd,_lhsOmxDstNdWd,_lhsOndStr,_lhsOpp,_lhsOself)))
-- ARule -------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         pp                   : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Rule:
         child ndNmL          : {[Nm]}
         child rlNm           : {Nm}
         child info           : {[String]}
         child eqns           : AEqns 
         visit 0:
            local mbDstWd     : _
            local mbPrevNdStr : _
            local self        : _
-}
-- cata
sem_ARule :: ARule  ->
             T_ARule 
sem_ARule (ARule_Rule _ndNmL _rlNm _info _eqns )  =
    (sem_ARule_Rule _ndNmL _rlNm _info (sem_AEqns _eqns ) )
-- semantic domain
type T_ARule  = ( PP_Doc,ARule )
sem_ARule_Rule :: ([Nm]) ->
                  Nm ->
                  ([String]) ->
                  T_AEqns  ->
                  T_ARule 
sem_ARule_Rule ndNmL_ rlNm_ info_ eqns_  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: ARule 
         _eqnsOmbDstWd :: (Maybe (Int,Int))
         _eqnsOmbPrevNdStr :: (Maybe String)
         _eqnsImxDstAtWd :: Int
         _eqnsImxDstNdWd :: Int
         _eqnsIpp :: PP_Doc
         _eqnsIself :: AEqns 
         -- "build/ruler2/ARule/PrettyPrintAG.ag"(line 9, column 21)
         _lhsOpp =
             "SEM" >#< pp (head ndNmL_)
             >-< indent atLhs2texDist ("|" >#< rlNm_ >|< indent atLhs2texDist _eqnsIpp)
         -- "build/ruler2/ARule/PrettyPrintAG.ag"(line 47, column 21)
         _mbDstWd =
             Just (_eqnsImxDstNdWd,_eqnsImxDstAtWd)
         -- "build/ruler2/ARule/PrettyPrintAG.ag"(line 84, column 21)
         _mbPrevNdStr =
             Nothing
         -- self rule
         _self =
             ARule_Rule ndNmL_ rlNm_ info_ _eqnsIself
         -- self rule
         _lhsOself =
             _self
         -- copy rule (from local)
         _eqnsOmbDstWd =
             _mbDstWd
         -- copy rule (from local)
         _eqnsOmbPrevNdStr =
             _mbPrevNdStr
         ( _eqnsImxDstAtWd,_eqnsImxDstNdWd,_eqnsIpp,_eqnsIself) =
             eqns_ _eqnsOmbDstWd _eqnsOmbPrevNdStr 
     in  ( _lhsOpp,_lhsOself))
-- ARules ------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         pp                   : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : ARule 
         child tl             : ARules 
         visit 0:
            local self        : _
      alternative Nil:
         visit 0:
            local self        : _
-}
-- cata
sem_ARules :: ARules  ->
              T_ARules 
sem_ARules list  =
    (Prelude.foldr sem_ARules_Cons sem_ARules_Nil (Prelude.map sem_ARule list) )
-- semantic domain
type T_ARules  = ( PP_Doc,ARules )
sem_ARules_Cons :: T_ARule  ->
                   T_ARules  ->
                   T_ARules 
sem_ARules_Cons hd_ tl_  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: ARules 
         _hdIpp :: PP_Doc
         _hdIself :: ARule 
         _tlIpp :: PP_Doc
         _tlIself :: ARules 
         -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 6, column 35)
         _lhsOpp =
             _hdIpp >-< _tlIpp
         -- self rule
         _self =
             (:) _hdIself _tlIself
         -- self rule
         _lhsOself =
             _self
         ( _hdIpp,_hdIself) =
             hd_ 
         ( _tlIpp,_tlIself) =
             tl_ 
     in  ( _lhsOpp,_lhsOself))
sem_ARules_Nil :: T_ARules 
sem_ARules_Nil  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: ARules 
         -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 6, column 35)
         _lhsOpp =
             empty
         -- self rule
         _self =
             []
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
-- ECnstr ------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         pp                   : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Empty:
         visit 0:
            local self        : _
      alternative Ty:
         child nms            : {[Nm]}
         visit 0:
            local self        : _
      alternative Var:
         child nm             : {Nm}
         visit 0:
            local self        : _
-}
-- cata
sem_ECnstr :: ECnstr  ->
              T_ECnstr 
sem_ECnstr (ECnstr_Empty )  =
    (sem_ECnstr_Empty )
sem_ECnstr (ECnstr_Ty _nms )  =
    (sem_ECnstr_Ty _nms )
sem_ECnstr (ECnstr_Var _nm )  =
    (sem_ECnstr_Var _nm )
-- semantic domain
type T_ECnstr  = ( PP_Doc,ECnstr )
sem_ECnstr_Empty :: T_ECnstr 
sem_ECnstr_Empty  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: ECnstr 
         -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 6, column 33)
         _lhsOpp =
             empty
         -- self rule
         _self =
             ECnstr_Empty
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_ECnstr_Ty :: ([Nm]) ->
                 T_ECnstr 
sem_ECnstr_Ty nms_  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: ECnstr 
         -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 43, column 21)
         _lhsOpp =
             ppCommas' nms_
         -- self rule
         _self =
             ECnstr_Ty nms_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_ECnstr_Var :: Nm ->
                  T_ECnstr 
sem_ECnstr_Var nm_  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: ECnstr 
         -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 44, column 21)
         _lhsOpp =
             pp nm_
         -- self rule
         _self =
             ECnstr_Var nm_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
-- Expr --------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         pp                   : PP_Doc
         self                 : SELF 
   alternatives:
      alternative AVar:
         child anm            : ANm 
         visit 0:
            local mbDstWd     : _
            local mbPrevNdStr : _
            local self        : _
      alternative App:
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local self        : _
      alternative AppTop:
         child expr           : Expr 
         visit 0:
            local self        : _
      alternative ChildOrder:
         child seqNr          : {Int}
         child expr           : Expr 
         visit 0:
            local self        : _
      alternative Cnstr:
         child expr           : Expr 
         child cnstr          : ECnstr 
         visit 0:
            local self        : _
      alternative Empty:
         visit 0:
            local self        : _
      alternative Expr:
         child expr           : Expr 
         visit 0:
            local self        : _
      alternative Int:
         child int            : {String}
         visit 0:
            local self        : _
      alternative LF:
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local self        : _
      alternative Named:
         child nm             : {Nm}
         child expr           : Expr 
         visit 0:
            local self        : _
      alternative Op:
         child nm             : {Nm}
         child nmExpr         : Expr 
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local self        : _
      alternative Paren:
         child expr           : Expr 
         visit 0:
            local self        : _
      alternative Retain:
         child expr           : Expr 
         visit 0:
            local self        : _
      alternative SP:
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local self        : _
      alternative Sel:
         child expr           : Expr 
         child selMbExpr      : MbExpr 
         visit 0:
            local self        : _
      alternative SelTop:
         child expr           : Expr 
         visit 0:
            local self        : _
      alternative StrAsIs:
         child str            : {String}
         visit 0:
            local self        : _
      alternative StrText:
         child str            : {String}
         visit 0:
            local self        : _
      alternative Undefined:
         visit 0:
            local self        : _
      alternative Uniq:
         visit 0:
            local self        : _
      alternative Var:
         child nm             : {Nm}
         visit 0:
            local self        : _
      alternative Wrap:
         child wrKind         : {WrKind}
         child expr           : Expr 
         visit 0:
            local self        : _
      alternative WrapCnstr:
         child cnstr          : ECnstr 
         visit 0:
            local self        : _
-}
-- cata
sem_Expr :: Expr  ->
            T_Expr 
sem_Expr (Expr_AVar _anm )  =
    (sem_Expr_AVar (sem_ANm _anm ) )
sem_Expr (Expr_App _lExpr _rExpr )  =
    (sem_Expr_App (sem_Expr _lExpr ) (sem_Expr _rExpr ) )
sem_Expr (Expr_AppTop _expr )  =
    (sem_Expr_AppTop (sem_Expr _expr ) )
sem_Expr (Expr_ChildOrder _seqNr _expr )  =
    (sem_Expr_ChildOrder _seqNr (sem_Expr _expr ) )
sem_Expr (Expr_Cnstr _expr _cnstr )  =
    (sem_Expr_Cnstr (sem_Expr _expr ) (sem_ECnstr _cnstr ) )
sem_Expr (Expr_Empty )  =
    (sem_Expr_Empty )
sem_Expr (Expr_Expr _expr )  =
    (sem_Expr_Expr (sem_Expr _expr ) )
sem_Expr (Expr_Int _int )  =
    (sem_Expr_Int _int )
sem_Expr (Expr_LF _lExpr _rExpr )  =
    (sem_Expr_LF (sem_Expr _lExpr ) (sem_Expr _rExpr ) )
sem_Expr (Expr_Named _nm _expr )  =
    (sem_Expr_Named _nm (sem_Expr _expr ) )
sem_Expr (Expr_Op _nm _nmExpr _lExpr _rExpr )  =
    (sem_Expr_Op _nm (sem_Expr _nmExpr ) (sem_Expr _lExpr ) (sem_Expr _rExpr ) )
sem_Expr (Expr_Paren _expr )  =
    (sem_Expr_Paren (sem_Expr _expr ) )
sem_Expr (Expr_Retain _expr )  =
    (sem_Expr_Retain (sem_Expr _expr ) )
sem_Expr (Expr_SP _lExpr _rExpr )  =
    (sem_Expr_SP (sem_Expr _lExpr ) (sem_Expr _rExpr ) )
sem_Expr (Expr_Sel _expr _selMbExpr )  =
    (sem_Expr_Sel (sem_Expr _expr ) (sem_MbExpr _selMbExpr ) )
sem_Expr (Expr_SelTop _expr )  =
    (sem_Expr_SelTop (sem_Expr _expr ) )
sem_Expr (Expr_StrAsIs _str )  =
    (sem_Expr_StrAsIs _str )
sem_Expr (Expr_StrText _str )  =
    (sem_Expr_StrText _str )
sem_Expr (Expr_Undefined )  =
    (sem_Expr_Undefined )
sem_Expr (Expr_Uniq )  =
    (sem_Expr_Uniq )
sem_Expr (Expr_Var _nm )  =
    (sem_Expr_Var _nm )
sem_Expr (Expr_Wrap _wrKind _expr )  =
    (sem_Expr_Wrap _wrKind (sem_Expr _expr ) )
sem_Expr (Expr_WrapCnstr _cnstr )  =
    (sem_Expr_WrapCnstr (sem_ECnstr _cnstr ) )
-- semantic domain
type T_Expr  = ( PP_Doc,Expr )
sem_Expr_AVar :: T_ANm  ->
                 T_Expr 
sem_Expr_AVar anm_  =
    (let _anmOisDest :: Bool
         _lhsOpp :: PP_Doc
         _lhsOself :: Expr 
         _anmOmbDstWd :: (Maybe (Int,Int))
         _anmOmbPrevNdStr :: (Maybe String)
         _anmImxDstAtWd :: Int
         _anmImxDstNdWd :: Int
         _anmIndStr :: String
         _anmIpp :: PP_Doc
         _anmIself :: ANm 
         -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 61, column 21)
         _anmOisDest =
             False
         -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 76, column 21)
         _mbDstWd =
             Nothing
         -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 92, column 21)
         _mbPrevNdStr =
             Nothing
         -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 6, column 33)
         _lhsOpp =
             _anmIpp
         -- self rule
         _self =
             Expr_AVar _anmIself
         -- self rule
         _lhsOself =
             _self
         -- copy rule (from local)
         _anmOmbDstWd =
             _mbDstWd
         -- copy rule (from local)
         _anmOmbPrevNdStr =
             _mbPrevNdStr
         ( _anmImxDstAtWd,_anmImxDstNdWd,_anmIndStr,_anmIpp,_anmIself) =
             anm_ _anmOisDest _anmOmbDstWd _anmOmbPrevNdStr 
     in  ( _lhsOpp,_lhsOself))
sem_Expr_App :: T_Expr  ->
                T_Expr  ->
                T_Expr 
sem_Expr_App lExpr_ rExpr_  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: Expr 
         _lExprIpp :: PP_Doc
         _lExprIself :: Expr 
         _rExprIpp :: PP_Doc
         _rExprIself :: Expr 
         -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 9, column 21)
         _lhsOpp =
             exprNeedPar ParCtxtAppL nmUnk _lExprIself _lExprIpp
             >#< exprNeedPar ParCtxtAppR nmUnk _rExprIself _rExprIpp
         -- self rule
         _self =
             Expr_App _lExprIself _rExprIself
         -- self rule
         _lhsOself =
             _self
         ( _lExprIpp,_lExprIself) =
             lExpr_ 
         ( _rExprIpp,_rExprIself) =
             rExpr_ 
     in  ( _lhsOpp,_lhsOself))
sem_Expr_AppTop :: T_Expr  ->
                   T_Expr 
sem_Expr_AppTop expr_  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: Expr 
         _exprIpp :: PP_Doc
         _exprIself :: Expr 
         -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 30, column 21)
         _lhsOpp =
             _exprIpp
         -- self rule
         _self =
             Expr_AppTop _exprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprIpp,_exprIself) =
             expr_ 
     in  ( _lhsOpp,_lhsOself))
sem_Expr_ChildOrder :: Int ->
                       T_Expr  ->
                       T_Expr 
sem_Expr_ChildOrder seqNr_ expr_  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: Expr 
         _exprIpp :: PP_Doc
         _exprIself :: Expr 
         -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 6, column 33)
         _lhsOpp =
             _exprIpp
         -- self rule
         _self =
             Expr_ChildOrder seqNr_ _exprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprIpp,_exprIself) =
             expr_ 
     in  ( _lhsOpp,_lhsOself))
sem_Expr_Cnstr :: T_Expr  ->
                  T_ECnstr  ->
                  T_Expr 
sem_Expr_Cnstr expr_ cnstr_  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: Expr 
         _exprIpp :: PP_Doc
         _exprIself :: Expr 
         _cnstrIpp :: PP_Doc
         _cnstrIself :: ECnstr 
         -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 23, column 21)
         _lhsOpp =
             ppCurlys (_exprIpp >|< "|" >|< _cnstrIpp)
         -- self rule
         _self =
             Expr_Cnstr _exprIself _cnstrIself
         -- self rule
         _lhsOself =
             _self
         ( _exprIpp,_exprIself) =
             expr_ 
         ( _cnstrIpp,_cnstrIself) =
             cnstr_ 
     in  ( _lhsOpp,_lhsOself))
sem_Expr_Empty :: T_Expr 
sem_Expr_Empty  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: Expr 
         -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 6, column 33)
         _lhsOpp =
             empty
         -- self rule
         _self =
             Expr_Empty
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Expr_Expr :: T_Expr  ->
                 T_Expr 
sem_Expr_Expr expr_  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: Expr 
         _exprIpp :: PP_Doc
         _exprIself :: Expr 
         -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 6, column 33)
         _lhsOpp =
             _exprIpp
         -- self rule
         _self =
             Expr_Expr _exprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprIpp,_exprIself) =
             expr_ 
     in  ( _lhsOpp,_lhsOself))
sem_Expr_Int :: String ->
                T_Expr 
sem_Expr_Int int_  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: Expr 
         -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 20, column 21)
         _lhsOpp =
             pp int_
         -- self rule
         _self =
             Expr_Int int_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Expr_LF :: T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_LF lExpr_ rExpr_  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: Expr 
         _lExprIpp :: PP_Doc
         _lExprIself :: Expr 
         _rExprIpp :: PP_Doc
         _rExprIself :: Expr 
         -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 17, column 21)
         _lhsOpp =
             exprNeedPar ParCtxtAppR nmUnk _lExprIself _lExprIpp
             >-< exprNeedPar ParCtxtAppR nmUnk _rExprIself _rExprIpp
         -- self rule
         _self =
             Expr_LF _lExprIself _rExprIself
         -- self rule
         _lhsOself =
             _self
         ( _lExprIpp,_lExprIself) =
             lExpr_ 
         ( _rExprIpp,_rExprIself) =
             rExpr_ 
     in  ( _lhsOpp,_lhsOself))
sem_Expr_Named :: Nm ->
                  T_Expr  ->
                  T_Expr 
sem_Expr_Named nm_ expr_  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: Expr 
         _exprIpp :: PP_Doc
         _exprIself :: Expr 
         -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 22, column 21)
         _lhsOpp =
             _exprIpp
         -- self rule
         _self =
             Expr_Named nm_ _exprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprIpp,_exprIself) =
             expr_ 
     in  ( _lhsOpp,_lhsOself))
sem_Expr_Op :: Nm ->
               T_Expr  ->
               T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_Op nm_ nmExpr_ lExpr_ rExpr_  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: Expr 
         _nmExprIpp :: PP_Doc
         _nmExprIself :: Expr 
         _lExprIpp :: PP_Doc
         _lExprIself :: Expr 
         _rExprIpp :: PP_Doc
         _rExprIself :: Expr 
         -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 11, column 21)
         _lhsOpp =
             let op = if nm_ == nmSp1 then empty else _nmExprIpp
             in  ppExprMbEmpty _lExprIself (>|< " ")
                     (exprNeedPar ParCtxtOpL nm_ _lExprIself _lExprIpp)
                 >|< op
                 >|< ppExprMbEmpty _rExprIself (" " >|<)
                         (exprNeedPar ParCtxtOpR nm_ _rExprIself _rExprIpp)
         -- self rule
         _self =
             Expr_Op nm_ _nmExprIself _lExprIself _rExprIself
         -- self rule
         _lhsOself =
             _self
         ( _nmExprIpp,_nmExprIself) =
             nmExpr_ 
         ( _lExprIpp,_lExprIself) =
             lExpr_ 
         ( _rExprIpp,_rExprIself) =
             rExpr_ 
     in  ( _lhsOpp,_lhsOself))
sem_Expr_Paren :: T_Expr  ->
                  T_Expr 
sem_Expr_Paren expr_  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: Expr 
         _exprIpp :: PP_Doc
         _exprIself :: Expr 
         -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 29, column 21)
         _lhsOpp =
             _exprIpp
         -- self rule
         _self =
             Expr_Paren _exprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprIpp,_exprIself) =
             expr_ 
     in  ( _lhsOpp,_lhsOself))
sem_Expr_Retain :: T_Expr  ->
                   T_Expr 
sem_Expr_Retain expr_  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: Expr 
         _exprIpp :: PP_Doc
         _exprIself :: Expr 
         -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 6, column 33)
         _lhsOpp =
             _exprIpp
         -- self rule
         _self =
             Expr_Retain _exprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprIpp,_exprIself) =
             expr_ 
     in  ( _lhsOpp,_lhsOself))
sem_Expr_SP :: T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_SP lExpr_ rExpr_  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: Expr 
         _lExprIpp :: PP_Doc
         _lExprIself :: Expr 
         _rExprIpp :: PP_Doc
         _rExprIself :: Expr 
         -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 19, column 21)
         _lhsOpp =
             _lExprIpp >|< _rExprIpp
         -- self rule
         _self =
             Expr_SP _lExprIself _rExprIself
         -- self rule
         _lhsOself =
             _self
         ( _lExprIpp,_lExprIself) =
             lExpr_ 
         ( _rExprIpp,_rExprIself) =
             rExpr_ 
     in  ( _lhsOpp,_lhsOself))
sem_Expr_Sel :: T_Expr  ->
                T_MbExpr  ->
                T_Expr 
sem_Expr_Sel expr_ selMbExpr_  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: Expr 
         _exprIpp :: PP_Doc
         _exprIself :: Expr 
         _selMbExprIpp :: PP_Doc
         _selMbExprIself :: MbExpr 
         -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 27, column 21)
         _lhsOpp =
             exprNeedPar ParCtxtOther nmUnk _exprIself _exprIpp >|< cfgStrSel >|< _selMbExprIpp
         -- self rule
         _self =
             Expr_Sel _exprIself _selMbExprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprIpp,_exprIself) =
             expr_ 
         ( _selMbExprIpp,_selMbExprIself) =
             selMbExpr_ 
     in  ( _lhsOpp,_lhsOself))
sem_Expr_SelTop :: T_Expr  ->
                   T_Expr 
sem_Expr_SelTop expr_  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: Expr 
         _exprIpp :: PP_Doc
         _exprIself :: Expr 
         -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 6, column 33)
         _lhsOpp =
             _exprIpp
         -- self rule
         _self =
             Expr_SelTop _exprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprIpp,_exprIself) =
             expr_ 
     in  ( _lhsOpp,_lhsOself))
sem_Expr_StrAsIs :: String ->
                    T_Expr 
sem_Expr_StrAsIs str_  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: Expr 
         -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 21, column 21)
         _lhsOpp =
             pp str_
         -- self rule
         _self =
             Expr_StrAsIs str_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Expr_StrText :: String ->
                    T_Expr 
sem_Expr_StrText str_  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: Expr 
         -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 21, column 21)
         _lhsOpp =
             pp str_
         -- self rule
         _self =
             Expr_StrText str_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Expr_Undefined :: T_Expr 
sem_Expr_Undefined  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: Expr 
         -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 25, column 21)
         _lhsOpp =
             pp "_"
         -- self rule
         _self =
             Expr_Undefined
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Expr_Uniq :: T_Expr 
sem_Expr_Uniq  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: Expr 
         -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 24, column 21)
         _lhsOpp =
             pp "?uniq?"
         -- self rule
         _self =
             Expr_Uniq
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Expr_Var :: Nm ->
                T_Expr 
sem_Expr_Var nm_  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: Expr 
         -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 28, column 21)
         _lhsOpp =
             pp nm_
         -- self rule
         _self =
             Expr_Var nm_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Expr_Wrap :: WrKind ->
                 T_Expr  ->
                 T_Expr 
sem_Expr_Wrap wrKind_ expr_  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: Expr 
         _exprIpp :: PP_Doc
         _exprIself :: Expr 
         -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 6, column 33)
         _lhsOpp =
             _exprIpp
         -- self rule
         _self =
             Expr_Wrap wrKind_ _exprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprIpp,_exprIself) =
             expr_ 
     in  ( _lhsOpp,_lhsOself))
sem_Expr_WrapCnstr :: T_ECnstr  ->
                      T_Expr 
sem_Expr_WrapCnstr cnstr_  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: Expr 
         _cnstrIpp :: PP_Doc
         _cnstrIself :: ECnstr 
         -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 6, column 33)
         _lhsOpp =
             _cnstrIpp
         -- self rule
         _self =
             Expr_WrapCnstr _cnstrIself
         -- self rule
         _lhsOself =
             _self
         ( _cnstrIpp,_cnstrIself) =
             cnstr_ 
     in  ( _lhsOpp,_lhsOself))
-- MbExpr ------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         pp                   : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Just:
         child just           : Expr 
         visit 0:
            local self        : _
      alternative Nothing:
         visit 0:
            local self        : _
-}
-- cata
sem_MbExpr :: MbExpr  ->
              T_MbExpr 
sem_MbExpr (Prelude.Just x )  =
    (sem_MbExpr_Just (sem_Expr x ) )
sem_MbExpr Prelude.Nothing  =
    sem_MbExpr_Nothing
-- semantic domain
type T_MbExpr  = ( PP_Doc,MbExpr )
sem_MbExpr_Just :: T_Expr  ->
                   T_MbExpr 
sem_MbExpr_Just just_  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: MbExpr 
         _justIpp :: PP_Doc
         _justIself :: Expr 
         -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 6, column 33)
         _lhsOpp =
             _justIpp
         -- self rule
         _self =
             Just _justIself
         -- self rule
         _lhsOself =
             _self
         ( _justIpp,_justIself) =
             just_ 
     in  ( _lhsOpp,_lhsOself))
sem_MbExpr_Nothing :: T_MbExpr 
sem_MbExpr_Nothing  =
    (let _lhsOpp :: PP_Doc
         _lhsOself :: MbExpr 
         -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 6, column 33)
         _lhsOpp =
             empty
         -- self rule
         _self =
             Nothing
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
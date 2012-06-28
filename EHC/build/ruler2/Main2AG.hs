

-- UUAGC 0.9.39.1 (build/ruler2/Main2AG.ag)
module Main2AG where

import Data.Maybe
import Data.Char
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import EH.Util.Pretty
import EH.Util.PrettyUtils
import EH.Util.Utils
import Opts
import Common
import LaTeXFmtUtils
import Expr.Utils
import Ty.Utils
import ARule.Utils
import ViewSel.Utils
import Config (cfgStrSel)
import FmGam
import RwExprGam
import ECnstrGam
import AbsSyn.AbsSyn2











ppAS2 :: Opts -> FmGam Expr -> Decls -> PP_Doc
ppAS2 o g r
  = pp_Syn_AGItf r2
  where r1 = sem_AGItf (AGItf_AGItf r)
        r2 = wrap_AGItf r1
                (Inh_AGItf {opts_Inh_AGItf = o, fmGam_Inh_AGItf = g})


data PPCfg
  = PPCfg
      { pcDef       ::  Nm
      , pcUse       ::  Nm
      , pcFigEnv    ::  Nm
      , pcRule      ::  Nm
      }

mkPPCfg :: Opts -> FmGam Expr -> PPCfg
mkPPCfg o g
  = PPCfg
      { pcDef       =   c "rulerCmdDef"
      , pcUse       =   c "rulerCmdUse"
      , pcFigEnv    =   c "rulerRulesetFigureEnv"
      , pcRule      =   c "rulerRuleCmd"
      }
  where c = fmNmFmtCmd o g . Nm

-- AEqn --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
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
type T_AEqn  = (FmGam Expr) ->
               (Maybe (Int,Int)) ->
               (Maybe String) ->
               ( Int,Int,String,PP_Doc,AEqn )
sem_AEqn_Eqn :: T_AEqnDest  ->
                T_AExpr  ->
                T_AEqn 
sem_AEqn_Eqn dest_ val_  =
    (\ _lhsIfmGam
       _lhsImbDstWd
       _lhsImbPrevNdStr ->
         (let _lhsOpp :: PP_Doc
              _lhsOmxDstAtWd :: Int
              _lhsOmxDstNdWd :: Int
              _lhsOself :: AEqn 
              _lhsOndStr :: String
              _destOfmGam :: (FmGam Expr)
              _destOmbDstWd :: (Maybe (Int,Int))
              _destOmbPrevNdStr :: (Maybe String)
              _valOfmGam :: (FmGam Expr)
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
              _destOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _destOmbDstWd =
                  _lhsImbDstWd
              -- copy rule (down)
              _destOmbPrevNdStr =
                  _lhsImbPrevNdStr
              -- copy rule (down)
              _valOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _valOmbDstWd =
                  _lhsImbDstWd
              ( _destIdstWd,_destIisComposite,_destImxDstAtWd,_destImxDstNdWd,_destIndStr,_destIpp,_destIself) =
                  dest_ _destOfmGam _destOmbDstWd _destOmbPrevNdStr 
              ( _valImxDstAtWd,_valImxDstNdWd,_valIpp,_valIself) =
                  val_ _valOfmGam _valOmbDstWd 
          in  ( _lhsOmxDstAtWd,_lhsOmxDstNdWd,_lhsOndStr,_lhsOpp,_lhsOself)))
sem_AEqn_Err :: T_Expr  ->
                T_AEqn 
sem_AEqn_Err expr_  =
    (\ _lhsIfmGam
       _lhsImbDstWd
       _lhsImbPrevNdStr ->
         (let _lhsOpp :: PP_Doc
              _lhsOndStr :: String
              _lhsOmxDstAtWd :: Int
              _lhsOmxDstNdWd :: Int
              _lhsOself :: AEqn 
              _exprOfmGam :: (FmGam Expr)
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
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              ( _exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOmxDstAtWd,_lhsOmxDstNdWd,_lhsOndStr,_lhsOpp,_lhsOself)))
-- AEqnDest ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
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
type T_AEqnDest  = (FmGam Expr) ->
                   (Maybe (Int,Int)) ->
                   (Maybe String) ->
                   ( Int,Bool,Int,Int,String,PP_Doc,AEqnDest )
sem_AEqnDest_Many :: T_AEqnDests  ->
                     T_AEqnDest 
sem_AEqnDest_Many dests_  =
    (\ _lhsIfmGam
       _lhsImbDstWd
       _lhsImbPrevNdStr ->
         (let _lhsOpp :: PP_Doc
              _destsOmbDstWd :: (Maybe (Int,Int))
              _lhsOdstWd :: Int
              _lhsOndStr :: String
              _lhsOisComposite :: Bool
              _lhsOmxDstAtWd :: Int
              _lhsOmxDstNdWd :: Int
              _lhsOself :: AEqnDest 
              _destsOfmGam :: (FmGam Expr)
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
              -- copy rule (down)
              _destsOfmGam =
                  _lhsIfmGam
              ( _destsIdstWdL,_destsImxDstAtWd,_destsImxDstNdWd,_destsIpp,_destsIppL,_destsIself) =
                  dests_ _destsOfmGam _destsOmbDstWd 
          in  ( _lhsOdstWd,_lhsOisComposite,_lhsOmxDstAtWd,_lhsOmxDstNdWd,_lhsOndStr,_lhsOpp,_lhsOself)))
sem_AEqnDest_One :: T_ANm  ->
                    T_AEqnDest 
sem_AEqnDest_One anm_  =
    (\ _lhsIfmGam
       _lhsImbDstWd
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
      inherited attributes:
         fmGam                : FmGam Expr
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
type T_AEqnDests  = (FmGam Expr) ->
                    (Maybe (Int,Int)) ->
                    ( ([Int]),Int,Int,PP_Doc,([PP_Doc]),AEqnDests )
sem_AEqnDests_Cons :: T_AEqnDest  ->
                      T_AEqnDests  ->
                      T_AEqnDests 
sem_AEqnDests_Cons hd_ tl_  =
    (\ _lhsIfmGam
       _lhsImbDstWd ->
         (let _lhsOppL :: ([PP_Doc])
              _lhsOdstWdL :: ([Int])
              _lhsOmxDstAtWd :: Int
              _lhsOmxDstNdWd :: Int
              _lhsOpp :: PP_Doc
              _lhsOself :: AEqnDests 
              _hdOfmGam :: (FmGam Expr)
              _hdOmbDstWd :: (Maybe (Int,Int))
              _hdOmbPrevNdStr :: (Maybe String)
              _tlOfmGam :: (FmGam Expr)
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
              _hdOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _hdOmbDstWd =
                  _lhsImbDstWd
              -- copy rule (from local)
              _hdOmbPrevNdStr =
                  _mbPrevNdStr
              -- copy rule (down)
              _tlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _tlOmbDstWd =
                  _lhsImbDstWd
              ( _hdIdstWd,_hdIisComposite,_hdImxDstAtWd,_hdImxDstNdWd,_hdIndStr,_hdIpp,_hdIself) =
                  hd_ _hdOfmGam _hdOmbDstWd _hdOmbPrevNdStr 
              ( _tlIdstWdL,_tlImxDstAtWd,_tlImxDstNdWd,_tlIpp,_tlIppL,_tlIself) =
                  tl_ _tlOfmGam _tlOmbDstWd 
          in  ( _lhsOdstWdL,_lhsOmxDstAtWd,_lhsOmxDstNdWd,_lhsOpp,_lhsOppL,_lhsOself)))
sem_AEqnDests_Nil :: T_AEqnDests 
sem_AEqnDests_Nil  =
    (\ _lhsIfmGam
       _lhsImbDstWd ->
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
         fmGam                : FmGam Expr
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
type T_AEqns  = (FmGam Expr) ->
                (Maybe (Int,Int)) ->
                (Maybe String) ->
                ( Int,Int,PP_Doc,AEqns )
sem_AEqns_Cons :: T_AEqn  ->
                  T_AEqns  ->
                  T_AEqns 
sem_AEqns_Cons hd_ tl_  =
    (\ _lhsIfmGam
       _lhsImbDstWd
       _lhsImbPrevNdStr ->
         (let _tlOmbPrevNdStr :: (Maybe String)
              _lhsOmxDstAtWd :: Int
              _lhsOmxDstNdWd :: Int
              _lhsOpp :: PP_Doc
              _lhsOself :: AEqns 
              _hdOfmGam :: (FmGam Expr)
              _hdOmbDstWd :: (Maybe (Int,Int))
              _hdOmbPrevNdStr :: (Maybe String)
              _tlOfmGam :: (FmGam Expr)
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
              _hdOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _hdOmbDstWd =
                  _lhsImbDstWd
              -- copy rule (down)
              _hdOmbPrevNdStr =
                  _lhsImbPrevNdStr
              -- copy rule (down)
              _tlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _tlOmbDstWd =
                  _lhsImbDstWd
              ( _hdImxDstAtWd,_hdImxDstNdWd,_hdIndStr,_hdIpp,_hdIself) =
                  hd_ _hdOfmGam _hdOmbDstWd _hdOmbPrevNdStr 
              ( _tlImxDstAtWd,_tlImxDstNdWd,_tlIpp,_tlIself) =
                  tl_ _tlOfmGam _tlOmbDstWd _tlOmbPrevNdStr 
          in  ( _lhsOmxDstAtWd,_lhsOmxDstNdWd,_lhsOpp,_lhsOself)))
sem_AEqns_Nil :: T_AEqns 
sem_AEqns_Nil  =
    (\ _lhsIfmGam
       _lhsImbDstWd
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
      inherited attributes:
         fmGam                : FmGam Expr
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
type T_AExpr  = (FmGam Expr) ->
                (Maybe (Int,Int)) ->
                ( Int,Int,PP_Doc,AExpr )
sem_AExpr_Expr :: T_Expr  ->
                  T_AExpr 
sem_AExpr_Expr expr_  =
    (\ _lhsIfmGam
       _lhsImbDstWd ->
         (let _lhsOpp :: PP_Doc
              _lhsOmxDstAtWd :: Int
              _lhsOmxDstNdWd :: Int
              _lhsOself :: AExpr 
              _exprOfmGam :: (FmGam Expr)
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
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              ( _exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOmxDstAtWd,_lhsOmxDstNdWd,_lhsOpp,_lhsOself)))
-- AGARuleItf --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         fmGam                : FmGam Expr
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
type T_AGARuleItf  = (FmGam Expr) ->
                     ( PP_Doc,ARule )
sem_AGARuleItf_AGItf :: T_ARule  ->
                        T_AGARuleItf 
sem_AGARuleItf_AGItf rule_  =
    (\ _lhsIfmGam ->
         (let _lhsOpp :: PP_Doc
              _lhsOself :: ARule 
              _ruleOfmGam :: (FmGam Expr)
              _ruleIpp :: PP_Doc
              _ruleIself :: ARule 
              -- use rule "build/ruler2/ARule/PrettyPrintAG.ag"(line 6, column 35)
              _lhsOpp =
                  _ruleIpp
              -- copy rule (up)
              _lhsOself =
                  _ruleIself
              -- copy rule (down)
              _ruleOfmGam =
                  _lhsIfmGam
              ( _ruleIpp,_ruleIself) =
                  rule_ _ruleOfmGam 
          in  ( _lhsOpp,_lhsOself)))
-- AGExprItf ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         fmGam                : FmGam Expr
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
type T_AGExprItf  = (FmGam Expr) ->
                    ( PP_Doc,Expr )
sem_AGExprItf_AGItf :: T_Expr  ->
                       T_AGExprItf 
sem_AGExprItf_AGItf expr_  =
    (\ _lhsIfmGam ->
         (let _lhsOpp :: PP_Doc
              _lhsOself :: Expr 
              _exprOfmGam :: (FmGam Expr)
              _exprIpp :: PP_Doc
              _exprIself :: Expr 
              -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 6, column 33)
              _lhsOpp =
                  _exprIpp
              -- copy rule (up)
              _lhsOself =
                  _exprIself
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              ( _exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOpp,_lhsOself)))
-- AGItf -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative AGItf:
         child decls          : Decls 
-}
-- cata
sem_AGItf :: AGItf  ->
             T_AGItf 
sem_AGItf (AGItf_AGItf _decls )  =
    (sem_AGItf_AGItf (sem_Decls _decls ) )
-- semantic domain
type T_AGItf  = (FmGam Expr) ->
                Opts ->
                ( PP_Doc)
data Inh_AGItf  = Inh_AGItf {fmGam_Inh_AGItf :: (FmGam Expr),opts_Inh_AGItf :: Opts}
data Syn_AGItf  = Syn_AGItf {pp_Syn_AGItf :: PP_Doc}
wrap_AGItf :: T_AGItf  ->
              Inh_AGItf  ->
              Syn_AGItf 
wrap_AGItf sem (Inh_AGItf _lhsIfmGam _lhsIopts )  =
    (let ( _lhsOpp) = sem _lhsIfmGam _lhsIopts 
     in  (Syn_AGItf _lhsOpp ))
sem_AGItf_AGItf :: T_Decls  ->
                   T_AGItf 
sem_AGItf_AGItf decls_  =
    (\ _lhsIfmGam
       _lhsIopts ->
         (let _declsOppCfg :: PPCfg
              _lhsOpp :: PP_Doc
              _declsOfmGam :: (FmGam Expr)
              _declsOopts :: Opts
              _declsIpp :: PP_Doc
              -- "build/ruler2/AS2/Pretty.ag"(line 32, column 21)
              _declsOppCfg =
                  mkPPCfg _lhsIopts _lhsIfmGam
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 1, column 28)
              _lhsOpp =
                  _declsIpp
              -- copy rule (down)
              _declsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _declsOopts =
                  _lhsIopts
              ( _declsIpp) =
                  decls_ _declsOfmGam _declsOopts _declsOppCfg 
          in  ( _lhsOpp)))
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
      inherited attribute:
         fmGam                : FmGam Expr
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
type T_ARule  = (FmGam Expr) ->
                ( PP_Doc,ARule )
sem_ARule_Rule :: ([Nm]) ->
                  Nm ->
                  ([String]) ->
                  T_AEqns  ->
                  T_ARule 
sem_ARule_Rule ndNmL_ rlNm_ info_ eqns_  =
    (\ _lhsIfmGam ->
         (let _lhsOpp :: PP_Doc
              _lhsOself :: ARule 
              _eqnsOfmGam :: (FmGam Expr)
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
              -- copy rule (down)
              _eqnsOfmGam =
                  _lhsIfmGam
              -- copy rule (from local)
              _eqnsOmbDstWd =
                  _mbDstWd
              -- copy rule (from local)
              _eqnsOmbPrevNdStr =
                  _mbPrevNdStr
              ( _eqnsImxDstAtWd,_eqnsImxDstNdWd,_eqnsIpp,_eqnsIself) =
                  eqns_ _eqnsOfmGam _eqnsOmbDstWd _eqnsOmbPrevNdStr 
          in  ( _lhsOpp,_lhsOself)))
-- ARules ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         fmGam                : FmGam Expr
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
type T_ARules  = (FmGam Expr) ->
                 ( PP_Doc,ARules )
sem_ARules_Cons :: T_ARule  ->
                   T_ARules  ->
                   T_ARules 
sem_ARules_Cons hd_ tl_  =
    (\ _lhsIfmGam ->
         (let _lhsOpp :: PP_Doc
              _lhsOself :: ARules 
              _hdOfmGam :: (FmGam Expr)
              _tlOfmGam :: (FmGam Expr)
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
              -- copy rule (down)
              _hdOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _tlOfmGam =
                  _lhsIfmGam
              ( _hdIpp,_hdIself) =
                  hd_ _hdOfmGam 
              ( _tlIpp,_tlIself) =
                  tl_ _tlOfmGam 
          in  ( _lhsOpp,_lhsOself)))
sem_ARules_Nil :: T_ARules 
sem_ARules_Nil  =
    (\ _lhsIfmGam ->
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
          in  ( _lhsOpp,_lhsOself)))
-- AttrAGDecl --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
         ppCfg                : PPCfg
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Attr:
         child ndNm           : {Nm}
         child inhAts         : {[(Nm,Nm)]}
         child inhsynAts      : {[(Nm,Nm)]}
         child synAts         : {[(Nm,Nm)]}
-}
-- cata
sem_AttrAGDecl :: AttrAGDecl  ->
                  T_AttrAGDecl 
sem_AttrAGDecl (AttrAGDecl_Attr _ndNm _inhAts _inhsynAts _synAts )  =
    (sem_AttrAGDecl_Attr _ndNm _inhAts _inhsynAts _synAts )
-- semantic domain
type T_AttrAGDecl  = (FmGam Expr) ->
                     Opts ->
                     PPCfg ->
                     ( PP_Doc)
sem_AttrAGDecl_Attr :: Nm ->
                       ([(Nm,Nm)]) ->
                       ([(Nm,Nm)]) ->
                       ([(Nm,Nm)]) ->
                       T_AttrAGDecl 
sem_AttrAGDecl_Attr ndNm_ inhAts_ inhsynAts_ synAts_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              -- "build/ruler2/AS2/Pretty.ag"(line 51, column 21)
              _lhsOpp =
                  let mka g = ppListSep "" "" "  " [ n >|< ":" >#< t | (n,t) <- g ]
                  in  "ATTR" >#< ndNm_
                      >#< ppBrackets (mka inhAts_ >#< "|" >#< mka inhsynAts_ >#< "|" >#< mka synAts_)
          in  ( _lhsOpp)))
-- DataAGAlt ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         isFirstAlt           : Bool
         mkAltStr             : String -> String
         mxAltWd              : Int
         mxFldWd              : Int
         opts                 : Opts
         ppCfg                : PPCfg
      synthesized attributes:
         gathMxAltWd          : Int
         gathMxFldWd          : Int
         pp                   : PP_Doc
         ppHS                 : PP_Doc
   alternatives:
      alternative Alt:
         child nm             : {Nm}
         child flds           : DataAGFlds 
         visit 0:
            local nmStr       : _
            local mkFldStr    : _
-}
-- cata
sem_DataAGAlt :: DataAGAlt  ->
                 T_DataAGAlt 
sem_DataAGAlt (DataAGAlt_Alt _nm _flds )  =
    (sem_DataAGAlt_Alt _nm (sem_DataAGFlds _flds ) )
-- semantic domain
type T_DataAGAlt  = (FmGam Expr) ->
                    Bool ->
                    (String -> String) ->
                    Int ->
                    Int ->
                    Opts ->
                    PPCfg ->
                    ( Int,Int,PP_Doc,PP_Doc)
sem_DataAGAlt_Alt :: Nm ->
                     T_DataAGFlds  ->
                     T_DataAGAlt 
sem_DataAGAlt_Alt nm_ flds_  =
    (\ _lhsIfmGam
       _lhsIisFirstAlt
       _lhsImkAltStr
       _lhsImxAltWd
       _lhsImxFldWd
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              _lhsOgathMxAltWd :: Int
              _lhsOppHS :: PP_Doc
              _fldsOisFirstFld :: Bool
              _lhsOgathMxFldWd :: Int
              _fldsOfmGam :: (FmGam Expr)
              _fldsOmkFldStr :: (String -> String)
              _fldsOmxFldWd :: Int
              _fldsOopts :: Opts
              _fldsOppCfg :: PPCfg
              _fldsIgathMxFldWd :: Int
              _fldsIpp :: PP_Doc
              _fldsIppHS :: PP_Doc
              -- "build/ruler2/AS2/Pretty.ag"(line 62, column 21)
              _lhsOpp =
                  "|" >#< strPad _nmStr _lhsImxAltWd >|< _fldsIpp
              -- "build/ruler2/AS2/Pretty.ag"(line 78, column 21)
              _nmStr =
                  _lhsImkAltStr $ show $ nm_
              -- "build/ruler2/AS2/Pretty.ag"(line 79, column 21)
              _lhsOgathMxAltWd =
                  length _nmStr
              -- "build/ruler2/AS2/Pretty.ag"(line 94, column 21)
              _mkFldStr =
                  case optGenFM _lhsIopts of
                    FmAG -> id
                    FmHS -> \n -> n ++ "_" ++ _nmStr
              -- "build/ruler2/AS2/Pretty.ag"(line 99, column 21)
              _lhsOppHS =
                  (if _lhsIisFirstAlt then "=" else "|") >#< strPad _nmStr _lhsImxAltWd >|< (_fldsIppHS >-< "}")
              -- "build/ruler2/AS2/Pretty.ag"(line 115, column 21)
              _fldsOisFirstFld =
                  True
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 67, column 55)
              _lhsOgathMxFldWd =
                  _fldsIgathMxFldWd
              -- copy rule (down)
              _fldsOfmGam =
                  _lhsIfmGam
              -- copy rule (from local)
              _fldsOmkFldStr =
                  _mkFldStr
              -- copy rule (down)
              _fldsOmxFldWd =
                  _lhsImxFldWd
              -- copy rule (down)
              _fldsOopts =
                  _lhsIopts
              -- copy rule (down)
              _fldsOppCfg =
                  _lhsIppCfg
              ( _fldsIgathMxFldWd,_fldsIpp,_fldsIppHS) =
                  flds_ _fldsOfmGam _fldsOisFirstFld _fldsOmkFldStr _fldsOmxFldWd _fldsOopts _fldsOppCfg 
          in  ( _lhsOgathMxAltWd,_lhsOgathMxFldWd,_lhsOpp,_lhsOppHS)))
-- DataAGAlts --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         isFirstAlt           : Bool
         mkAltStr             : String -> String
         mxAltWd              : Int
         mxFldWd              : Int
         opts                 : Opts
         ppCfg                : PPCfg
      synthesized attributes:
         gathMxAltWd          : Int
         gathMxFldWd          : Int
         pp                   : PP_Doc
         ppHS                 : PP_Doc
   alternatives:
      alternative Cons:
         child hd             : DataAGAlt 
         child tl             : DataAGAlts 
      alternative Nil:
-}
-- cata
sem_DataAGAlts :: DataAGAlts  ->
                  T_DataAGAlts 
sem_DataAGAlts list  =
    (Prelude.foldr sem_DataAGAlts_Cons sem_DataAGAlts_Nil (Prelude.map sem_DataAGAlt list) )
-- semantic domain
type T_DataAGAlts  = (FmGam Expr) ->
                     Bool ->
                     (String -> String) ->
                     Int ->
                     Int ->
                     Opts ->
                     PPCfg ->
                     ( Int,Int,PP_Doc,PP_Doc)
sem_DataAGAlts_Cons :: T_DataAGAlt  ->
                       T_DataAGAlts  ->
                       T_DataAGAlts 
sem_DataAGAlts_Cons hd_ tl_  =
    (\ _lhsIfmGam
       _lhsIisFirstAlt
       _lhsImkAltStr
       _lhsImxAltWd
       _lhsImxFldWd
       _lhsIopts
       _lhsIppCfg ->
         (let _tlOisFirstAlt :: Bool
              _lhsOgathMxAltWd :: Int
              _lhsOgathMxFldWd :: Int
              _lhsOpp :: PP_Doc
              _lhsOppHS :: PP_Doc
              _hdOfmGam :: (FmGam Expr)
              _hdOisFirstAlt :: Bool
              _hdOmkAltStr :: (String -> String)
              _hdOmxAltWd :: Int
              _hdOmxFldWd :: Int
              _hdOopts :: Opts
              _hdOppCfg :: PPCfg
              _tlOfmGam :: (FmGam Expr)
              _tlOmkAltStr :: (String -> String)
              _tlOmxAltWd :: Int
              _tlOmxFldWd :: Int
              _tlOopts :: Opts
              _tlOppCfg :: PPCfg
              _hdIgathMxAltWd :: Int
              _hdIgathMxFldWd :: Int
              _hdIpp :: PP_Doc
              _hdIppHS :: PP_Doc
              _tlIgathMxAltWd :: Int
              _tlIgathMxFldWd :: Int
              _tlIpp :: PP_Doc
              _tlIppHS :: PP_Doc
              -- "build/ruler2/AS2/Pretty.ag"(line 112, column 21)
              _tlOisFirstAlt =
                  False
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 68, column 47)
              _lhsOgathMxAltWd =
                  _hdIgathMxAltWd `max` _tlIgathMxAltWd
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 67, column 55)
              _lhsOgathMxFldWd =
                  _hdIgathMxFldWd `max` _tlIgathMxFldWd
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 1, column 28)
              _lhsOpp =
                  _hdIpp >-< _tlIpp
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 104, column 35)
              _lhsOppHS =
                  _hdIppHS >-< _tlIppHS
              -- copy rule (down)
              _hdOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _hdOisFirstAlt =
                  _lhsIisFirstAlt
              -- copy rule (down)
              _hdOmkAltStr =
                  _lhsImkAltStr
              -- copy rule (down)
              _hdOmxAltWd =
                  _lhsImxAltWd
              -- copy rule (down)
              _hdOmxFldWd =
                  _lhsImxFldWd
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOppCfg =
                  _lhsIppCfg
              -- copy rule (down)
              _tlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _tlOmkAltStr =
                  _lhsImkAltStr
              -- copy rule (down)
              _tlOmxAltWd =
                  _lhsImxAltWd
              -- copy rule (down)
              _tlOmxFldWd =
                  _lhsImxFldWd
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOppCfg =
                  _lhsIppCfg
              ( _hdIgathMxAltWd,_hdIgathMxFldWd,_hdIpp,_hdIppHS) =
                  hd_ _hdOfmGam _hdOisFirstAlt _hdOmkAltStr _hdOmxAltWd _hdOmxFldWd _hdOopts _hdOppCfg 
              ( _tlIgathMxAltWd,_tlIgathMxFldWd,_tlIpp,_tlIppHS) =
                  tl_ _tlOfmGam _tlOisFirstAlt _tlOmkAltStr _tlOmxAltWd _tlOmxFldWd _tlOopts _tlOppCfg 
          in  ( _lhsOgathMxAltWd,_lhsOgathMxFldWd,_lhsOpp,_lhsOppHS)))
sem_DataAGAlts_Nil :: T_DataAGAlts 
sem_DataAGAlts_Nil  =
    (\ _lhsIfmGam
       _lhsIisFirstAlt
       _lhsImkAltStr
       _lhsImxAltWd
       _lhsImxFldWd
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOgathMxAltWd :: Int
              _lhsOgathMxFldWd :: Int
              _lhsOpp :: PP_Doc
              _lhsOppHS :: PP_Doc
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 68, column 47)
              _lhsOgathMxAltWd =
                  0
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 67, column 55)
              _lhsOgathMxFldWd =
                  0
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 1, column 28)
              _lhsOpp =
                  empty
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 104, column 35)
              _lhsOppHS =
                  empty
          in  ( _lhsOgathMxAltWd,_lhsOgathMxFldWd,_lhsOpp,_lhsOppHS)))
-- DataAGDecl --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
         ppCfg                : PPCfg
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Data:
         child ndNm           : {Nm}
         child alts           : DataAGAlts 
         visit 0:
            local nmStr       : _
            local mkAltStr    : _
-}
-- cata
sem_DataAGDecl :: DataAGDecl  ->
                  T_DataAGDecl 
sem_DataAGDecl (DataAGDecl_Data _ndNm _alts )  =
    (sem_DataAGDecl_Data _ndNm (sem_DataAGAlts _alts ) )
-- semantic domain
type T_DataAGDecl  = (FmGam Expr) ->
                     Opts ->
                     PPCfg ->
                     ( PP_Doc)
sem_DataAGDecl_Data :: Nm ->
                       T_DataAGAlts  ->
                       T_DataAGDecl 
sem_DataAGDecl_Data ndNm_ alts_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              _altsOmxAltWd :: Int
              _altsOmxFldWd :: Int
              _altsOisFirstAlt :: Bool
              _altsOfmGam :: (FmGam Expr)
              _altsOmkAltStr :: (String -> String)
              _altsOopts :: Opts
              _altsOppCfg :: PPCfg
              _altsIgathMxAltWd :: Int
              _altsIgathMxFldWd :: Int
              _altsIpp :: PP_Doc
              _altsIppHS :: PP_Doc
              -- "build/ruler2/AS2/Pretty.ag"(line 56, column 21)
              _lhsOpp =
                  case optGenFM _lhsIopts of
                    FmAG -> "DATA" >#< _nmStr >-< indent 2 _altsIpp
                    FmHS -> "data" >#< _nmStr >-< indent 2 _altsIppHS
                    _    -> empty
              -- "build/ruler2/AS2/Pretty.ag"(line 71, column 21)
              _altsOmxAltWd =
                  _altsIgathMxAltWd + atLhs2texDist
              -- "build/ruler2/AS2/Pretty.ag"(line 71, column 21)
              _altsOmxFldWd =
                  _altsIgathMxFldWd + atLhs2texDist
              -- "build/ruler2/AS2/Pretty.ag"(line 75, column 21)
              _nmStr =
                  show ndNm_
              -- "build/ruler2/AS2/Pretty.ag"(line 89, column 21)
              _mkAltStr =
                  case optGenFM _lhsIopts of
                    FmAG -> id
                    FmHS -> \n -> _nmStr ++ "_" ++ n
              -- "build/ruler2/AS2/Pretty.ag"(line 109, column 21)
              _altsOisFirstAlt =
                  True
              -- copy rule (down)
              _altsOfmGam =
                  _lhsIfmGam
              -- copy rule (from local)
              _altsOmkAltStr =
                  _mkAltStr
              -- copy rule (down)
              _altsOopts =
                  _lhsIopts
              -- copy rule (down)
              _altsOppCfg =
                  _lhsIppCfg
              ( _altsIgathMxAltWd,_altsIgathMxFldWd,_altsIpp,_altsIppHS) =
                  alts_ _altsOfmGam _altsOisFirstAlt _altsOmkAltStr _altsOmxAltWd _altsOmxFldWd _altsOopts _altsOppCfg 
          in  ( _lhsOpp)))
-- DataAGFld ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         isFirstFld           : Bool
         mkFldStr             : String -> String
         mxFldWd              : Int
         opts                 : Opts
         ppCfg                : PPCfg
      synthesized attributes:
         gathMxFldWd          : Int
         pp                   : PP_Doc
         ppHS                 : PP_Doc
   alternatives:
      alternative Fld:
         child nm             : {Nm}
         child ty             : {Ty}
         child tyIsData       : {Bool}
         visit 0:
            local nmStr       : _
-}
-- cata
sem_DataAGFld :: DataAGFld  ->
                 T_DataAGFld 
sem_DataAGFld (DataAGFld_Fld _nm _ty _tyIsData )  =
    (sem_DataAGFld_Fld _nm _ty _tyIsData )
-- semantic domain
type T_DataAGFld  = (FmGam Expr) ->
                    Bool ->
                    (String -> String) ->
                    Int ->
                    Opts ->
                    PPCfg ->
                    ( Int,PP_Doc,PP_Doc)
sem_DataAGFld_Fld :: Nm ->
                     Ty ->
                     Bool ->
                     T_DataAGFld 
sem_DataAGFld_Fld nm_ ty_ tyIsData_  =
    (\ _lhsIfmGam
       _lhsIisFirstFld
       _lhsImkFldStr
       _lhsImxFldWd
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              _lhsOgathMxFldWd :: Int
              _lhsOppHS :: PP_Doc
              -- "build/ruler2/AS2/Pretty.ag"(line 65, column 21)
              _lhsOpp =
                  strPad _nmStr _lhsImxFldWd >|< ":" >#< (if tyIsData_ then id else ppCurlys) (pp ty_)
              -- "build/ruler2/AS2/Pretty.ag"(line 82, column 21)
              _nmStr =
                  _lhsImkFldStr $ show $ nm_
              -- "build/ruler2/AS2/Pretty.ag"(line 83, column 21)
              _lhsOgathMxFldWd =
                  length _nmStr
              -- "build/ruler2/AS2/Pretty.ag"(line 102, column 21)
              _lhsOppHS =
                  (if _lhsIisFirstFld then "{" else ",") >#< strPad _nmStr _lhsImxFldWd >|< "::" >#< pp ty_
          in  ( _lhsOgathMxFldWd,_lhsOpp,_lhsOppHS)))
-- DataAGFlds --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         isFirstFld           : Bool
         mkFldStr             : String -> String
         mxFldWd              : Int
         opts                 : Opts
         ppCfg                : PPCfg
      synthesized attributes:
         gathMxFldWd          : Int
         pp                   : PP_Doc
         ppHS                 : PP_Doc
   alternatives:
      alternative Cons:
         child hd             : DataAGFld 
         child tl             : DataAGFlds 
      alternative Nil:
-}
-- cata
sem_DataAGFlds :: DataAGFlds  ->
                  T_DataAGFlds 
sem_DataAGFlds list  =
    (Prelude.foldr sem_DataAGFlds_Cons sem_DataAGFlds_Nil (Prelude.map sem_DataAGFld list) )
-- semantic domain
type T_DataAGFlds  = (FmGam Expr) ->
                     Bool ->
                     (String -> String) ->
                     Int ->
                     Opts ->
                     PPCfg ->
                     ( Int,PP_Doc,PP_Doc)
sem_DataAGFlds_Cons :: T_DataAGFld  ->
                       T_DataAGFlds  ->
                       T_DataAGFlds 
sem_DataAGFlds_Cons hd_ tl_  =
    (\ _lhsIfmGam
       _lhsIisFirstFld
       _lhsImkFldStr
       _lhsImxFldWd
       _lhsIopts
       _lhsIppCfg ->
         (let _tlOisFirstFld :: Bool
              _lhsOgathMxFldWd :: Int
              _lhsOpp :: PP_Doc
              _lhsOppHS :: PP_Doc
              _hdOfmGam :: (FmGam Expr)
              _hdOisFirstFld :: Bool
              _hdOmkFldStr :: (String -> String)
              _hdOmxFldWd :: Int
              _hdOopts :: Opts
              _hdOppCfg :: PPCfg
              _tlOfmGam :: (FmGam Expr)
              _tlOmkFldStr :: (String -> String)
              _tlOmxFldWd :: Int
              _tlOopts :: Opts
              _tlOppCfg :: PPCfg
              _hdIgathMxFldWd :: Int
              _hdIpp :: PP_Doc
              _hdIppHS :: PP_Doc
              _tlIgathMxFldWd :: Int
              _tlIpp :: PP_Doc
              _tlIppHS :: PP_Doc
              -- "build/ruler2/AS2/Pretty.ag"(line 118, column 21)
              _tlOisFirstFld =
                  False
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 67, column 55)
              _lhsOgathMxFldWd =
                  _hdIgathMxFldWd `max` _tlIgathMxFldWd
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 1, column 28)
              _lhsOpp =
                  _hdIpp >-< _tlIpp
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 104, column 35)
              _lhsOppHS =
                  _hdIppHS >-< _tlIppHS
              -- copy rule (down)
              _hdOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _hdOisFirstFld =
                  _lhsIisFirstFld
              -- copy rule (down)
              _hdOmkFldStr =
                  _lhsImkFldStr
              -- copy rule (down)
              _hdOmxFldWd =
                  _lhsImxFldWd
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOppCfg =
                  _lhsIppCfg
              -- copy rule (down)
              _tlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _tlOmkFldStr =
                  _lhsImkFldStr
              -- copy rule (down)
              _tlOmxFldWd =
                  _lhsImxFldWd
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOppCfg =
                  _lhsIppCfg
              ( _hdIgathMxFldWd,_hdIpp,_hdIppHS) =
                  hd_ _hdOfmGam _hdOisFirstFld _hdOmkFldStr _hdOmxFldWd _hdOopts _hdOppCfg 
              ( _tlIgathMxFldWd,_tlIpp,_tlIppHS) =
                  tl_ _tlOfmGam _tlOisFirstFld _tlOmkFldStr _tlOmxFldWd _tlOopts _tlOppCfg 
          in  ( _lhsOgathMxFldWd,_lhsOpp,_lhsOppHS)))
sem_DataAGFlds_Nil :: T_DataAGFlds 
sem_DataAGFlds_Nil  =
    (\ _lhsIfmGam
       _lhsIisFirstFld
       _lhsImkFldStr
       _lhsImxFldWd
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOgathMxFldWd :: Int
              _lhsOpp :: PP_Doc
              _lhsOppHS :: PP_Doc
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 67, column 55)
              _lhsOgathMxFldWd =
                  0
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 1, column 28)
              _lhsOpp =
                  empty
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 104, column 35)
              _lhsOppHS =
                  empty
          in  ( _lhsOgathMxFldWd,_lhsOpp,_lhsOppHS)))
-- Decl --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
         ppCfg                : PPCfg
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative AttrAG:
         child decl           : AttrAGDecl 
      alternative Chunk:
         child nm             : {Nm}
         child decl           : Decl 
      alternative DataAG:
         child decl           : DataAGDecl 
      alternative Preamble:
         child preamble       : {String}
      alternative RsVw:
         child decl           : RsVwDecl 
      alternative ScVwAtExplain:
         child atExprs        : {[(Expr,Expr)]}
      alternative ScVwExplain:
         child exExpr         : {Expr}
-}
-- cata
sem_Decl :: Decl  ->
            T_Decl 
sem_Decl (Decl_AttrAG _decl )  =
    (sem_Decl_AttrAG (sem_AttrAGDecl _decl ) )
sem_Decl (Decl_Chunk _nm _decl )  =
    (sem_Decl_Chunk _nm (sem_Decl _decl ) )
sem_Decl (Decl_DataAG _decl )  =
    (sem_Decl_DataAG (sem_DataAGDecl _decl ) )
sem_Decl (Decl_Preamble _preamble )  =
    (sem_Decl_Preamble _preamble )
sem_Decl (Decl_RsVw _decl )  =
    (sem_Decl_RsVw (sem_RsVwDecl _decl ) )
sem_Decl (Decl_ScVwAtExplain _atExprs )  =
    (sem_Decl_ScVwAtExplain _atExprs )
sem_Decl (Decl_ScVwExplain _exExpr )  =
    (sem_Decl_ScVwExplain _exExpr )
-- semantic domain
type T_Decl  = (FmGam Expr) ->
               Opts ->
               PPCfg ->
               ( PP_Doc)
sem_Decl_AttrAG :: T_AttrAGDecl  ->
                   T_Decl 
sem_Decl_AttrAG decl_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              _declOfmGam :: (FmGam Expr)
              _declOopts :: Opts
              _declOppCfg :: PPCfg
              _declIpp :: PP_Doc
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 1, column 28)
              _lhsOpp =
                  _declIpp
              -- copy rule (down)
              _declOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _declOopts =
                  _lhsIopts
              -- copy rule (down)
              _declOppCfg =
                  _lhsIppCfg
              ( _declIpp) =
                  decl_ _declOfmGam _declOopts _declOppCfg 
          in  ( _lhsOpp)))
sem_Decl_Chunk :: Nm ->
                  T_Decl  ->
                  T_Decl 
sem_Decl_Chunk nm_ decl_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              _declOfmGam :: (FmGam Expr)
              _declOopts :: Opts
              _declOppCfg :: PPCfg
              _declIpp :: PP_Doc
              -- "build/ruler2/AS2/Pretty.ag"(line 38, column 21)
              _lhsOpp =
                  ppWrapShuffle nm_ _declIpp
              -- copy rule (down)
              _declOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _declOopts =
                  _lhsIopts
              -- copy rule (down)
              _declOppCfg =
                  _lhsIppCfg
              ( _declIpp) =
                  decl_ _declOfmGam _declOopts _declOppCfg 
          in  ( _lhsOpp)))
sem_Decl_DataAG :: T_DataAGDecl  ->
                   T_Decl 
sem_Decl_DataAG decl_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              _declOfmGam :: (FmGam Expr)
              _declOopts :: Opts
              _declOppCfg :: PPCfg
              _declIpp :: PP_Doc
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 1, column 28)
              _lhsOpp =
                  _declIpp
              -- copy rule (down)
              _declOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _declOopts =
                  _lhsIopts
              -- copy rule (down)
              _declOppCfg =
                  _lhsIppCfg
              ( _declIpp) =
                  decl_ _declOfmGam _declOopts _declOppCfg 
          in  ( _lhsOpp)))
sem_Decl_Preamble :: String ->
                     T_Decl 
sem_Decl_Preamble preamble_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              -- "build/ruler2/AS2/Pretty.ag"(line 153, column 21)
              _lhsOpp =
                  pp preamble_
          in  ( _lhsOpp)))
sem_Decl_RsVw :: T_RsVwDecl  ->
                 T_Decl 
sem_Decl_RsVw decl_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              _declOfmGam :: (FmGam Expr)
              _declOopts :: Opts
              _declOppCfg :: PPCfg
              _declIpp :: PP_Doc
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 1, column 28)
              _lhsOpp =
                  _declIpp
              -- copy rule (down)
              _declOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _declOopts =
                  _lhsIopts
              -- copy rule (down)
              _declOppCfg =
                  _lhsIppCfg
              ( _declIpp) =
                  decl_ _declOfmGam _declOopts _declOppCfg 
          in  ( _lhsOpp)))
sem_Decl_ScVwAtExplain :: ([(Expr,Expr)]) ->
                          T_Decl 
sem_Decl_ScVwAtExplain atExprs_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              -- "build/ruler2/AS2/Pretty.ag"(line 157, column 21)
              _lhsOpp =
                  vlist $ map (\(n,e) -> exprFmtTeX _lhsIopts _lhsIfmGam n >#< "&" >#< exprFmtTeX _lhsIopts _lhsIfmGam e >#< "\\\\") $ atExprs_
          in  ( _lhsOpp)))
sem_Decl_ScVwExplain :: Expr ->
                        T_Decl 
sem_Decl_ScVwExplain exExpr_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              -- "build/ruler2/AS2/Pretty.ag"(line 156, column 21)
              _lhsOpp =
                  exprFmtTeX _lhsIopts _lhsIfmGam exExpr_
          in  ( _lhsOpp)))
-- Decls -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
         ppCfg                : PPCfg
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Cons:
         child hd             : Decl 
         child tl             : Decls 
      alternative Nil:
-}
-- cata
sem_Decls :: Decls  ->
             T_Decls 
sem_Decls list  =
    (Prelude.foldr sem_Decls_Cons sem_Decls_Nil (Prelude.map sem_Decl list) )
-- semantic domain
type T_Decls  = (FmGam Expr) ->
                Opts ->
                PPCfg ->
                ( PP_Doc)
sem_Decls_Cons :: T_Decl  ->
                  T_Decls  ->
                  T_Decls 
sem_Decls_Cons hd_ tl_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              _hdOfmGam :: (FmGam Expr)
              _hdOopts :: Opts
              _hdOppCfg :: PPCfg
              _tlOfmGam :: (FmGam Expr)
              _tlOopts :: Opts
              _tlOppCfg :: PPCfg
              _hdIpp :: PP_Doc
              _tlIpp :: PP_Doc
              -- "build/ruler2/AS2/Pretty.ag"(line 4, column 21)
              _lhsOpp =
                  _hdIpp >-< "" >-< _tlIpp
              -- copy rule (down)
              _hdOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOppCfg =
                  _lhsIppCfg
              -- copy rule (down)
              _tlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOppCfg =
                  _lhsIppCfg
              ( _hdIpp) =
                  hd_ _hdOfmGam _hdOopts _hdOppCfg 
              ( _tlIpp) =
                  tl_ _tlOfmGam _tlOopts _tlOppCfg 
          in  ( _lhsOpp)))
sem_Decls_Nil :: T_Decls 
sem_Decls_Nil  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 1, column 28)
              _lhsOpp =
                  empty
          in  ( _lhsOpp)))
-- ECnstr ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         fmGam                : FmGam Expr
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
type T_ECnstr  = (FmGam Expr) ->
                 ( PP_Doc,ECnstr )
sem_ECnstr_Empty :: T_ECnstr 
sem_ECnstr_Empty  =
    (\ _lhsIfmGam ->
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
          in  ( _lhsOpp,_lhsOself)))
sem_ECnstr_Ty :: ([Nm]) ->
                 T_ECnstr 
sem_ECnstr_Ty nms_  =
    (\ _lhsIfmGam ->
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
          in  ( _lhsOpp,_lhsOself)))
sem_ECnstr_Var :: Nm ->
                  T_ECnstr 
sem_ECnstr_Var nm_  =
    (\ _lhsIfmGam ->
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
          in  ( _lhsOpp,_lhsOself)))
-- Expr --------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         fmGam                : FmGam Expr
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
type T_Expr  = (FmGam Expr) ->
               ( PP_Doc,Expr )
sem_Expr_AVar :: T_ANm  ->
                 T_Expr 
sem_Expr_AVar anm_  =
    (\ _lhsIfmGam ->
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
          in  ( _lhsOpp,_lhsOself)))
sem_Expr_App :: T_Expr  ->
                T_Expr  ->
                T_Expr 
sem_Expr_App lExpr_ rExpr_  =
    (\ _lhsIfmGam ->
         (let _lhsOpp :: PP_Doc
              _lhsOself :: Expr 
              _lExprOfmGam :: (FmGam Expr)
              _rExprOfmGam :: (FmGam Expr)
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
              -- copy rule (down)
              _lExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _rExprOfmGam =
                  _lhsIfmGam
              ( _lExprIpp,_lExprIself) =
                  lExpr_ _lExprOfmGam 
              ( _rExprIpp,_rExprIself) =
                  rExpr_ _rExprOfmGam 
          in  ( _lhsOpp,_lhsOself)))
sem_Expr_AppTop :: T_Expr  ->
                   T_Expr 
sem_Expr_AppTop expr_  =
    (\ _lhsIfmGam ->
         (let _lhsOpp :: PP_Doc
              _lhsOself :: Expr 
              _exprOfmGam :: (FmGam Expr)
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
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              ( _exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOpp,_lhsOself)))
sem_Expr_ChildOrder :: Int ->
                       T_Expr  ->
                       T_Expr 
sem_Expr_ChildOrder seqNr_ expr_  =
    (\ _lhsIfmGam ->
         (let _lhsOpp :: PP_Doc
              _lhsOself :: Expr 
              _exprOfmGam :: (FmGam Expr)
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
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              ( _exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOpp,_lhsOself)))
sem_Expr_Cnstr :: T_Expr  ->
                  T_ECnstr  ->
                  T_Expr 
sem_Expr_Cnstr expr_ cnstr_  =
    (\ _lhsIfmGam ->
         (let _lhsOpp :: PP_Doc
              _lhsOself :: Expr 
              _exprOfmGam :: (FmGam Expr)
              _cnstrOfmGam :: (FmGam Expr)
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
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _cnstrOfmGam =
                  _lhsIfmGam
              ( _exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
              ( _cnstrIpp,_cnstrIself) =
                  cnstr_ _cnstrOfmGam 
          in  ( _lhsOpp,_lhsOself)))
sem_Expr_Empty :: T_Expr 
sem_Expr_Empty  =
    (\ _lhsIfmGam ->
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
          in  ( _lhsOpp,_lhsOself)))
sem_Expr_Expr :: T_Expr  ->
                 T_Expr 
sem_Expr_Expr expr_  =
    (\ _lhsIfmGam ->
         (let _lhsOpp :: PP_Doc
              _lhsOself :: Expr 
              _exprOfmGam :: (FmGam Expr)
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
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              ( _exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOpp,_lhsOself)))
sem_Expr_Int :: String ->
                T_Expr 
sem_Expr_Int int_  =
    (\ _lhsIfmGam ->
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
          in  ( _lhsOpp,_lhsOself)))
sem_Expr_LF :: T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_LF lExpr_ rExpr_  =
    (\ _lhsIfmGam ->
         (let _lhsOpp :: PP_Doc
              _lhsOself :: Expr 
              _lExprOfmGam :: (FmGam Expr)
              _rExprOfmGam :: (FmGam Expr)
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
              -- copy rule (down)
              _lExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _rExprOfmGam =
                  _lhsIfmGam
              ( _lExprIpp,_lExprIself) =
                  lExpr_ _lExprOfmGam 
              ( _rExprIpp,_rExprIself) =
                  rExpr_ _rExprOfmGam 
          in  ( _lhsOpp,_lhsOself)))
sem_Expr_Named :: Nm ->
                  T_Expr  ->
                  T_Expr 
sem_Expr_Named nm_ expr_  =
    (\ _lhsIfmGam ->
         (let _lhsOpp :: PP_Doc
              _lhsOself :: Expr 
              _exprOfmGam :: (FmGam Expr)
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
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              ( _exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOpp,_lhsOself)))
sem_Expr_Op :: Nm ->
               T_Expr  ->
               T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_Op nm_ nmExpr_ lExpr_ rExpr_  =
    (\ _lhsIfmGam ->
         (let _lhsOpp :: PP_Doc
              _lhsOself :: Expr 
              _nmExprOfmGam :: (FmGam Expr)
              _lExprOfmGam :: (FmGam Expr)
              _rExprOfmGam :: (FmGam Expr)
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
              -- copy rule (down)
              _nmExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _lExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _rExprOfmGam =
                  _lhsIfmGam
              ( _nmExprIpp,_nmExprIself) =
                  nmExpr_ _nmExprOfmGam 
              ( _lExprIpp,_lExprIself) =
                  lExpr_ _lExprOfmGam 
              ( _rExprIpp,_rExprIself) =
                  rExpr_ _rExprOfmGam 
          in  ( _lhsOpp,_lhsOself)))
sem_Expr_Paren :: T_Expr  ->
                  T_Expr 
sem_Expr_Paren expr_  =
    (\ _lhsIfmGam ->
         (let _lhsOpp :: PP_Doc
              _lhsOself :: Expr 
              _exprOfmGam :: (FmGam Expr)
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
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              ( _exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOpp,_lhsOself)))
sem_Expr_Retain :: T_Expr  ->
                   T_Expr 
sem_Expr_Retain expr_  =
    (\ _lhsIfmGam ->
         (let _lhsOpp :: PP_Doc
              _lhsOself :: Expr 
              _exprOfmGam :: (FmGam Expr)
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
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              ( _exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOpp,_lhsOself)))
sem_Expr_SP :: T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_SP lExpr_ rExpr_  =
    (\ _lhsIfmGam ->
         (let _lhsOpp :: PP_Doc
              _lhsOself :: Expr 
              _lExprOfmGam :: (FmGam Expr)
              _rExprOfmGam :: (FmGam Expr)
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
              -- copy rule (down)
              _lExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _rExprOfmGam =
                  _lhsIfmGam
              ( _lExprIpp,_lExprIself) =
                  lExpr_ _lExprOfmGam 
              ( _rExprIpp,_rExprIself) =
                  rExpr_ _rExprOfmGam 
          in  ( _lhsOpp,_lhsOself)))
sem_Expr_Sel :: T_Expr  ->
                T_MbExpr  ->
                T_Expr 
sem_Expr_Sel expr_ selMbExpr_  =
    (\ _lhsIfmGam ->
         (let _lhsOpp :: PP_Doc
              _lhsOself :: Expr 
              _exprOfmGam :: (FmGam Expr)
              _selMbExprOfmGam :: (FmGam Expr)
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
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _selMbExprOfmGam =
                  _lhsIfmGam
              ( _exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
              ( _selMbExprIpp,_selMbExprIself) =
                  selMbExpr_ _selMbExprOfmGam 
          in  ( _lhsOpp,_lhsOself)))
sem_Expr_SelTop :: T_Expr  ->
                   T_Expr 
sem_Expr_SelTop expr_  =
    (\ _lhsIfmGam ->
         (let _lhsOpp :: PP_Doc
              _lhsOself :: Expr 
              _exprOfmGam :: (FmGam Expr)
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
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              ( _exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOpp,_lhsOself)))
sem_Expr_StrAsIs :: String ->
                    T_Expr 
sem_Expr_StrAsIs str_  =
    (\ _lhsIfmGam ->
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
          in  ( _lhsOpp,_lhsOself)))
sem_Expr_StrText :: String ->
                    T_Expr 
sem_Expr_StrText str_  =
    (\ _lhsIfmGam ->
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
          in  ( _lhsOpp,_lhsOself)))
sem_Expr_Undefined :: T_Expr 
sem_Expr_Undefined  =
    (\ _lhsIfmGam ->
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
          in  ( _lhsOpp,_lhsOself)))
sem_Expr_Uniq :: T_Expr 
sem_Expr_Uniq  =
    (\ _lhsIfmGam ->
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
          in  ( _lhsOpp,_lhsOself)))
sem_Expr_Var :: Nm ->
                T_Expr 
sem_Expr_Var nm_  =
    (\ _lhsIfmGam ->
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
          in  ( _lhsOpp,_lhsOself)))
sem_Expr_Wrap :: WrKind ->
                 T_Expr  ->
                 T_Expr 
sem_Expr_Wrap wrKind_ expr_  =
    (\ _lhsIfmGam ->
         (let _lhsOpp :: PP_Doc
              _lhsOself :: Expr 
              _exprOfmGam :: (FmGam Expr)
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
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              ( _exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOpp,_lhsOself)))
sem_Expr_WrapCnstr :: T_ECnstr  ->
                      T_Expr 
sem_Expr_WrapCnstr cnstr_  =
    (\ _lhsIfmGam ->
         (let _lhsOpp :: PP_Doc
              _lhsOself :: Expr 
              _cnstrOfmGam :: (FmGam Expr)
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
              -- copy rule (down)
              _cnstrOfmGam =
                  _lhsIfmGam
              ( _cnstrIpp,_cnstrIself) =
                  cnstr_ _cnstrOfmGam 
          in  ( _lhsOpp,_lhsOself)))
-- Jd ----------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
         ppCfg                : PPCfg
      synthesized attributes:
         isSmall              : Bool
         pp                   : PP_Doc
   alternatives:
      alternative Ats:
         child nm             : {Nm}
         child scNm           : {Nm}
         child ats            : JdAts 
      alternative Expr:
         child nm             : {Nm}
         child scNm           : {Nm}
         child expr           : Expr 
         child isSmall        : {Bool}
      alternative LTX:
         child nm             : {Nm}
         child scNm           : {Nm}
         child expr           : Expr 
         child isSmall        : {Bool}
-}
-- cata
sem_Jd :: Jd  ->
          T_Jd 
sem_Jd (Jd_Ats _nm _scNm _ats )  =
    (sem_Jd_Ats _nm _scNm (sem_JdAts _ats ) )
sem_Jd (Jd_Expr _nm _scNm _expr _isSmall )  =
    (sem_Jd_Expr _nm _scNm (sem_Expr _expr ) _isSmall )
sem_Jd (Jd_LTX _nm _scNm _expr _isSmall )  =
    (sem_Jd_LTX _nm _scNm (sem_Expr _expr ) _isSmall )
-- semantic domain
type T_Jd  = (FmGam Expr) ->
             Opts ->
             PPCfg ->
             ( Bool,PP_Doc)
sem_Jd_Ats :: Nm ->
              Nm ->
              T_JdAts  ->
              T_Jd 
sem_Jd_Ats nm_ scNm_ ats_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              _lhsOisSmall :: Bool
              _atsOfmGam :: (FmGam Expr)
              _atsOopts :: Opts
              _atsOppCfg :: PPCfg
              _atsIpp :: PP_Doc
              -- "build/ruler2/AS2/Pretty.ag"(line 45, column 21)
              _lhsOpp =
                  nm_ >#< ppParens (pp scNm_) >|< ":" >#< _atsIpp
              -- "build/ruler2/AS2/Pretty.ag"(line 149, column 15)
              _lhsOisSmall =
                  False
              -- copy rule (down)
              _atsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _atsOopts =
                  _lhsIopts
              -- copy rule (down)
              _atsOppCfg =
                  _lhsIppCfg
              ( _atsIpp) =
                  ats_ _atsOfmGam _atsOopts _atsOppCfg 
          in  ( _lhsOisSmall,_lhsOpp)))
sem_Jd_Expr :: Nm ->
               Nm ->
               T_Expr  ->
               Bool ->
               T_Jd 
sem_Jd_Expr nm_ scNm_ expr_ isSmall_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOisSmall :: Bool
              _lhsOpp :: PP_Doc
              _exprOfmGam :: (FmGam Expr)
              _exprIpp :: PP_Doc
              _exprIself :: Expr 
              -- "build/ruler2/AS2/Pretty.ag"(line 148, column 15)
              _lhsOisSmall =
                  isSmall_
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 1, column 28)
              _lhsOpp =
                  _exprIpp
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              ( _exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOisSmall,_lhsOpp)))
sem_Jd_LTX :: Nm ->
              Nm ->
              T_Expr  ->
              Bool ->
              T_Jd 
sem_Jd_LTX nm_ scNm_ expr_ isSmall_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              _lhsOisSmall :: Bool
              _exprOfmGam :: (FmGam Expr)
              _exprIpp :: PP_Doc
              _exprIself :: Expr 
              -- "build/ruler2/AS2/Pretty.ag"(line 136, column 21)
              _lhsOpp =
                  switchLaTeXLhs (exprFmtTeX _lhsIopts _lhsIfmGam _exprIself)
              -- "build/ruler2/AS2/Pretty.ag"(line 148, column 15)
              _lhsOisSmall =
                  isSmall_
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              ( _exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOisSmall,_lhsOpp)))
-- JdAt --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
         ppCfg                : PPCfg
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative At:
         child nm             : {Nm}
         child expr           : Expr 
-}
-- cata
sem_JdAt :: JdAt  ->
            T_JdAt 
sem_JdAt (JdAt_At _nm _expr )  =
    (sem_JdAt_At _nm (sem_Expr _expr ) )
-- semantic domain
type T_JdAt  = (FmGam Expr) ->
               Opts ->
               PPCfg ->
               ( PP_Doc)
sem_JdAt_At :: Nm ->
               T_Expr  ->
               T_JdAt 
sem_JdAt_At nm_ expr_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              _exprOfmGam :: (FmGam Expr)
              _exprIpp :: PP_Doc
              _exprIself :: Expr 
              -- "build/ruler2/AS2/Pretty.ag"(line 48, column 21)
              _lhsOpp =
                  nm_ >#< "=" >#< _exprIpp
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              ( _exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOpp)))
-- JdAts -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
         ppCfg                : PPCfg
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Cons:
         child hd             : JdAt 
         child tl             : JdAts 
      alternative Nil:
-}
-- cata
sem_JdAts :: JdAts  ->
             T_JdAts 
sem_JdAts list  =
    (Prelude.foldr sem_JdAts_Cons sem_JdAts_Nil (Prelude.map sem_JdAt list) )
-- semantic domain
type T_JdAts  = (FmGam Expr) ->
                Opts ->
                PPCfg ->
                ( PP_Doc)
sem_JdAts_Cons :: T_JdAt  ->
                  T_JdAts  ->
                  T_JdAts 
sem_JdAts_Cons hd_ tl_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              _hdOfmGam :: (FmGam Expr)
              _hdOopts :: Opts
              _hdOppCfg :: PPCfg
              _tlOfmGam :: (FmGam Expr)
              _tlOopts :: Opts
              _tlOppCfg :: PPCfg
              _hdIpp :: PP_Doc
              _tlIpp :: PP_Doc
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 1, column 28)
              _lhsOpp =
                  _hdIpp >-< _tlIpp
              -- copy rule (down)
              _hdOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOppCfg =
                  _lhsIppCfg
              -- copy rule (down)
              _tlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOppCfg =
                  _lhsIppCfg
              ( _hdIpp) =
                  hd_ _hdOfmGam _hdOopts _hdOppCfg 
              ( _tlIpp) =
                  tl_ _tlOfmGam _tlOopts _tlOppCfg 
          in  ( _lhsOpp)))
sem_JdAts_Nil :: T_JdAts 
sem_JdAts_Nil  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 1, column 28)
              _lhsOpp =
                  empty
          in  ( _lhsOpp)))
-- Jds ---------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
         ppCfg                : PPCfg
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : Jd 
         child tl             : Jds 
      alternative Nil:
-}
-- cata
sem_Jds :: Jds  ->
           T_Jds 
sem_Jds list  =
    (Prelude.foldr sem_Jds_Cons sem_Jds_Nil (Prelude.map sem_Jd list) )
-- semantic domain
type T_Jds  = (FmGam Expr) ->
              Opts ->
              PPCfg ->
              ( PP_Doc,([PP_Doc]))
sem_Jds_Cons :: T_Jd  ->
                T_Jds  ->
                T_Jds 
sem_Jds_Cons hd_ tl_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOppL :: ([PP_Doc])
              _lhsOpp :: PP_Doc
              _hdOfmGam :: (FmGam Expr)
              _hdOopts :: Opts
              _hdOppCfg :: PPCfg
              _tlOfmGam :: (FmGam Expr)
              _tlOopts :: Opts
              _tlOppCfg :: PPCfg
              _hdIisSmall :: Bool
              _hdIpp :: PP_Doc
              _tlIpp :: PP_Doc
              _tlIppL :: ([PP_Doc])
              -- "build/ruler2/AS2/Pretty.ag"(line 142, column 21)
              _lhsOppL =
                  if _hdIisSmall && not (null _tlIppL)
                  then (_hdIpp >#< "\\hspace{2ex}" >#< head _tlIppL) : (tail _tlIppL)
                  else _hdIpp : _tlIppL
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 1, column 28)
              _lhsOpp =
                  _hdIpp >-< _tlIpp
              -- copy rule (down)
              _hdOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOppCfg =
                  _lhsIppCfg
              -- copy rule (down)
              _tlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOppCfg =
                  _lhsIppCfg
              ( _hdIisSmall,_hdIpp) =
                  hd_ _hdOfmGam _hdOopts _hdOppCfg 
              ( _tlIpp,_tlIppL) =
                  tl_ _tlOfmGam _tlOopts _tlOppCfg 
          in  ( _lhsOpp,_lhsOppL)))
sem_Jds_Nil :: T_Jds 
sem_Jds_Nil  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOppL :: ([PP_Doc])
              _lhsOpp :: PP_Doc
              -- "build/ruler2/AS2/Pretty.ag"(line 141, column 21)
              _lhsOppL =
                  []
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 1, column 28)
              _lhsOpp =
                  empty
          in  ( _lhsOpp,_lhsOppL)))
-- MbExpr ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         fmGam                : FmGam Expr
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
type T_MbExpr  = (FmGam Expr) ->
                 ( PP_Doc,MbExpr )
sem_MbExpr_Just :: T_Expr  ->
                   T_MbExpr 
sem_MbExpr_Just just_  =
    (\ _lhsIfmGam ->
         (let _lhsOpp :: PP_Doc
              _lhsOself :: MbExpr 
              _justOfmGam :: (FmGam Expr)
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
              -- copy rule (down)
              _justOfmGam =
                  _lhsIfmGam
              ( _justIpp,_justIself) =
                  just_ _justOfmGam 
          in  ( _lhsOpp,_lhsOself)))
sem_MbExpr_Nothing :: T_MbExpr 
sem_MbExpr_Nothing  =
    (\ _lhsIfmGam ->
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
          in  ( _lhsOpp,_lhsOself)))
-- RlDecl ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
         ppCfg                : PPCfg
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative AG:
         child nm             : {Nm}
         child pos            : {SPos}
         child arule          : ARule 
      alternative Chunk:
         child nm             : {Nm}
         child rl             : RlDecl 
      alternative LTX:
         child nm             : {Nm}
         child rlNm           : {Nm}
         child vwNm           : {Nm}
         child pos            : {SPos}
         child preJds         : Jds 
         child postJds        : Jds 
      alternative LTXAlias:
         child fullAliasNm    : {Nm}
         child fullNm         : {Nm}
      alternative Rl:
         child nm             : {Nm}
         child fullNm         : {Nm}
         child pos            : {SPos}
         child agStr          : {Nm}
         child preJds         : Jds 
         child postJds        : Jds 
-}
-- cata
sem_RlDecl :: RlDecl  ->
              T_RlDecl 
sem_RlDecl (RlDecl_AG _nm _pos _arule )  =
    (sem_RlDecl_AG _nm _pos (sem_ARule _arule ) )
sem_RlDecl (RlDecl_Chunk _nm _rl )  =
    (sem_RlDecl_Chunk _nm (sem_RlDecl _rl ) )
sem_RlDecl (RlDecl_LTX _nm _rlNm _vwNm _pos _preJds _postJds )  =
    (sem_RlDecl_LTX _nm _rlNm _vwNm _pos (sem_Jds _preJds ) (sem_Jds _postJds ) )
sem_RlDecl (RlDecl_LTXAlias _fullAliasNm _fullNm )  =
    (sem_RlDecl_LTXAlias _fullAliasNm _fullNm )
sem_RlDecl (RlDecl_Rl _nm _fullNm _pos _agStr _preJds _postJds )  =
    (sem_RlDecl_Rl _nm _fullNm _pos _agStr (sem_Jds _preJds ) (sem_Jds _postJds ) )
-- semantic domain
type T_RlDecl  = (FmGam Expr) ->
                 Opts ->
                 PPCfg ->
                 ( PP_Doc)
sem_RlDecl_AG :: Nm ->
                 SPos ->
                 T_ARule  ->
                 T_RlDecl 
sem_RlDecl_AG nm_ pos_ arule_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              _aruleOfmGam :: (FmGam Expr)
              _aruleIpp :: PP_Doc
              _aruleIself :: ARule 
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 1, column 28)
              _lhsOpp =
                  _aruleIpp
              -- copy rule (down)
              _aruleOfmGam =
                  _lhsIfmGam
              ( _aruleIpp,_aruleIself) =
                  arule_ _aruleOfmGam 
          in  ( _lhsOpp)))
sem_RlDecl_Chunk :: Nm ->
                    T_RlDecl  ->
                    T_RlDecl 
sem_RlDecl_Chunk nm_ rl_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              _rlOfmGam :: (FmGam Expr)
              _rlOopts :: Opts
              _rlOppCfg :: PPCfg
              _rlIpp :: PP_Doc
              -- "build/ruler2/AS2/Pretty.ag"(line 35, column 21)
              _lhsOpp =
                  ppWrapShuffle nm_ _rlIpp
              -- copy rule (down)
              _rlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _rlOopts =
                  _lhsIopts
              -- copy rule (down)
              _rlOppCfg =
                  _lhsIppCfg
              ( _rlIpp) =
                  rl_ _rlOfmGam _rlOopts _rlOppCfg 
          in  ( _lhsOpp)))
sem_RlDecl_LTX :: Nm ->
                  Nm ->
                  Nm ->
                  SPos ->
                  T_Jds  ->
                  T_Jds  ->
                  T_RlDecl 
sem_RlDecl_LTX nm_ rlNm_ vwNm_ pos_ preJds_ postJds_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              _preJdsOfmGam :: (FmGam Expr)
              _preJdsOopts :: Opts
              _preJdsOppCfg :: PPCfg
              _postJdsOfmGam :: (FmGam Expr)
              _postJdsOopts :: Opts
              _postJdsOppCfg :: PPCfg
              _preJdsIpp :: PP_Doc
              _preJdsIppL :: ([PP_Doc])
              _postJdsIpp :: PP_Doc
              _postJdsIppL :: ([PP_Doc])
              -- "build/ruler2/AS2/Pretty.ag"(line 121, column 21)
              _lhsOpp =
                  let r = "\\" >|< (pcRule _lhsIppCfg) >|< ppCurly rlNm_ >|< ppCurly vwNm_
                          >-< ppListSepVV "{%" "}" "\\\\" _preJdsIppL
                          >-< ppListSepVV "{%" "}" "\\\\" _postJdsIppL
                  in  mkTexCmdDef (pcDef _lhsIppCfg) nm_ r
              -- copy rule (down)
              _preJdsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _preJdsOopts =
                  _lhsIopts
              -- copy rule (down)
              _preJdsOppCfg =
                  _lhsIppCfg
              -- copy rule (down)
              _postJdsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _postJdsOopts =
                  _lhsIopts
              -- copy rule (down)
              _postJdsOppCfg =
                  _lhsIppCfg
              ( _preJdsIpp,_preJdsIppL) =
                  preJds_ _preJdsOfmGam _preJdsOopts _preJdsOppCfg 
              ( _postJdsIpp,_postJdsIppL) =
                  postJds_ _postJdsOfmGam _postJdsOopts _postJdsOppCfg 
          in  ( _lhsOpp)))
sem_RlDecl_LTXAlias :: Nm ->
                       Nm ->
                       T_RlDecl 
sem_RlDecl_LTXAlias fullAliasNm_ fullNm_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              -- "build/ruler2/AS2/Pretty.ag"(line 125, column 21)
              _lhsOpp =
                  mkTexCmdDef (pcDef _lhsIppCfg) fullAliasNm_ (mkTexCmdUse (pcUse _lhsIppCfg) fullNm_)
          in  ( _lhsOpp)))
sem_RlDecl_Rl :: Nm ->
                 Nm ->
                 SPos ->
                 Nm ->
                 T_Jds  ->
                 T_Jds  ->
                 T_RlDecl 
sem_RlDecl_Rl nm_ fullNm_ pos_ agStr_ preJds_ postJds_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              _preJdsOfmGam :: (FmGam Expr)
              _preJdsOopts :: Opts
              _preJdsOppCfg :: PPCfg
              _postJdsOfmGam :: (FmGam Expr)
              _postJdsOopts :: Opts
              _postJdsOppCfg :: PPCfg
              _preJdsIpp :: PP_Doc
              _preJdsIppL :: ([PP_Doc])
              _postJdsIpp :: PP_Doc
              _postJdsIppL :: ([PP_Doc])
              -- "build/ruler2/AS2/Pretty.ag"(line 41, column 21)
              _lhsOpp =
                  nm_ >#< ppBracketsCommas [pp pos_,pp agStr_]
                  >-< indent 2 (_preJdsIpp >-< "---" >-< _postJdsIpp)
              -- copy rule (down)
              _preJdsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _preJdsOopts =
                  _lhsIopts
              -- copy rule (down)
              _preJdsOppCfg =
                  _lhsIppCfg
              -- copy rule (down)
              _postJdsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _postJdsOopts =
                  _lhsIopts
              -- copy rule (down)
              _postJdsOppCfg =
                  _lhsIppCfg
              ( _preJdsIpp,_preJdsIppL) =
                  preJds_ _preJdsOfmGam _preJdsOopts _preJdsOppCfg 
              ( _postJdsIpp,_postJdsIppL) =
                  postJds_ _postJdsOfmGam _postJdsOopts _postJdsOppCfg 
          in  ( _lhsOpp)))
-- RlDecls -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
         ppCfg                : PPCfg
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Cons:
         child hd             : RlDecl 
         child tl             : RlDecls 
      alternative Nil:
-}
-- cata
sem_RlDecls :: RlDecls  ->
               T_RlDecls 
sem_RlDecls list  =
    (Prelude.foldr sem_RlDecls_Cons sem_RlDecls_Nil (Prelude.map sem_RlDecl list) )
-- semantic domain
type T_RlDecls  = (FmGam Expr) ->
                  Opts ->
                  PPCfg ->
                  ( PP_Doc)
sem_RlDecls_Cons :: T_RlDecl  ->
                    T_RlDecls  ->
                    T_RlDecls 
sem_RlDecls_Cons hd_ tl_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              _hdOfmGam :: (FmGam Expr)
              _hdOopts :: Opts
              _hdOppCfg :: PPCfg
              _tlOfmGam :: (FmGam Expr)
              _tlOopts :: Opts
              _tlOppCfg :: PPCfg
              _hdIpp :: PP_Doc
              _tlIpp :: PP_Doc
              -- "build/ruler2/AS2/Pretty.ag"(line 7, column 21)
              _lhsOpp =
                  _hdIpp >-< "" >-< _tlIpp
              -- copy rule (down)
              _hdOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOppCfg =
                  _lhsIppCfg
              -- copy rule (down)
              _tlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOppCfg =
                  _lhsIppCfg
              ( _hdIpp) =
                  hd_ _hdOfmGam _hdOopts _hdOppCfg 
              ( _tlIpp) =
                  tl_ _tlOfmGam _tlOopts _tlOppCfg 
          in  ( _lhsOpp)))
sem_RlDecls_Nil :: T_RlDecls 
sem_RlDecls_Nil  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 1, column 28)
              _lhsOpp =
                  empty
          in  ( _lhsOpp)))
-- RsVwDecl ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
         ppCfg                : PPCfg
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Rs:
         child nm             : {Nm}
         child scNm           : {Nm}
         child descr          : {String}
         child vwDecls        : VwDecls 
-}
-- cata
sem_RsVwDecl :: RsVwDecl  ->
                T_RsVwDecl 
sem_RsVwDecl (RsVwDecl_Rs _nm _scNm _descr _vwDecls )  =
    (sem_RsVwDecl_Rs _nm _scNm _descr (sem_VwDecls _vwDecls ) )
-- semantic domain
type T_RsVwDecl  = (FmGam Expr) ->
                   Opts ->
                   PPCfg ->
                   ( PP_Doc)
sem_RsVwDecl_Rs :: Nm ->
                   Nm ->
                   String ->
                   T_VwDecls  ->
                   T_RsVwDecl 
sem_RsVwDecl_Rs nm_ scNm_ descr_ vwDecls_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              _vwDeclsOfmGam :: (FmGam Expr)
              _vwDeclsOopts :: Opts
              _vwDeclsOppCfg :: PPCfg
              _vwDeclsIpp :: PP_Doc
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 1, column 28)
              _lhsOpp =
                  _vwDeclsIpp
              -- copy rule (down)
              _vwDeclsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _vwDeclsOopts =
                  _lhsIopts
              -- copy rule (down)
              _vwDeclsOppCfg =
                  _lhsIppCfg
              ( _vwDeclsIpp) =
                  vwDecls_ _vwDeclsOfmGam _vwDeclsOopts _vwDeclsOppCfg 
          in  ( _lhsOpp)))
-- RsVwDecls ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
         ppCfg                : PPCfg
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Cons:
         child hd             : RsVwDecl 
         child tl             : RsVwDecls 
      alternative Nil:
-}
-- cata
sem_RsVwDecls :: RsVwDecls  ->
                 T_RsVwDecls 
sem_RsVwDecls list  =
    (Prelude.foldr sem_RsVwDecls_Cons sem_RsVwDecls_Nil (Prelude.map sem_RsVwDecl list) )
-- semantic domain
type T_RsVwDecls  = (FmGam Expr) ->
                    Opts ->
                    PPCfg ->
                    ( PP_Doc)
sem_RsVwDecls_Cons :: T_RsVwDecl  ->
                      T_RsVwDecls  ->
                      T_RsVwDecls 
sem_RsVwDecls_Cons hd_ tl_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              _hdOfmGam :: (FmGam Expr)
              _hdOopts :: Opts
              _hdOppCfg :: PPCfg
              _tlOfmGam :: (FmGam Expr)
              _tlOopts :: Opts
              _tlOppCfg :: PPCfg
              _hdIpp :: PP_Doc
              _tlIpp :: PP_Doc
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 1, column 28)
              _lhsOpp =
                  _hdIpp >-< _tlIpp
              -- copy rule (down)
              _hdOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOppCfg =
                  _lhsIppCfg
              -- copy rule (down)
              _tlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOppCfg =
                  _lhsIppCfg
              ( _hdIpp) =
                  hd_ _hdOfmGam _hdOopts _hdOppCfg 
              ( _tlIpp) =
                  tl_ _tlOfmGam _tlOopts _tlOppCfg 
          in  ( _lhsOpp)))
sem_RsVwDecls_Nil :: T_RsVwDecls 
sem_RsVwDecls_Nil  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 1, column 28)
              _lhsOpp =
                  empty
          in  ( _lhsOpp)))
-- VwDecl ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
         ppCfg                : PPCfg
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Grp:
         child nm             : {Nm}
         child fullNm         : {Nm}
         child rlFullNmL      : {[(Nm,Nm)]}
      alternative LTX:
         child nm             : {Nm}
         child scMetaNm       : {Nm}
         child scmExpr        : Expr 
         child rlDecls        : RlDecls 
      alternative LTXFig:
         child nm             : {Nm}
         child fullNm         : {Nm}
         child scMetaNm       : {Nm}
         child descr          : {String}
         child rlFullNmL      : {[Nm]}
      alternative Vw:
         child nm             : {Nm}
         child fullNm         : {Nm}
         child rlDecls        : RlDecls 
-}
-- cata
sem_VwDecl :: VwDecl  ->
              T_VwDecl 
sem_VwDecl (VwDecl_Grp _nm _fullNm _rlFullNmL )  =
    (sem_VwDecl_Grp _nm _fullNm _rlFullNmL )
sem_VwDecl (VwDecl_LTX _nm _scMetaNm _scmExpr _rlDecls )  =
    (sem_VwDecl_LTX _nm _scMetaNm (sem_Expr _scmExpr ) (sem_RlDecls _rlDecls ) )
sem_VwDecl (VwDecl_LTXFig _nm _fullNm _scMetaNm _descr _rlFullNmL )  =
    (sem_VwDecl_LTXFig _nm _fullNm _scMetaNm _descr _rlFullNmL )
sem_VwDecl (VwDecl_Vw _nm _fullNm _rlDecls )  =
    (sem_VwDecl_Vw _nm _fullNm (sem_RlDecls _rlDecls ) )
-- semantic domain
type T_VwDecl  = (FmGam Expr) ->
                 Opts ->
                 PPCfg ->
                 ( PP_Doc)
sem_VwDecl_Grp :: Nm ->
                  Nm ->
                  ([(Nm,Nm)]) ->
                  T_VwDecl 
sem_VwDecl_Grp nm_ fullNm_ rlFullNmL_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 1, column 28)
              _lhsOpp =
                  empty
          in  ( _lhsOpp)))
sem_VwDecl_LTX :: Nm ->
                  Nm ->
                  T_Expr  ->
                  T_RlDecls  ->
                  T_VwDecl 
sem_VwDecl_LTX nm_ scMetaNm_ scmExpr_ rlDecls_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              _scmExprOfmGam :: (FmGam Expr)
              _rlDeclsOfmGam :: (FmGam Expr)
              _rlDeclsOopts :: Opts
              _rlDeclsOppCfg :: PPCfg
              _scmExprIpp :: PP_Doc
              _scmExprIself :: Expr 
              _rlDeclsIpp :: PP_Doc
              -- "build/ruler2/AS2/Pretty.ag"(line 128, column 21)
              _lhsOpp =
                  let meta = ensureTeXMath $ switchLaTeXLhs $ exprFmtTeX _lhsIopts _lhsIfmGam _scmExprIself
                  in  mkTexCmdDef (pcDef _lhsIppCfg) scMetaNm_ meta >-< _rlDeclsIpp
              -- copy rule (down)
              _scmExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _rlDeclsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _rlDeclsOopts =
                  _lhsIopts
              -- copy rule (down)
              _rlDeclsOppCfg =
                  _lhsIppCfg
              ( _scmExprIpp,_scmExprIself) =
                  scmExpr_ _scmExprOfmGam 
              ( _rlDeclsIpp) =
                  rlDecls_ _rlDeclsOfmGam _rlDeclsOopts _rlDeclsOppCfg 
          in  ( _lhsOpp)))
sem_VwDecl_LTXFig :: Nm ->
                     Nm ->
                     Nm ->
                     String ->
                     ([Nm]) ->
                     T_VwDecl 
sem_VwDecl_LTXFig nm_ fullNm_ scMetaNm_ descr_ rlFullNmL_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              -- "build/ruler2/AS2/Pretty.ag"(line 130, column 21)
              _lhsOpp =
                  let fig = "\\begin" >|< ppCurly (pcFigEnv _lhsIppCfg) >|< ppCurly (mkTexCmdUse (pcUse _lhsIppCfg) scMetaNm_) >|< ppCurly (pp descr_) >|< ppCurly fullNm_ >|< (ppCurly nm_)
                            >-< vlist (intersperse (pp "\\hspace{1ex}") . map (mkTexCmdUse (pcUse _lhsIppCfg)) $ rlFullNmL_)
                            >-< "\\end" >|< ppCurly (pcFigEnv _lhsIppCfg)
                  in  mkTexCmdDef (pcDef _lhsIppCfg) fullNm_ fig
          in  ( _lhsOpp)))
sem_VwDecl_Vw :: Nm ->
                 Nm ->
                 T_RlDecls  ->
                 T_VwDecl 
sem_VwDecl_Vw nm_ fullNm_ rlDecls_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              _rlDeclsOfmGam :: (FmGam Expr)
              _rlDeclsOopts :: Opts
              _rlDeclsOppCfg :: PPCfg
              _rlDeclsIpp :: PP_Doc
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 1, column 28)
              _lhsOpp =
                  _rlDeclsIpp
              -- copy rule (down)
              _rlDeclsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _rlDeclsOopts =
                  _lhsIopts
              -- copy rule (down)
              _rlDeclsOppCfg =
                  _lhsIppCfg
              ( _rlDeclsIpp) =
                  rlDecls_ _rlDeclsOfmGam _rlDeclsOopts _rlDeclsOppCfg 
          in  ( _lhsOpp)))
-- VwDecls -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
         ppCfg                : PPCfg
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Cons:
         child hd             : VwDecl 
         child tl             : VwDecls 
      alternative Nil:
-}
-- cata
sem_VwDecls :: VwDecls  ->
               T_VwDecls 
sem_VwDecls list  =
    (Prelude.foldr sem_VwDecls_Cons sem_VwDecls_Nil (Prelude.map sem_VwDecl list) )
-- semantic domain
type T_VwDecls  = (FmGam Expr) ->
                  Opts ->
                  PPCfg ->
                  ( PP_Doc)
sem_VwDecls_Cons :: T_VwDecl  ->
                    T_VwDecls  ->
                    T_VwDecls 
sem_VwDecls_Cons hd_ tl_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              _hdOfmGam :: (FmGam Expr)
              _hdOopts :: Opts
              _hdOppCfg :: PPCfg
              _tlOfmGam :: (FmGam Expr)
              _tlOopts :: Opts
              _tlOppCfg :: PPCfg
              _hdIpp :: PP_Doc
              _tlIpp :: PP_Doc
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 1, column 28)
              _lhsOpp =
                  _hdIpp >-< _tlIpp
              -- copy rule (down)
              _hdOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOppCfg =
                  _lhsIppCfg
              -- copy rule (down)
              _tlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOppCfg =
                  _lhsIppCfg
              ( _hdIpp) =
                  hd_ _hdOfmGam _hdOopts _hdOppCfg 
              ( _tlIpp) =
                  tl_ _tlOfmGam _tlOopts _tlOppCfg 
          in  ( _lhsOpp)))
sem_VwDecls_Nil :: T_VwDecls 
sem_VwDecls_Nil  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIppCfg ->
         (let _lhsOpp :: PP_Doc
              -- use rule "build/ruler2/AS2/Pretty.ag"(line 1, column 28)
              _lhsOpp =
                  empty
          in  ( _lhsOpp)))
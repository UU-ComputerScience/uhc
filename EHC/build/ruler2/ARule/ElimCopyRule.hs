

-- UUAGC 0.9.39.1 (build/ruler2/ARule/ElimCopyRule.ag)
module ARule.ElimCopyRule(module Gam, AtDefdGam, AtDefdGam', adGamUnion, ppADGam, CrOrdGam, arlElimCopyRule) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import EH.Util.Pretty
import Common
import Expr.Expr
import ARule.ARule
import Gam









arlElimCopyRule :: [Nm] -> AtDefdGam -> AtDefdGam' -> ARule -> ARule
arlElimCopyRule co ag ag2 rl
  = self_Syn_AGARuleItf r2
  where r1 = sem_AGARuleItf (AGARuleItf_AGItf rl)
        r2 = wrap_AGARuleItf r1
                (Inh_AGARuleItf {croNmL_Inh_AGARuleItf = co
                                ,adGam_Inh_AGARuleItf = ag
                                ,ad2Gam_Inh_AGARuleItf = ag2
                                })



type CrOrdGam = Gam Nm Nm



type AtDefdGam = Gam Nm Bool
type AtDefdGam' = Gam Nm (Set.Set Nm)

adGamUnion :: AtDefdGam' -> AtDefdGam' -> AtDefdGam'
adGamUnion = gamUnionWith Set.union

ppADGam :: AtDefdGam' -> PP_Doc
ppADGam = ppGam . gamMap (pp.show)

adGamIsThr :: AtDefdGam -> AtDefdGam' -> CrOrdGam -> Nm -> Nm -> Nm -> Bool
adGamIsThr ag1 ag2 cg
  = if gamIsEmpty ag2 then isThr1 else isThr2
  where isThr1 _    _    nAt = gamFindWithDefault False nAt ag1
        isThr2 nNd1 nNd2 nAt
          = case gamLookup nNd2 cg of
              Nothing -> False
              Just nNd1'
                | nNd1 == nNd1'
                  -> isComingOut
                | not isComingOut
                  -> isThr2 nNd1 nNd1' nAt
                | otherwise
                  -> False
                where isComingOut = maybe False (nNd1' `Set.member`) $ gamLookup nAt ag2

-- AEqn --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ad2Gam               : AtDefdGam'
         adGam                : AtDefdGam
         croGam               : CrOrdGam
      synthesized attributes:
         replCrEqns           : [AEqn]
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
type T_AEqn  = (AtDefdGam') ->
               AtDefdGam ->
               CrOrdGam ->
               ( ([AEqn]),AEqn )
sem_AEqn_Eqn :: T_AEqnDest  ->
                T_AExpr  ->
                T_AEqn 
sem_AEqn_Eqn dest_ val_  =
    (\ _lhsIad2Gam
       _lhsIadGam
       _lhsIcroGam ->
         (let _lhsOreplCrEqns :: ([AEqn])
              _destOisInComposite :: Bool
              _lhsOself :: AEqn 
              _destImbSingleANm :: (Maybe ANm)
              _destIself :: AEqnDest 
              _valImbSingleANm :: (Maybe ANm)
              _valIself :: AExpr 
              -- "build/ruler2/ARule/ElimCopyRule.ag"(line 80, column 21)
              _lhsOreplCrEqns =
                  let
                      isThr = adGamIsThr _lhsIadGam _lhsIad2Gam _lhsIcroGam
                      isPrev1 n1 n2 = maybe False (==n1) $ gamLookup n2 _lhsIcroGam
                      isPrev = if gamIsEmpty _lhsIad2Gam then isPrev1 else \_ _ -> True
                  in  case (_destImbSingleANm,_valImbSingleANm) of
                        (Just (ANm_Node nn dn),Just (ANm_Lhs sn _)) | gamIsEmpty _lhsIad2Gam && dn == sn && not (isThr nmLhs nn dn)
                          -> []
                        (Just (ANm_Node nn dn),Just (ANm_Lhs sn _)) | dn == sn && (isThr nmLhs nn dn) && nmLhs `isPrev` nn
                          -> []
                        (Just (ANm_Node nn1 dn),Just (ANm_Node nn2 sn)) | dn == sn && (isThr nn2 nn1 dn) && nn2 `isPrev` nn1
                          -> []
                        (Just (ANm_Lhs dn _),Just (ANm_Node nn sn)) | dn == sn                     && nn `isPrev` nmLhs
                          -> []
                        (Just (ANm_Lhs dn _),Just (ANm_Lhs sn _)) | dn == sn && (isThr nmLhs nmLhs dn) && nmLhs `isPrev` nmLhs
                          -> []
                        _ -> [_self]
              -- "build/ruler2/ARule/InCompDestAG.ag"(line 5, column 21)
              _destOisInComposite =
                  False
              -- self rule
              _self =
                  AEqn_Eqn _destIself _valIself
              -- self rule
              _lhsOself =
                  _self
              ( _destImbSingleANm,_destIself) =
                  dest_ _destOisInComposite 
              ( _valImbSingleANm,_valIself) =
                  val_ 
          in  ( _lhsOreplCrEqns,_lhsOself)))
sem_AEqn_Err :: T_Expr  ->
                T_AEqn 
sem_AEqn_Err expr_  =
    (\ _lhsIad2Gam
       _lhsIadGam
       _lhsIcroGam ->
         (let _lhsOreplCrEqns :: ([AEqn])
              _lhsOself :: AEqn 
              _exprImbSingleANm :: (Maybe ANm)
              _exprIself :: Expr 
              -- "build/ruler2/ARule/ElimCopyRule.ag"(line 96, column 21)
              _lhsOreplCrEqns =
                  [_self]
              -- self rule
              _self =
                  AEqn_Err _exprIself
              -- self rule
              _lhsOself =
                  _self
              ( _exprImbSingleANm,_exprIself) =
                  expr_ 
          in  ( _lhsOreplCrEqns,_lhsOself)))
-- AEqnDest ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         isInComposite        : Bool
      synthesized attributes:
         mbSingleANm          : Maybe ANm
         self                 : SELF 
   alternatives:
      alternative Many:
         child dests          : AEqnDests 
         visit 0:
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
type T_AEqnDest  = Bool ->
                   ( (Maybe ANm),AEqnDest )
sem_AEqnDest_Many :: T_AEqnDests  ->
                     T_AEqnDest 
sem_AEqnDest_Many dests_  =
    (\ _lhsIisInComposite ->
         (let _lhsOmbSingleANm :: (Maybe ANm)
              _destsOisInComposite :: Bool
              _lhsOself :: AEqnDest 
              _destsIself :: AEqnDests 
              -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 10, column 21)
              _lhsOmbSingleANm =
                  Nothing
              -- "build/ruler2/ARule/InCompDestAG.ag"(line 9, column 21)
              _destsOisInComposite =
                  True
              -- self rule
              _self =
                  AEqnDest_Many _destsIself
              -- self rule
              _lhsOself =
                  _self
              ( _destsIself) =
                  dests_ _destsOisInComposite 
          in  ( _lhsOmbSingleANm,_lhsOself)))
sem_AEqnDest_One :: T_ANm  ->
                    T_AEqnDest 
sem_AEqnDest_One anm_  =
    (\ _lhsIisInComposite ->
         (let _lhsOmbSingleANm :: (Maybe ANm)
              _lhsOself :: AEqnDest 
              _anmIself :: ANm 
              -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 9, column 21)
              _lhsOmbSingleANm =
                  if _lhsIisInComposite then Nothing else Just _anmIself
              -- self rule
              _self =
                  AEqnDest_One _anmIself
              -- self rule
              _lhsOself =
                  _self
              ( _anmIself) =
                  anm_ 
          in  ( _lhsOmbSingleANm,_lhsOself)))
-- AEqnDests ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         isInComposite        : Bool
      synthesized attribute:
         self                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : AEqnDest 
         child tl             : AEqnDests 
         visit 0:
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
type T_AEqnDests  = Bool ->
                    ( AEqnDests )
sem_AEqnDests_Cons :: T_AEqnDest  ->
                      T_AEqnDests  ->
                      T_AEqnDests 
sem_AEqnDests_Cons hd_ tl_  =
    (\ _lhsIisInComposite ->
         (let _lhsOself :: AEqnDests 
              _hdOisInComposite :: Bool
              _tlOisInComposite :: Bool
              _hdImbSingleANm :: (Maybe ANm)
              _hdIself :: AEqnDest 
              _tlIself :: AEqnDests 
              -- self rule
              _self =
                  (:) _hdIself _tlIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _hdOisInComposite =
                  _lhsIisInComposite
              -- copy rule (down)
              _tlOisInComposite =
                  _lhsIisInComposite
              ( _hdImbSingleANm,_hdIself) =
                  hd_ _hdOisInComposite 
              ( _tlIself) =
                  tl_ _tlOisInComposite 
          in  ( _lhsOself)))
sem_AEqnDests_Nil :: T_AEqnDests 
sem_AEqnDests_Nil  =
    (\ _lhsIisInComposite ->
         (let _lhsOself :: AEqnDests 
              -- self rule
              _self =
                  []
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOself)))
-- AEqns -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ad2Gam               : AtDefdGam'
         adGam                : AtDefdGam
         croGam               : CrOrdGam
      synthesized attributes:
         replCrEqns           : [AEqn]
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
type T_AEqns  = (AtDefdGam') ->
                AtDefdGam ->
                CrOrdGam ->
                ( ([AEqn]),AEqns )
sem_AEqns_Cons :: T_AEqn  ->
                  T_AEqns  ->
                  T_AEqns 
sem_AEqns_Cons hd_ tl_  =
    (\ _lhsIad2Gam
       _lhsIadGam
       _lhsIcroGam ->
         (let _lhsOreplCrEqns :: ([AEqn])
              _lhsOself :: AEqns 
              _hdOad2Gam :: (AtDefdGam')
              _hdOadGam :: AtDefdGam
              _hdOcroGam :: CrOrdGam
              _tlOad2Gam :: (AtDefdGam')
              _tlOadGam :: AtDefdGam
              _tlOcroGam :: CrOrdGam
              _hdIreplCrEqns :: ([AEqn])
              _hdIself :: AEqn 
              _tlIreplCrEqns :: ([AEqn])
              _tlIself :: AEqns 
              -- use rule "build/ruler2/ARule/ElimCopyRule.ag"(line 77, column 34)
              _lhsOreplCrEqns =
                  _hdIreplCrEqns ++ _tlIreplCrEqns
              -- self rule
              _self =
                  (:) _hdIself _tlIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _hdOad2Gam =
                  _lhsIad2Gam
              -- copy rule (down)
              _hdOadGam =
                  _lhsIadGam
              -- copy rule (down)
              _hdOcroGam =
                  _lhsIcroGam
              -- copy rule (down)
              _tlOad2Gam =
                  _lhsIad2Gam
              -- copy rule (down)
              _tlOadGam =
                  _lhsIadGam
              -- copy rule (down)
              _tlOcroGam =
                  _lhsIcroGam
              ( _hdIreplCrEqns,_hdIself) =
                  hd_ _hdOad2Gam _hdOadGam _hdOcroGam 
              ( _tlIreplCrEqns,_tlIself) =
                  tl_ _tlOad2Gam _tlOadGam _tlOcroGam 
          in  ( _lhsOreplCrEqns,_lhsOself)))
sem_AEqns_Nil :: T_AEqns 
sem_AEqns_Nil  =
    (\ _lhsIad2Gam
       _lhsIadGam
       _lhsIcroGam ->
         (let _lhsOreplCrEqns :: ([AEqn])
              _lhsOself :: AEqns 
              -- use rule "build/ruler2/ARule/ElimCopyRule.ag"(line 77, column 34)
              _lhsOreplCrEqns =
                  []
              -- self rule
              _self =
                  []
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOreplCrEqns,_lhsOself)))
-- AExpr -------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         mbSingleANm          : Maybe ANm
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
type T_AExpr  = ( (Maybe ANm),AExpr )
sem_AExpr_Expr :: T_Expr  ->
                  T_AExpr 
sem_AExpr_Expr expr_  =
    (let _lhsOself :: AExpr 
         _lhsOmbSingleANm :: (Maybe ANm)
         _exprImbSingleANm :: (Maybe ANm)
         _exprIself :: Expr 
         -- self rule
         _self =
             AExpr_Expr _exprIself
         -- self rule
         _lhsOself =
             _self
         -- copy rule (up)
         _lhsOmbSingleANm =
             _exprImbSingleANm
         ( _exprImbSingleANm,_exprIself) =
             expr_ 
     in  ( _lhsOmbSingleANm,_lhsOself))
-- AGARuleItf --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ad2Gam               : AtDefdGam'
         adGam                : AtDefdGam
         croNmL               : [Nm]
      synthesized attribute:
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
type T_AGARuleItf  = (AtDefdGam') ->
                     AtDefdGam ->
                     ([Nm]) ->
                     ( ARule )
data Inh_AGARuleItf  = Inh_AGARuleItf {ad2Gam_Inh_AGARuleItf :: (AtDefdGam'),adGam_Inh_AGARuleItf :: AtDefdGam,croNmL_Inh_AGARuleItf :: ([Nm])}
data Syn_AGARuleItf  = Syn_AGARuleItf {self_Syn_AGARuleItf :: ARule }
wrap_AGARuleItf :: T_AGARuleItf  ->
                   Inh_AGARuleItf  ->
                   Syn_AGARuleItf 
wrap_AGARuleItf sem (Inh_AGARuleItf _lhsIad2Gam _lhsIadGam _lhsIcroNmL )  =
    (let ( _lhsOself) = sem _lhsIad2Gam _lhsIadGam _lhsIcroNmL 
     in  (Syn_AGARuleItf _lhsOself ))
sem_AGARuleItf_AGItf :: T_ARule  ->
                        T_AGARuleItf 
sem_AGARuleItf_AGItf rule_  =
    (\ _lhsIad2Gam
       _lhsIadGam
       _lhsIcroNmL ->
         (let _ruleOcroGam :: CrOrdGam
              _lhsOself :: ARule 
              _ruleOad2Gam :: (AtDefdGam')
              _ruleOadGam :: AtDefdGam
              _ruleIself :: ARule 
              -- "build/ruler2/ARule/ElimCopyRule.ag"(line 75, column 21)
              _ruleOcroGam =
                  fst . foldl (\(g,pn) n -> (gamInsert n pn g,n)) (emptyGam,head _lhsIcroNmL) $ tail _lhsIcroNmL
              -- copy rule (up)
              _lhsOself =
                  _ruleIself
              -- copy rule (down)
              _ruleOad2Gam =
                  _lhsIad2Gam
              -- copy rule (down)
              _ruleOadGam =
                  _lhsIadGam
              ( _ruleIself) =
                  rule_ _ruleOad2Gam _ruleOadGam _ruleOcroGam 
          in  ( _lhsOself)))
-- AGExprItf ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
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
type T_AGExprItf  = ( Expr )
sem_AGExprItf_AGItf :: T_Expr  ->
                       T_AGExprItf 
sem_AGExprItf_AGItf expr_  =
    (let _lhsOself :: Expr 
         _exprImbSingleANm :: (Maybe ANm)
         _exprIself :: Expr 
         -- copy rule (up)
         _lhsOself =
             _exprIself
         ( _exprImbSingleANm,_exprIself) =
             expr_ 
     in  ( _lhsOself))
-- ANm ---------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : SELF 
   alternatives:
      alternative Fld:
         child nm             : {Nm}
         visit 0:
            local self        : _
      alternative Lhs:
         child nm             : {Nm}
         child props          : {[AtProp]}
         visit 0:
            local self        : _
      alternative Loc:
         child nm             : {Nm}
         child props          : {[AtProp]}
         visit 0:
            local self        : _
      alternative Node:
         child ndNm           : {Nm}
         child nm             : {Nm}
         visit 0:
            local self        : _
      alternative Wild:
         visit 0:
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
type T_ANm  = ( ANm )
sem_ANm_Fld :: Nm ->
               T_ANm 
sem_ANm_Fld nm_  =
    (let _lhsOself :: ANm 
         -- self rule
         _self =
             ANm_Fld nm_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ANm_Lhs :: Nm ->
               ([AtProp]) ->
               T_ANm 
sem_ANm_Lhs nm_ props_  =
    (let _lhsOself :: ANm 
         -- self rule
         _self =
             ANm_Lhs nm_ props_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ANm_Loc :: Nm ->
               ([AtProp]) ->
               T_ANm 
sem_ANm_Loc nm_ props_  =
    (let _lhsOself :: ANm 
         -- self rule
         _self =
             ANm_Loc nm_ props_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ANm_Node :: Nm ->
                Nm ->
                T_ANm 
sem_ANm_Node ndNm_ nm_  =
    (let _lhsOself :: ANm 
         -- self rule
         _self =
             ANm_Node ndNm_ nm_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ANm_Wild :: T_ANm 
sem_ANm_Wild  =
    (let _lhsOself :: ANm 
         -- self rule
         _self =
             ANm_Wild
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
-- ARule -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ad2Gam               : AtDefdGam'
         adGam                : AtDefdGam
         croGam               : CrOrdGam
      synthesized attribute:
         self                 : SELF 
   alternatives:
      alternative Rule:
         child ndNmL          : {[Nm]}
         child rlNm           : {Nm}
         child info           : {[String]}
         child eqns           : AEqns 
         visit 0:
            local self        : _
-}
-- cata
sem_ARule :: ARule  ->
             T_ARule 
sem_ARule (ARule_Rule _ndNmL _rlNm _info _eqns )  =
    (sem_ARule_Rule _ndNmL _rlNm _info (sem_AEqns _eqns ) )
-- semantic domain
type T_ARule  = (AtDefdGam') ->
                AtDefdGam ->
                CrOrdGam ->
                ( ARule )
sem_ARule_Rule :: ([Nm]) ->
                  Nm ->
                  ([String]) ->
                  T_AEqns  ->
                  T_ARule 
sem_ARule_Rule ndNmL_ rlNm_ info_ eqns_  =
    (\ _lhsIad2Gam
       _lhsIadGam
       _lhsIcroGam ->
         (let _lhsOself :: ARule 
              _eqnsOad2Gam :: (AtDefdGam')
              _eqnsOadGam :: AtDefdGam
              _eqnsOcroGam :: CrOrdGam
              _eqnsIreplCrEqns :: ([AEqn])
              _eqnsIself :: AEqns 
              -- "build/ruler2/ARule/ElimCopyRule.ag"(line 99, column 21)
              _lhsOself =
                  ARule_Rule ndNmL_ rlNm_ info_ _eqnsIreplCrEqns
              -- self rule
              _self =
                  ARule_Rule ndNmL_ rlNm_ info_ _eqnsIself
              -- copy rule (down)
              _eqnsOad2Gam =
                  _lhsIad2Gam
              -- copy rule (down)
              _eqnsOadGam =
                  _lhsIadGam
              -- copy rule (down)
              _eqnsOcroGam =
                  _lhsIcroGam
              ( _eqnsIreplCrEqns,_eqnsIself) =
                  eqns_ _eqnsOad2Gam _eqnsOadGam _eqnsOcroGam 
          in  ( _lhsOself)))
-- ARules ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ad2Gam               : AtDefdGam'
         adGam                : AtDefdGam
         croGam               : CrOrdGam
      synthesized attribute:
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
type T_ARules  = (AtDefdGam') ->
                 AtDefdGam ->
                 CrOrdGam ->
                 ( ARules )
sem_ARules_Cons :: T_ARule  ->
                   T_ARules  ->
                   T_ARules 
sem_ARules_Cons hd_ tl_  =
    (\ _lhsIad2Gam
       _lhsIadGam
       _lhsIcroGam ->
         (let _lhsOself :: ARules 
              _hdOad2Gam :: (AtDefdGam')
              _hdOadGam :: AtDefdGam
              _hdOcroGam :: CrOrdGam
              _tlOad2Gam :: (AtDefdGam')
              _tlOadGam :: AtDefdGam
              _tlOcroGam :: CrOrdGam
              _hdIself :: ARule 
              _tlIself :: ARules 
              -- self rule
              _self =
                  (:) _hdIself _tlIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _hdOad2Gam =
                  _lhsIad2Gam
              -- copy rule (down)
              _hdOadGam =
                  _lhsIadGam
              -- copy rule (down)
              _hdOcroGam =
                  _lhsIcroGam
              -- copy rule (down)
              _tlOad2Gam =
                  _lhsIad2Gam
              -- copy rule (down)
              _tlOadGam =
                  _lhsIadGam
              -- copy rule (down)
              _tlOcroGam =
                  _lhsIcroGam
              ( _hdIself) =
                  hd_ _hdOad2Gam _hdOadGam _hdOcroGam 
              ( _tlIself) =
                  tl_ _tlOad2Gam _tlOadGam _tlOcroGam 
          in  ( _lhsOself)))
sem_ARules_Nil :: T_ARules 
sem_ARules_Nil  =
    (\ _lhsIad2Gam
       _lhsIadGam
       _lhsIcroGam ->
         (let _lhsOself :: ARules 
              -- self rule
              _self =
                  []
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOself)))
-- ECnstr ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
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
type T_ECnstr  = ( ECnstr )
sem_ECnstr_Empty :: T_ECnstr 
sem_ECnstr_Empty  =
    (let _lhsOself :: ECnstr 
         -- self rule
         _self =
             ECnstr_Empty
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ECnstr_Ty :: ([Nm]) ->
                 T_ECnstr 
sem_ECnstr_Ty nms_  =
    (let _lhsOself :: ECnstr 
         -- self rule
         _self =
             ECnstr_Ty nms_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ECnstr_Var :: Nm ->
                  T_ECnstr 
sem_ECnstr_Var nm_  =
    (let _lhsOself :: ECnstr 
         -- self rule
         _self =
             ECnstr_Var nm_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Expr --------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         mbSingleANm          : Maybe ANm
         self                 : SELF 
   alternatives:
      alternative AVar:
         child anm            : ANm 
         visit 0:
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
type T_Expr  = ( (Maybe ANm),Expr )
sem_Expr_AVar :: T_ANm  ->
                 T_Expr 
sem_Expr_AVar anm_  =
    (let _lhsOmbSingleANm :: (Maybe ANm)
         _lhsOself :: Expr 
         _anmIself :: ANm 
         -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 19, column 21)
         _lhsOmbSingleANm =
             Just _anmIself
         -- self rule
         _self =
             Expr_AVar _anmIself
         -- self rule
         _lhsOself =
             _self
         ( _anmIself) =
             anm_ 
     in  ( _lhsOmbSingleANm,_lhsOself))
sem_Expr_App :: T_Expr  ->
                T_Expr  ->
                T_Expr 
sem_Expr_App lExpr_ rExpr_  =
    (let _lhsOmbSingleANm :: (Maybe ANm)
         _lhsOself :: Expr 
         _lExprImbSingleANm :: (Maybe ANm)
         _lExprIself :: Expr 
         _rExprImbSingleANm :: (Maybe ANm)
         _rExprIself :: Expr 
         -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
         _lhsOmbSingleANm =
             Nothing
         -- self rule
         _self =
             Expr_App _lExprIself _rExprIself
         -- self rule
         _lhsOself =
             _self
         ( _lExprImbSingleANm,_lExprIself) =
             lExpr_ 
         ( _rExprImbSingleANm,_rExprIself) =
             rExpr_ 
     in  ( _lhsOmbSingleANm,_lhsOself))
sem_Expr_AppTop :: T_Expr  ->
                   T_Expr 
sem_Expr_AppTop expr_  =
    (let _lhsOself :: Expr 
         _lhsOmbSingleANm :: (Maybe ANm)
         _exprImbSingleANm :: (Maybe ANm)
         _exprIself :: Expr 
         -- self rule
         _self =
             Expr_AppTop _exprIself
         -- self rule
         _lhsOself =
             _self
         -- copy rule (up)
         _lhsOmbSingleANm =
             _exprImbSingleANm
         ( _exprImbSingleANm,_exprIself) =
             expr_ 
     in  ( _lhsOmbSingleANm,_lhsOself))
sem_Expr_ChildOrder :: Int ->
                       T_Expr  ->
                       T_Expr 
sem_Expr_ChildOrder seqNr_ expr_  =
    (let _lhsOmbSingleANm :: (Maybe ANm)
         _lhsOself :: Expr 
         _exprImbSingleANm :: (Maybe ANm)
         _exprIself :: Expr 
         -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
         _lhsOmbSingleANm =
             Nothing
         -- self rule
         _self =
             Expr_ChildOrder seqNr_ _exprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprImbSingleANm,_exprIself) =
             expr_ 
     in  ( _lhsOmbSingleANm,_lhsOself))
sem_Expr_Cnstr :: T_Expr  ->
                  T_ECnstr  ->
                  T_Expr 
sem_Expr_Cnstr expr_ cnstr_  =
    (let _lhsOmbSingleANm :: (Maybe ANm)
         _lhsOself :: Expr 
         _exprImbSingleANm :: (Maybe ANm)
         _exprIself :: Expr 
         _cnstrIself :: ECnstr 
         -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
         _lhsOmbSingleANm =
             Nothing
         -- self rule
         _self =
             Expr_Cnstr _exprIself _cnstrIself
         -- self rule
         _lhsOself =
             _self
         ( _exprImbSingleANm,_exprIself) =
             expr_ 
         ( _cnstrIself) =
             cnstr_ 
     in  ( _lhsOmbSingleANm,_lhsOself))
sem_Expr_Empty :: T_Expr 
sem_Expr_Empty  =
    (let _lhsOmbSingleANm :: (Maybe ANm)
         _lhsOself :: Expr 
         -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
         _lhsOmbSingleANm =
             Nothing
         -- self rule
         _self =
             Expr_Empty
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOmbSingleANm,_lhsOself))
sem_Expr_Expr :: T_Expr  ->
                 T_Expr 
sem_Expr_Expr expr_  =
    (let _lhsOmbSingleANm :: (Maybe ANm)
         _lhsOself :: Expr 
         _exprImbSingleANm :: (Maybe ANm)
         _exprIself :: Expr 
         -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
         _lhsOmbSingleANm =
             Nothing
         -- self rule
         _self =
             Expr_Expr _exprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprImbSingleANm,_exprIself) =
             expr_ 
     in  ( _lhsOmbSingleANm,_lhsOself))
sem_Expr_Int :: String ->
                T_Expr 
sem_Expr_Int int_  =
    (let _lhsOmbSingleANm :: (Maybe ANm)
         _lhsOself :: Expr 
         -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
         _lhsOmbSingleANm =
             Nothing
         -- self rule
         _self =
             Expr_Int int_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOmbSingleANm,_lhsOself))
sem_Expr_LF :: T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_LF lExpr_ rExpr_  =
    (let _lhsOmbSingleANm :: (Maybe ANm)
         _lhsOself :: Expr 
         _lExprImbSingleANm :: (Maybe ANm)
         _lExprIself :: Expr 
         _rExprImbSingleANm :: (Maybe ANm)
         _rExprIself :: Expr 
         -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
         _lhsOmbSingleANm =
             Nothing
         -- self rule
         _self =
             Expr_LF _lExprIself _rExprIself
         -- self rule
         _lhsOself =
             _self
         ( _lExprImbSingleANm,_lExprIself) =
             lExpr_ 
         ( _rExprImbSingleANm,_rExprIself) =
             rExpr_ 
     in  ( _lhsOmbSingleANm,_lhsOself))
sem_Expr_Named :: Nm ->
                  T_Expr  ->
                  T_Expr 
sem_Expr_Named nm_ expr_  =
    (let _lhsOmbSingleANm :: (Maybe ANm)
         _lhsOself :: Expr 
         _exprImbSingleANm :: (Maybe ANm)
         _exprIself :: Expr 
         -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
         _lhsOmbSingleANm =
             Nothing
         -- self rule
         _self =
             Expr_Named nm_ _exprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprImbSingleANm,_exprIself) =
             expr_ 
     in  ( _lhsOmbSingleANm,_lhsOself))
sem_Expr_Op :: Nm ->
               T_Expr  ->
               T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_Op nm_ nmExpr_ lExpr_ rExpr_  =
    (let _lhsOmbSingleANm :: (Maybe ANm)
         _lhsOself :: Expr 
         _nmExprImbSingleANm :: (Maybe ANm)
         _nmExprIself :: Expr 
         _lExprImbSingleANm :: (Maybe ANm)
         _lExprIself :: Expr 
         _rExprImbSingleANm :: (Maybe ANm)
         _rExprIself :: Expr 
         -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
         _lhsOmbSingleANm =
             Nothing
         -- self rule
         _self =
             Expr_Op nm_ _nmExprIself _lExprIself _rExprIself
         -- self rule
         _lhsOself =
             _self
         ( _nmExprImbSingleANm,_nmExprIself) =
             nmExpr_ 
         ( _lExprImbSingleANm,_lExprIself) =
             lExpr_ 
         ( _rExprImbSingleANm,_rExprIself) =
             rExpr_ 
     in  ( _lhsOmbSingleANm,_lhsOself))
sem_Expr_Paren :: T_Expr  ->
                  T_Expr 
sem_Expr_Paren expr_  =
    (let _lhsOself :: Expr 
         _lhsOmbSingleANm :: (Maybe ANm)
         _exprImbSingleANm :: (Maybe ANm)
         _exprIself :: Expr 
         -- self rule
         _self =
             Expr_Paren _exprIself
         -- self rule
         _lhsOself =
             _self
         -- copy rule (up)
         _lhsOmbSingleANm =
             _exprImbSingleANm
         ( _exprImbSingleANm,_exprIself) =
             expr_ 
     in  ( _lhsOmbSingleANm,_lhsOself))
sem_Expr_Retain :: T_Expr  ->
                   T_Expr 
sem_Expr_Retain expr_  =
    (let _lhsOmbSingleANm :: (Maybe ANm)
         _lhsOself :: Expr 
         _exprImbSingleANm :: (Maybe ANm)
         _exprIself :: Expr 
         -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
         _lhsOmbSingleANm =
             Nothing
         -- self rule
         _self =
             Expr_Retain _exprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprImbSingleANm,_exprIself) =
             expr_ 
     in  ( _lhsOmbSingleANm,_lhsOself))
sem_Expr_SP :: T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_SP lExpr_ rExpr_  =
    (let _lhsOmbSingleANm :: (Maybe ANm)
         _lhsOself :: Expr 
         _lExprImbSingleANm :: (Maybe ANm)
         _lExprIself :: Expr 
         _rExprImbSingleANm :: (Maybe ANm)
         _rExprIself :: Expr 
         -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
         _lhsOmbSingleANm =
             Nothing
         -- self rule
         _self =
             Expr_SP _lExprIself _rExprIself
         -- self rule
         _lhsOself =
             _self
         ( _lExprImbSingleANm,_lExprIself) =
             lExpr_ 
         ( _rExprImbSingleANm,_rExprIself) =
             rExpr_ 
     in  ( _lhsOmbSingleANm,_lhsOself))
sem_Expr_Sel :: T_Expr  ->
                T_MbExpr  ->
                T_Expr 
sem_Expr_Sel expr_ selMbExpr_  =
    (let _lhsOmbSingleANm :: (Maybe ANm)
         _lhsOself :: Expr 
         _exprImbSingleANm :: (Maybe ANm)
         _exprIself :: Expr 
         _selMbExprIself :: MbExpr 
         -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
         _lhsOmbSingleANm =
             Nothing
         -- self rule
         _self =
             Expr_Sel _exprIself _selMbExprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprImbSingleANm,_exprIself) =
             expr_ 
         ( _selMbExprIself) =
             selMbExpr_ 
     in  ( _lhsOmbSingleANm,_lhsOself))
sem_Expr_SelTop :: T_Expr  ->
                   T_Expr 
sem_Expr_SelTop expr_  =
    (let _lhsOmbSingleANm :: (Maybe ANm)
         _lhsOself :: Expr 
         _exprImbSingleANm :: (Maybe ANm)
         _exprIself :: Expr 
         -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
         _lhsOmbSingleANm =
             Nothing
         -- self rule
         _self =
             Expr_SelTop _exprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprImbSingleANm,_exprIself) =
             expr_ 
     in  ( _lhsOmbSingleANm,_lhsOself))
sem_Expr_StrAsIs :: String ->
                    T_Expr 
sem_Expr_StrAsIs str_  =
    (let _lhsOmbSingleANm :: (Maybe ANm)
         _lhsOself :: Expr 
         -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
         _lhsOmbSingleANm =
             Nothing
         -- self rule
         _self =
             Expr_StrAsIs str_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOmbSingleANm,_lhsOself))
sem_Expr_StrText :: String ->
                    T_Expr 
sem_Expr_StrText str_  =
    (let _lhsOmbSingleANm :: (Maybe ANm)
         _lhsOself :: Expr 
         -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
         _lhsOmbSingleANm =
             Nothing
         -- self rule
         _self =
             Expr_StrText str_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOmbSingleANm,_lhsOself))
sem_Expr_Undefined :: T_Expr 
sem_Expr_Undefined  =
    (let _lhsOmbSingleANm :: (Maybe ANm)
         _lhsOself :: Expr 
         -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
         _lhsOmbSingleANm =
             Nothing
         -- self rule
         _self =
             Expr_Undefined
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOmbSingleANm,_lhsOself))
sem_Expr_Uniq :: T_Expr 
sem_Expr_Uniq  =
    (let _lhsOmbSingleANm :: (Maybe ANm)
         _lhsOself :: Expr 
         -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
         _lhsOmbSingleANm =
             Nothing
         -- self rule
         _self =
             Expr_Uniq
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOmbSingleANm,_lhsOself))
sem_Expr_Var :: Nm ->
                T_Expr 
sem_Expr_Var nm_  =
    (let _lhsOmbSingleANm :: (Maybe ANm)
         _lhsOself :: Expr 
         -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
         _lhsOmbSingleANm =
             Nothing
         -- self rule
         _self =
             Expr_Var nm_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOmbSingleANm,_lhsOself))
sem_Expr_Wrap :: WrKind ->
                 T_Expr  ->
                 T_Expr 
sem_Expr_Wrap wrKind_ expr_  =
    (let _lhsOmbSingleANm :: (Maybe ANm)
         _lhsOself :: Expr 
         _exprImbSingleANm :: (Maybe ANm)
         _exprIself :: Expr 
         -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
         _lhsOmbSingleANm =
             Nothing
         -- self rule
         _self =
             Expr_Wrap wrKind_ _exprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprImbSingleANm,_exprIself) =
             expr_ 
     in  ( _lhsOmbSingleANm,_lhsOself))
sem_Expr_WrapCnstr :: T_ECnstr  ->
                      T_Expr 
sem_Expr_WrapCnstr cnstr_  =
    (let _lhsOmbSingleANm :: (Maybe ANm)
         _lhsOself :: Expr 
         _cnstrIself :: ECnstr 
         -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
         _lhsOmbSingleANm =
             Nothing
         -- self rule
         _self =
             Expr_WrapCnstr _cnstrIself
         -- self rule
         _lhsOself =
             _self
         ( _cnstrIself) =
             cnstr_ 
     in  ( _lhsOmbSingleANm,_lhsOself))
-- MbExpr ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
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
type T_MbExpr  = ( MbExpr )
sem_MbExpr_Just :: T_Expr  ->
                   T_MbExpr 
sem_MbExpr_Just just_  =
    (let _lhsOself :: MbExpr 
         _justImbSingleANm :: (Maybe ANm)
         _justIself :: Expr 
         -- self rule
         _self =
             Just _justIself
         -- self rule
         _lhsOself =
             _self
         ( _justImbSingleANm,_justIself) =
             just_ 
     in  ( _lhsOself))
sem_MbExpr_Nothing :: T_MbExpr 
sem_MbExpr_Nothing  =
    (let _lhsOself :: MbExpr 
         -- self rule
         _self =
             Nothing
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
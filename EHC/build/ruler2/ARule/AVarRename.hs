

-- UUAGC 0.9.39.1 (build/ruler2/ARule/AVarRename.ag)
module ARule.AVarRename(arlElimAlphaRename) where

import qualified Data.Map as Map
import Common
import Expr.Expr
import ARule.ARule
import FmGam









exprASubst :: RnMp -> Expr -> Expr
exprASubst rnm e
  = self_Syn_AGExprItf r2
  where r1 = sem_AGExprItf (AGExprItf_AGItf e)
        r2 = wrap_AGExprItf r1
                (Inh_AGExprItf { rnMp_Inh_AGExprItf = rnm
                               })

arlElimAlphaRename :: ARule -> ARule
arlElimAlphaRename r
  = self_Syn_AGARuleItf r2
  where r1 = sem_AGARuleItf (AGARuleItf_AGItf r)
        r2 = wrap_AGARuleItf r1
                (Inh_AGARuleItf)



rnRepl :: RnMp -> RnSrc -> RnSrc
rnRepl m e
  = case e of
      RnExpr (Expr_AVar (ANm_Loc n _)) -> r n
      RnNm   (ANm_Loc n _)             -> r n
      _                                -> e
  where r n = maybe e (RnExpr . exprASubst (Map.delete n m) . rnSrc2Expr . snd) (Map.lookup n m)

-- AEqn --------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         rnMp                 : RnMp
      synthesized attributes:
         gathRnMp             : RnMp
         replRnEqns           : [AEqn]
         self                 : SELF 
   alternatives:
      alternative Eqn:
         child dest           : AEqnDest 
         child val            : AExpr 
         visit 0:
            local gathRnMp    : _
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
type T_AEqn  = RnMp ->
               ( RnMp,([AEqn]),AEqn )
sem_AEqn_Eqn :: T_AEqnDest  ->
                T_AExpr  ->
                T_AEqn 
sem_AEqn_Eqn dest_ val_  =
    (\ _lhsIrnMp ->
         (let _lhsOreplRnEqns :: ([AEqn])
              _destOisInComposite :: Bool
              _lhsOgathRnMp :: RnMp
              _lhsOself :: AEqn 
              _destOrnMp :: RnMp
              _valOrnMp :: RnMp
              _destImbSingleANm :: (Maybe ANm)
              _destIself :: AEqnDest 
              _valIgathRnMp :: RnMp
              _valImbSingleANm :: (Maybe ANm)
              _valIself :: AExpr 
              -- "build/ruler2/ARule/AVarRename.ag"(line 55, column 21)
              _gathRnMp =
                  let m = case _destImbSingleANm of
                              Just (ANm_Loc dn p) | AtRetain `notElem` p
                                -> Map.singleton dn (0,v)
                                where v = case (_valImbSingleANm,_valIself) of
                                            (Just sn,_           ) -> RnNm sn
                                            (_      ,AExpr_Expr e) -> RnExpr e
                              _ -> Map.empty
                  in  m `rnMpUnion` _valIgathRnMp
              -- "build/ruler2/ARule/AVarRename.ag"(line 82, column 21)
              _lhsOreplRnEqns =
                  case _destImbSingleANm of
                      Just (ANm_Loc n _)
                        -> case Map.lookup n _lhsIrnMp of
                             Just _ -> []
                             _      -> [_self]
                      _ -> [_self]
              -- "build/ruler2/ARule/InCompDestAG.ag"(line 5, column 21)
              _destOisInComposite =
                  False
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  _gathRnMp
              -- self rule
              _self =
                  AEqn_Eqn _destIself _valIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _destOrnMp =
                  _lhsIrnMp
              -- copy rule (down)
              _valOrnMp =
                  _lhsIrnMp
              ( _destImbSingleANm,_destIself) =
                  dest_ _destOisInComposite _destOrnMp 
              ( _valIgathRnMp,_valImbSingleANm,_valIself) =
                  val_ _valOrnMp 
          in  ( _lhsOgathRnMp,_lhsOreplRnEqns,_lhsOself)))
sem_AEqn_Err :: T_Expr  ->
                T_AEqn 
sem_AEqn_Err expr_  =
    (\ _lhsIrnMp ->
         (let _lhsOreplRnEqns :: ([AEqn])
              _lhsOgathRnMp :: RnMp
              _lhsOself :: AEqn 
              _exprOrnMp :: RnMp
              _exprIgathRnMp :: RnMp
              _exprImbSingleANm :: (Maybe ANm)
              _exprIself :: Expr 
              -- "build/ruler2/ARule/AVarRename.ag"(line 88, column 21)
              _lhsOreplRnEqns =
                  [_self]
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  _exprIgathRnMp
              -- self rule
              _self =
                  AEqn_Err _exprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _exprOrnMp =
                  _lhsIrnMp
              ( _exprIgathRnMp,_exprImbSingleANm,_exprIself) =
                  expr_ _exprOrnMp 
          in  ( _lhsOgathRnMp,_lhsOreplRnEqns,_lhsOself)))
-- AEqnDest ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         isInComposite        : Bool
         rnMp                 : RnMp
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
                   RnMp ->
                   ( (Maybe ANm),AEqnDest )
sem_AEqnDest_Many :: T_AEqnDests  ->
                     T_AEqnDest 
sem_AEqnDest_Many dests_  =
    (\ _lhsIisInComposite
       _lhsIrnMp ->
         (let _lhsOmbSingleANm :: (Maybe ANm)
              _destsOisInComposite :: Bool
              _lhsOself :: AEqnDest 
              _destsOrnMp :: RnMp
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
              -- copy rule (down)
              _destsOrnMp =
                  _lhsIrnMp
              ( _destsIself) =
                  dests_ _destsOisInComposite _destsOrnMp 
          in  ( _lhsOmbSingleANm,_lhsOself)))
sem_AEqnDest_One :: T_ANm  ->
                    T_AEqnDest 
sem_AEqnDest_One anm_  =
    (\ _lhsIisInComposite
       _lhsIrnMp ->
         (let _lhsOmbSingleANm :: (Maybe ANm)
              _lhsOself :: AEqnDest 
              _anmOrnMp :: RnMp
              _anmIgathRnMp :: RnMp
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
              -- copy rule (down)
              _anmOrnMp =
                  _lhsIrnMp
              ( _anmIgathRnMp,_anmIself) =
                  anm_ _anmOrnMp 
          in  ( _lhsOmbSingleANm,_lhsOself)))
-- AEqnDests ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         isInComposite        : Bool
         rnMp                 : RnMp
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
                    RnMp ->
                    ( AEqnDests )
sem_AEqnDests_Cons :: T_AEqnDest  ->
                      T_AEqnDests  ->
                      T_AEqnDests 
sem_AEqnDests_Cons hd_ tl_  =
    (\ _lhsIisInComposite
       _lhsIrnMp ->
         (let _lhsOself :: AEqnDests 
              _hdOisInComposite :: Bool
              _hdOrnMp :: RnMp
              _tlOisInComposite :: Bool
              _tlOrnMp :: RnMp
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
              _hdOrnMp =
                  _lhsIrnMp
              -- copy rule (down)
              _tlOisInComposite =
                  _lhsIisInComposite
              -- copy rule (down)
              _tlOrnMp =
                  _lhsIrnMp
              ( _hdImbSingleANm,_hdIself) =
                  hd_ _hdOisInComposite _hdOrnMp 
              ( _tlIself) =
                  tl_ _tlOisInComposite _tlOrnMp 
          in  ( _lhsOself)))
sem_AEqnDests_Nil :: T_AEqnDests 
sem_AEqnDests_Nil  =
    (\ _lhsIisInComposite
       _lhsIrnMp ->
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
      inherited attribute:
         rnMp                 : RnMp
      synthesized attributes:
         gathRnMp             : RnMp
         replRnEqns           : [AEqn]
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
type T_AEqns  = RnMp ->
                ( RnMp,([AEqn]),AEqns )
sem_AEqns_Cons :: T_AEqn  ->
                  T_AEqns  ->
                  T_AEqns 
sem_AEqns_Cons hd_ tl_  =
    (\ _lhsIrnMp ->
         (let _lhsOgathRnMp :: RnMp
              _lhsOreplRnEqns :: ([AEqn])
              _lhsOself :: AEqns 
              _hdOrnMp :: RnMp
              _tlOrnMp :: RnMp
              _hdIgathRnMp :: RnMp
              _hdIreplRnEqns :: ([AEqn])
              _hdIself :: AEqn 
              _tlIgathRnMp :: RnMp
              _tlIreplRnEqns :: ([AEqn])
              _tlIself :: AEqns 
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  _hdIgathRnMp `rnMpUnion` _tlIgathRnMp
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 76, column 34)
              _lhsOreplRnEqns =
                  _hdIreplRnEqns ++ _tlIreplRnEqns
              -- self rule
              _self =
                  (:) _hdIself _tlIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _hdOrnMp =
                  _lhsIrnMp
              -- copy rule (down)
              _tlOrnMp =
                  _lhsIrnMp
              ( _hdIgathRnMp,_hdIreplRnEqns,_hdIself) =
                  hd_ _hdOrnMp 
              ( _tlIgathRnMp,_tlIreplRnEqns,_tlIself) =
                  tl_ _tlOrnMp 
          in  ( _lhsOgathRnMp,_lhsOreplRnEqns,_lhsOself)))
sem_AEqns_Nil :: T_AEqns 
sem_AEqns_Nil  =
    (\ _lhsIrnMp ->
         (let _lhsOgathRnMp :: RnMp
              _lhsOreplRnEqns :: ([AEqn])
              _lhsOself :: AEqns 
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  Map.empty
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 76, column 34)
              _lhsOreplRnEqns =
                  []
              -- self rule
              _self =
                  []
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOgathRnMp,_lhsOreplRnEqns,_lhsOself)))
-- AExpr -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         rnMp                 : RnMp
      synthesized attributes:
         gathRnMp             : RnMp
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
type T_AExpr  = RnMp ->
                ( RnMp,(Maybe ANm),AExpr )
sem_AExpr_Expr :: T_Expr  ->
                  T_AExpr 
sem_AExpr_Expr expr_  =
    (\ _lhsIrnMp ->
         (let _lhsOgathRnMp :: RnMp
              _lhsOself :: AExpr 
              _lhsOmbSingleANm :: (Maybe ANm)
              _exprOrnMp :: RnMp
              _exprIgathRnMp :: RnMp
              _exprImbSingleANm :: (Maybe ANm)
              _exprIself :: Expr 
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  _exprIgathRnMp
              -- self rule
              _self =
                  AExpr_Expr _exprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOmbSingleANm =
                  _exprImbSingleANm
              -- copy rule (down)
              _exprOrnMp =
                  _lhsIrnMp
              ( _exprIgathRnMp,_exprImbSingleANm,_exprIself) =
                  expr_ _exprOrnMp 
          in  ( _lhsOgathRnMp,_lhsOmbSingleANm,_lhsOself)))
-- AGARuleItf --------------------------------------------------
{-
   visit 0:
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
type T_AGARuleItf  = ( ARule )
data Inh_AGARuleItf  = Inh_AGARuleItf {}
data Syn_AGARuleItf  = Syn_AGARuleItf {self_Syn_AGARuleItf :: ARule }
wrap_AGARuleItf :: T_AGARuleItf  ->
                   Inh_AGARuleItf  ->
                   Syn_AGARuleItf 
wrap_AGARuleItf sem (Inh_AGARuleItf )  =
    (let ( _lhsOself) = sem 
     in  (Syn_AGARuleItf _lhsOself ))
sem_AGARuleItf_AGItf :: T_ARule  ->
                        T_AGARuleItf 
sem_AGARuleItf_AGItf rule_  =
    (let _lhsOself :: ARule 
         _ruleIself :: ARule 
         -- copy rule (up)
         _lhsOself =
             _ruleIself
         ( _ruleIself) =
             rule_ 
     in  ( _lhsOself))
-- AGExprItf ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         rnMp                 : RnMp
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
type T_AGExprItf  = RnMp ->
                    ( Expr )
data Inh_AGExprItf  = Inh_AGExprItf {rnMp_Inh_AGExprItf :: RnMp}
data Syn_AGExprItf  = Syn_AGExprItf {self_Syn_AGExprItf :: Expr }
wrap_AGExprItf :: T_AGExprItf  ->
                  Inh_AGExprItf  ->
                  Syn_AGExprItf 
wrap_AGExprItf sem (Inh_AGExprItf _lhsIrnMp )  =
    (let ( _lhsOself) = sem _lhsIrnMp 
     in  (Syn_AGExprItf _lhsOself ))
sem_AGExprItf_AGItf :: T_Expr  ->
                       T_AGExprItf 
sem_AGExprItf_AGItf expr_  =
    (\ _lhsIrnMp ->
         (let _lhsOself :: Expr 
              _exprOrnMp :: RnMp
              _exprIgathRnMp :: RnMp
              _exprImbSingleANm :: (Maybe ANm)
              _exprIself :: Expr 
              -- copy rule (up)
              _lhsOself =
                  _exprIself
              -- copy rule (down)
              _exprOrnMp =
                  _lhsIrnMp
              ( _exprIgathRnMp,_exprImbSingleANm,_exprIself) =
                  expr_ _exprOrnMp 
          in  ( _lhsOself)))
-- ANm ---------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         rnMp                 : RnMp
      synthesized attributes:
         gathRnMp             : RnMp
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
type T_ANm  = RnMp ->
              ( RnMp,ANm )
sem_ANm_Fld :: Nm ->
               T_ANm 
sem_ANm_Fld nm_  =
    (\ _lhsIrnMp ->
         (let _lhsOgathRnMp :: RnMp
              _lhsOself :: ANm 
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  Map.empty
              -- self rule
              _self =
                  ANm_Fld nm_
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOgathRnMp,_lhsOself)))
sem_ANm_Lhs :: Nm ->
               ([AtProp]) ->
               T_ANm 
sem_ANm_Lhs nm_ props_  =
    (\ _lhsIrnMp ->
         (let _lhsOgathRnMp :: RnMp
              _lhsOself :: ANm 
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  Map.empty
              -- self rule
              _self =
                  ANm_Lhs nm_ props_
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOgathRnMp,_lhsOself)))
sem_ANm_Loc :: Nm ->
               ([AtProp]) ->
               T_ANm 
sem_ANm_Loc nm_ props_  =
    (\ _lhsIrnMp ->
         (let _lhsOgathRnMp :: RnMp
              _lhsOself :: ANm 
              -- "build/ruler2/ARule/AVarRename.ag"(line 65, column 21)
              _lhsOgathRnMp =
                  Map.singleton nm_ (1,RnNone)
              -- self rule
              _self =
                  ANm_Loc nm_ props_
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOgathRnMp,_lhsOself)))
sem_ANm_Node :: Nm ->
                Nm ->
                T_ANm 
sem_ANm_Node ndNm_ nm_  =
    (\ _lhsIrnMp ->
         (let _lhsOgathRnMp :: RnMp
              _lhsOself :: ANm 
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  Map.empty
              -- self rule
              _self =
                  ANm_Node ndNm_ nm_
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOgathRnMp,_lhsOself)))
sem_ANm_Wild :: T_ANm 
sem_ANm_Wild  =
    (\ _lhsIrnMp ->
         (let _lhsOgathRnMp :: RnMp
              _lhsOself :: ANm 
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  Map.empty
              -- self rule
              _self =
                  ANm_Wild
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOgathRnMp,_lhsOself)))
-- ARule -------------------------------------------------------
{-
   visit 0:
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
type T_ARule  = ( ARule )
sem_ARule_Rule :: ([Nm]) ->
                  Nm ->
                  ([String]) ->
                  T_AEqns  ->
                  T_ARule 
sem_ARule_Rule ndNmL_ rlNm_ info_ eqns_  =
    (let _eqnsOrnMp :: RnMp
         _lhsOself :: ARule 
         _eqnsIgathRnMp :: RnMp
         _eqnsIreplRnEqns :: ([AEqn])
         _eqnsIself :: AEqns 
         -- "build/ruler2/ARule/AVarRename.ag"(line 68, column 21)
         _eqnsOrnMp =
             Map.filter (\(c,v)
                          -> case v of
                               RnNone           -> False
                               RnExpr _ | c > 1 -> False
                               _                -> True
                        )
                        _eqnsIgathRnMp
         -- "build/ruler2/ARule/AVarRename.ag"(line 91, column 21)
         _lhsOself =
             ARule_Rule ndNmL_ rlNm_ info_ _eqnsIreplRnEqns
         -- self rule
         _self =
             ARule_Rule ndNmL_ rlNm_ info_ _eqnsIself
         ( _eqnsIgathRnMp,_eqnsIreplRnEqns,_eqnsIself) =
             eqns_ _eqnsOrnMp 
     in  ( _lhsOself))
-- ARules ------------------------------------------------------
{-
   visit 0:
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
type T_ARules  = ( ARules )
sem_ARules_Cons :: T_ARule  ->
                   T_ARules  ->
                   T_ARules 
sem_ARules_Cons hd_ tl_  =
    (let _lhsOself :: ARules 
         _hdIself :: ARule 
         _tlIself :: ARules 
         -- self rule
         _self =
             (:) _hdIself _tlIself
         -- self rule
         _lhsOself =
             _self
         ( _hdIself) =
             hd_ 
         ( _tlIself) =
             tl_ 
     in  ( _lhsOself))
sem_ARules_Nil :: T_ARules 
sem_ARules_Nil  =
    (let _lhsOself :: ARules 
         -- self rule
         _self =
             []
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
-- ECnstr ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         rnMp                 : RnMp
      synthesized attributes:
         gathRnMp             : RnMp
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
type T_ECnstr  = RnMp ->
                 ( RnMp,ECnstr )
sem_ECnstr_Empty :: T_ECnstr 
sem_ECnstr_Empty  =
    (\ _lhsIrnMp ->
         (let _lhsOgathRnMp :: RnMp
              _lhsOself :: ECnstr 
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  Map.empty
              -- self rule
              _self =
                  ECnstr_Empty
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOgathRnMp,_lhsOself)))
sem_ECnstr_Ty :: ([Nm]) ->
                 T_ECnstr 
sem_ECnstr_Ty nms_  =
    (\ _lhsIrnMp ->
         (let _lhsOgathRnMp :: RnMp
              _lhsOself :: ECnstr 
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  Map.empty
              -- self rule
              _self =
                  ECnstr_Ty nms_
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOgathRnMp,_lhsOself)))
sem_ECnstr_Var :: Nm ->
                  T_ECnstr 
sem_ECnstr_Var nm_  =
    (\ _lhsIrnMp ->
         (let _lhsOgathRnMp :: RnMp
              _lhsOself :: ECnstr 
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  Map.empty
              -- self rule
              _self =
                  ECnstr_Var nm_
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOgathRnMp,_lhsOself)))
-- Expr --------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         rnMp                 : RnMp
      synthesized attributes:
         gathRnMp             : RnMp
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
type T_Expr  = RnMp ->
               ( RnMp,(Maybe ANm),Expr )
sem_Expr_AVar :: T_ANm  ->
                 T_Expr 
sem_Expr_AVar anm_  =
    (\ _lhsIrnMp ->
         (let _lhsOself :: Expr 
              _lhsOmbSingleANm :: (Maybe ANm)
              _lhsOgathRnMp :: RnMp
              _anmOrnMp :: RnMp
              _anmIgathRnMp :: RnMp
              _anmIself :: ANm 
              -- "build/ruler2/ARule/AVarRename.ag"(line 79, column 21)
              _lhsOself =
                  rnSrc2Expr (rnRepl _lhsIrnMp (RnExpr _self))
              -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 19, column 21)
              _lhsOmbSingleANm =
                  Just _anmIself
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  _anmIgathRnMp
              -- self rule
              _self =
                  Expr_AVar _anmIself
              -- copy rule (down)
              _anmOrnMp =
                  _lhsIrnMp
              ( _anmIgathRnMp,_anmIself) =
                  anm_ _anmOrnMp 
          in  ( _lhsOgathRnMp,_lhsOmbSingleANm,_lhsOself)))
sem_Expr_App :: T_Expr  ->
                T_Expr  ->
                T_Expr 
sem_Expr_App lExpr_ rExpr_  =
    (\ _lhsIrnMp ->
         (let _lhsOmbSingleANm :: (Maybe ANm)
              _lhsOgathRnMp :: RnMp
              _lhsOself :: Expr 
              _lExprOrnMp :: RnMp
              _rExprOrnMp :: RnMp
              _lExprIgathRnMp :: RnMp
              _lExprImbSingleANm :: (Maybe ANm)
              _lExprIself :: Expr 
              _rExprIgathRnMp :: RnMp
              _rExprImbSingleANm :: (Maybe ANm)
              _rExprIself :: Expr 
              -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
              _lhsOmbSingleANm =
                  Nothing
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  _lExprIgathRnMp `rnMpUnion` _rExprIgathRnMp
              -- self rule
              _self =
                  Expr_App _lExprIself _rExprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _lExprOrnMp =
                  _lhsIrnMp
              -- copy rule (down)
              _rExprOrnMp =
                  _lhsIrnMp
              ( _lExprIgathRnMp,_lExprImbSingleANm,_lExprIself) =
                  lExpr_ _lExprOrnMp 
              ( _rExprIgathRnMp,_rExprImbSingleANm,_rExprIself) =
                  rExpr_ _rExprOrnMp 
          in  ( _lhsOgathRnMp,_lhsOmbSingleANm,_lhsOself)))
sem_Expr_AppTop :: T_Expr  ->
                   T_Expr 
sem_Expr_AppTop expr_  =
    (\ _lhsIrnMp ->
         (let _lhsOgathRnMp :: RnMp
              _lhsOself :: Expr 
              _lhsOmbSingleANm :: (Maybe ANm)
              _exprOrnMp :: RnMp
              _exprIgathRnMp :: RnMp
              _exprImbSingleANm :: (Maybe ANm)
              _exprIself :: Expr 
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  _exprIgathRnMp
              -- self rule
              _self =
                  Expr_AppTop _exprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOmbSingleANm =
                  _exprImbSingleANm
              -- copy rule (down)
              _exprOrnMp =
                  _lhsIrnMp
              ( _exprIgathRnMp,_exprImbSingleANm,_exprIself) =
                  expr_ _exprOrnMp 
          in  ( _lhsOgathRnMp,_lhsOmbSingleANm,_lhsOself)))
sem_Expr_ChildOrder :: Int ->
                       T_Expr  ->
                       T_Expr 
sem_Expr_ChildOrder seqNr_ expr_  =
    (\ _lhsIrnMp ->
         (let _lhsOmbSingleANm :: (Maybe ANm)
              _lhsOgathRnMp :: RnMp
              _lhsOself :: Expr 
              _exprOrnMp :: RnMp
              _exprIgathRnMp :: RnMp
              _exprImbSingleANm :: (Maybe ANm)
              _exprIself :: Expr 
              -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
              _lhsOmbSingleANm =
                  Nothing
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  _exprIgathRnMp
              -- self rule
              _self =
                  Expr_ChildOrder seqNr_ _exprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _exprOrnMp =
                  _lhsIrnMp
              ( _exprIgathRnMp,_exprImbSingleANm,_exprIself) =
                  expr_ _exprOrnMp 
          in  ( _lhsOgathRnMp,_lhsOmbSingleANm,_lhsOself)))
sem_Expr_Cnstr :: T_Expr  ->
                  T_ECnstr  ->
                  T_Expr 
sem_Expr_Cnstr expr_ cnstr_  =
    (\ _lhsIrnMp ->
         (let _lhsOmbSingleANm :: (Maybe ANm)
              _lhsOgathRnMp :: RnMp
              _lhsOself :: Expr 
              _exprOrnMp :: RnMp
              _cnstrOrnMp :: RnMp
              _exprIgathRnMp :: RnMp
              _exprImbSingleANm :: (Maybe ANm)
              _exprIself :: Expr 
              _cnstrIgathRnMp :: RnMp
              _cnstrIself :: ECnstr 
              -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
              _lhsOmbSingleANm =
                  Nothing
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  _exprIgathRnMp `rnMpUnion` _cnstrIgathRnMp
              -- self rule
              _self =
                  Expr_Cnstr _exprIself _cnstrIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _exprOrnMp =
                  _lhsIrnMp
              -- copy rule (down)
              _cnstrOrnMp =
                  _lhsIrnMp
              ( _exprIgathRnMp,_exprImbSingleANm,_exprIself) =
                  expr_ _exprOrnMp 
              ( _cnstrIgathRnMp,_cnstrIself) =
                  cnstr_ _cnstrOrnMp 
          in  ( _lhsOgathRnMp,_lhsOmbSingleANm,_lhsOself)))
sem_Expr_Empty :: T_Expr 
sem_Expr_Empty  =
    (\ _lhsIrnMp ->
         (let _lhsOmbSingleANm :: (Maybe ANm)
              _lhsOgathRnMp :: RnMp
              _lhsOself :: Expr 
              -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
              _lhsOmbSingleANm =
                  Nothing
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  Map.empty
              -- self rule
              _self =
                  Expr_Empty
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOgathRnMp,_lhsOmbSingleANm,_lhsOself)))
sem_Expr_Expr :: T_Expr  ->
                 T_Expr 
sem_Expr_Expr expr_  =
    (\ _lhsIrnMp ->
         (let _lhsOmbSingleANm :: (Maybe ANm)
              _lhsOgathRnMp :: RnMp
              _lhsOself :: Expr 
              _exprOrnMp :: RnMp
              _exprIgathRnMp :: RnMp
              _exprImbSingleANm :: (Maybe ANm)
              _exprIself :: Expr 
              -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
              _lhsOmbSingleANm =
                  Nothing
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  _exprIgathRnMp
              -- self rule
              _self =
                  Expr_Expr _exprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _exprOrnMp =
                  _lhsIrnMp
              ( _exprIgathRnMp,_exprImbSingleANm,_exprIself) =
                  expr_ _exprOrnMp 
          in  ( _lhsOgathRnMp,_lhsOmbSingleANm,_lhsOself)))
sem_Expr_Int :: String ->
                T_Expr 
sem_Expr_Int int_  =
    (\ _lhsIrnMp ->
         (let _lhsOmbSingleANm :: (Maybe ANm)
              _lhsOgathRnMp :: RnMp
              _lhsOself :: Expr 
              -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
              _lhsOmbSingleANm =
                  Nothing
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  Map.empty
              -- self rule
              _self =
                  Expr_Int int_
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOgathRnMp,_lhsOmbSingleANm,_lhsOself)))
sem_Expr_LF :: T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_LF lExpr_ rExpr_  =
    (\ _lhsIrnMp ->
         (let _lhsOmbSingleANm :: (Maybe ANm)
              _lhsOgathRnMp :: RnMp
              _lhsOself :: Expr 
              _lExprOrnMp :: RnMp
              _rExprOrnMp :: RnMp
              _lExprIgathRnMp :: RnMp
              _lExprImbSingleANm :: (Maybe ANm)
              _lExprIself :: Expr 
              _rExprIgathRnMp :: RnMp
              _rExprImbSingleANm :: (Maybe ANm)
              _rExprIself :: Expr 
              -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
              _lhsOmbSingleANm =
                  Nothing
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  _lExprIgathRnMp `rnMpUnion` _rExprIgathRnMp
              -- self rule
              _self =
                  Expr_LF _lExprIself _rExprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _lExprOrnMp =
                  _lhsIrnMp
              -- copy rule (down)
              _rExprOrnMp =
                  _lhsIrnMp
              ( _lExprIgathRnMp,_lExprImbSingleANm,_lExprIself) =
                  lExpr_ _lExprOrnMp 
              ( _rExprIgathRnMp,_rExprImbSingleANm,_rExprIself) =
                  rExpr_ _rExprOrnMp 
          in  ( _lhsOgathRnMp,_lhsOmbSingleANm,_lhsOself)))
sem_Expr_Named :: Nm ->
                  T_Expr  ->
                  T_Expr 
sem_Expr_Named nm_ expr_  =
    (\ _lhsIrnMp ->
         (let _lhsOmbSingleANm :: (Maybe ANm)
              _lhsOgathRnMp :: RnMp
              _lhsOself :: Expr 
              _exprOrnMp :: RnMp
              _exprIgathRnMp :: RnMp
              _exprImbSingleANm :: (Maybe ANm)
              _exprIself :: Expr 
              -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
              _lhsOmbSingleANm =
                  Nothing
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  _exprIgathRnMp
              -- self rule
              _self =
                  Expr_Named nm_ _exprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _exprOrnMp =
                  _lhsIrnMp
              ( _exprIgathRnMp,_exprImbSingleANm,_exprIself) =
                  expr_ _exprOrnMp 
          in  ( _lhsOgathRnMp,_lhsOmbSingleANm,_lhsOself)))
sem_Expr_Op :: Nm ->
               T_Expr  ->
               T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_Op nm_ nmExpr_ lExpr_ rExpr_  =
    (\ _lhsIrnMp ->
         (let _lhsOmbSingleANm :: (Maybe ANm)
              _lhsOgathRnMp :: RnMp
              _lhsOself :: Expr 
              _nmExprOrnMp :: RnMp
              _lExprOrnMp :: RnMp
              _rExprOrnMp :: RnMp
              _nmExprIgathRnMp :: RnMp
              _nmExprImbSingleANm :: (Maybe ANm)
              _nmExprIself :: Expr 
              _lExprIgathRnMp :: RnMp
              _lExprImbSingleANm :: (Maybe ANm)
              _lExprIself :: Expr 
              _rExprIgathRnMp :: RnMp
              _rExprImbSingleANm :: (Maybe ANm)
              _rExprIself :: Expr 
              -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
              _lhsOmbSingleANm =
                  Nothing
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  _nmExprIgathRnMp `rnMpUnion` _lExprIgathRnMp `rnMpUnion` _rExprIgathRnMp
              -- self rule
              _self =
                  Expr_Op nm_ _nmExprIself _lExprIself _rExprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _nmExprOrnMp =
                  _lhsIrnMp
              -- copy rule (down)
              _lExprOrnMp =
                  _lhsIrnMp
              -- copy rule (down)
              _rExprOrnMp =
                  _lhsIrnMp
              ( _nmExprIgathRnMp,_nmExprImbSingleANm,_nmExprIself) =
                  nmExpr_ _nmExprOrnMp 
              ( _lExprIgathRnMp,_lExprImbSingleANm,_lExprIself) =
                  lExpr_ _lExprOrnMp 
              ( _rExprIgathRnMp,_rExprImbSingleANm,_rExprIself) =
                  rExpr_ _rExprOrnMp 
          in  ( _lhsOgathRnMp,_lhsOmbSingleANm,_lhsOself)))
sem_Expr_Paren :: T_Expr  ->
                  T_Expr 
sem_Expr_Paren expr_  =
    (\ _lhsIrnMp ->
         (let _lhsOgathRnMp :: RnMp
              _lhsOself :: Expr 
              _lhsOmbSingleANm :: (Maybe ANm)
              _exprOrnMp :: RnMp
              _exprIgathRnMp :: RnMp
              _exprImbSingleANm :: (Maybe ANm)
              _exprIself :: Expr 
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  _exprIgathRnMp
              -- self rule
              _self =
                  Expr_Paren _exprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOmbSingleANm =
                  _exprImbSingleANm
              -- copy rule (down)
              _exprOrnMp =
                  _lhsIrnMp
              ( _exprIgathRnMp,_exprImbSingleANm,_exprIself) =
                  expr_ _exprOrnMp 
          in  ( _lhsOgathRnMp,_lhsOmbSingleANm,_lhsOself)))
sem_Expr_Retain :: T_Expr  ->
                   T_Expr 
sem_Expr_Retain expr_  =
    (\ _lhsIrnMp ->
         (let _lhsOmbSingleANm :: (Maybe ANm)
              _lhsOgathRnMp :: RnMp
              _lhsOself :: Expr 
              _exprOrnMp :: RnMp
              _exprIgathRnMp :: RnMp
              _exprImbSingleANm :: (Maybe ANm)
              _exprIself :: Expr 
              -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
              _lhsOmbSingleANm =
                  Nothing
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  _exprIgathRnMp
              -- self rule
              _self =
                  Expr_Retain _exprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _exprOrnMp =
                  _lhsIrnMp
              ( _exprIgathRnMp,_exprImbSingleANm,_exprIself) =
                  expr_ _exprOrnMp 
          in  ( _lhsOgathRnMp,_lhsOmbSingleANm,_lhsOself)))
sem_Expr_SP :: T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_SP lExpr_ rExpr_  =
    (\ _lhsIrnMp ->
         (let _lhsOmbSingleANm :: (Maybe ANm)
              _lhsOgathRnMp :: RnMp
              _lhsOself :: Expr 
              _lExprOrnMp :: RnMp
              _rExprOrnMp :: RnMp
              _lExprIgathRnMp :: RnMp
              _lExprImbSingleANm :: (Maybe ANm)
              _lExprIself :: Expr 
              _rExprIgathRnMp :: RnMp
              _rExprImbSingleANm :: (Maybe ANm)
              _rExprIself :: Expr 
              -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
              _lhsOmbSingleANm =
                  Nothing
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  _lExprIgathRnMp `rnMpUnion` _rExprIgathRnMp
              -- self rule
              _self =
                  Expr_SP _lExprIself _rExprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _lExprOrnMp =
                  _lhsIrnMp
              -- copy rule (down)
              _rExprOrnMp =
                  _lhsIrnMp
              ( _lExprIgathRnMp,_lExprImbSingleANm,_lExprIself) =
                  lExpr_ _lExprOrnMp 
              ( _rExprIgathRnMp,_rExprImbSingleANm,_rExprIself) =
                  rExpr_ _rExprOrnMp 
          in  ( _lhsOgathRnMp,_lhsOmbSingleANm,_lhsOself)))
sem_Expr_Sel :: T_Expr  ->
                T_MbExpr  ->
                T_Expr 
sem_Expr_Sel expr_ selMbExpr_  =
    (\ _lhsIrnMp ->
         (let _lhsOmbSingleANm :: (Maybe ANm)
              _lhsOgathRnMp :: RnMp
              _lhsOself :: Expr 
              _exprOrnMp :: RnMp
              _selMbExprOrnMp :: RnMp
              _exprIgathRnMp :: RnMp
              _exprImbSingleANm :: (Maybe ANm)
              _exprIself :: Expr 
              _selMbExprIgathRnMp :: RnMp
              _selMbExprIself :: MbExpr 
              -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
              _lhsOmbSingleANm =
                  Nothing
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  _exprIgathRnMp `rnMpUnion` _selMbExprIgathRnMp
              -- self rule
              _self =
                  Expr_Sel _exprIself _selMbExprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _exprOrnMp =
                  _lhsIrnMp
              -- copy rule (down)
              _selMbExprOrnMp =
                  _lhsIrnMp
              ( _exprIgathRnMp,_exprImbSingleANm,_exprIself) =
                  expr_ _exprOrnMp 
              ( _selMbExprIgathRnMp,_selMbExprIself) =
                  selMbExpr_ _selMbExprOrnMp 
          in  ( _lhsOgathRnMp,_lhsOmbSingleANm,_lhsOself)))
sem_Expr_SelTop :: T_Expr  ->
                   T_Expr 
sem_Expr_SelTop expr_  =
    (\ _lhsIrnMp ->
         (let _lhsOmbSingleANm :: (Maybe ANm)
              _lhsOgathRnMp :: RnMp
              _lhsOself :: Expr 
              _exprOrnMp :: RnMp
              _exprIgathRnMp :: RnMp
              _exprImbSingleANm :: (Maybe ANm)
              _exprIself :: Expr 
              -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
              _lhsOmbSingleANm =
                  Nothing
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  _exprIgathRnMp
              -- self rule
              _self =
                  Expr_SelTop _exprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _exprOrnMp =
                  _lhsIrnMp
              ( _exprIgathRnMp,_exprImbSingleANm,_exprIself) =
                  expr_ _exprOrnMp 
          in  ( _lhsOgathRnMp,_lhsOmbSingleANm,_lhsOself)))
sem_Expr_StrAsIs :: String ->
                    T_Expr 
sem_Expr_StrAsIs str_  =
    (\ _lhsIrnMp ->
         (let _lhsOmbSingleANm :: (Maybe ANm)
              _lhsOgathRnMp :: RnMp
              _lhsOself :: Expr 
              -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
              _lhsOmbSingleANm =
                  Nothing
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  Map.empty
              -- self rule
              _self =
                  Expr_StrAsIs str_
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOgathRnMp,_lhsOmbSingleANm,_lhsOself)))
sem_Expr_StrText :: String ->
                    T_Expr 
sem_Expr_StrText str_  =
    (\ _lhsIrnMp ->
         (let _lhsOmbSingleANm :: (Maybe ANm)
              _lhsOgathRnMp :: RnMp
              _lhsOself :: Expr 
              -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
              _lhsOmbSingleANm =
                  Nothing
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  Map.empty
              -- self rule
              _self =
                  Expr_StrText str_
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOgathRnMp,_lhsOmbSingleANm,_lhsOself)))
sem_Expr_Undefined :: T_Expr 
sem_Expr_Undefined  =
    (\ _lhsIrnMp ->
         (let _lhsOmbSingleANm :: (Maybe ANm)
              _lhsOgathRnMp :: RnMp
              _lhsOself :: Expr 
              -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
              _lhsOmbSingleANm =
                  Nothing
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  Map.empty
              -- self rule
              _self =
                  Expr_Undefined
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOgathRnMp,_lhsOmbSingleANm,_lhsOself)))
sem_Expr_Uniq :: T_Expr 
sem_Expr_Uniq  =
    (\ _lhsIrnMp ->
         (let _lhsOmbSingleANm :: (Maybe ANm)
              _lhsOgathRnMp :: RnMp
              _lhsOself :: Expr 
              -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
              _lhsOmbSingleANm =
                  Nothing
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  Map.empty
              -- self rule
              _self =
                  Expr_Uniq
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOgathRnMp,_lhsOmbSingleANm,_lhsOself)))
sem_Expr_Var :: Nm ->
                T_Expr 
sem_Expr_Var nm_  =
    (\ _lhsIrnMp ->
         (let _lhsOmbSingleANm :: (Maybe ANm)
              _lhsOgathRnMp :: RnMp
              _lhsOself :: Expr 
              -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
              _lhsOmbSingleANm =
                  Nothing
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  Map.empty
              -- self rule
              _self =
                  Expr_Var nm_
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOgathRnMp,_lhsOmbSingleANm,_lhsOself)))
sem_Expr_Wrap :: WrKind ->
                 T_Expr  ->
                 T_Expr 
sem_Expr_Wrap wrKind_ expr_  =
    (\ _lhsIrnMp ->
         (let _lhsOmbSingleANm :: (Maybe ANm)
              _lhsOgathRnMp :: RnMp
              _lhsOself :: Expr 
              _exprOrnMp :: RnMp
              _exprIgathRnMp :: RnMp
              _exprImbSingleANm :: (Maybe ANm)
              _exprIself :: Expr 
              -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
              _lhsOmbSingleANm =
                  Nothing
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  _exprIgathRnMp
              -- self rule
              _self =
                  Expr_Wrap wrKind_ _exprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _exprOrnMp =
                  _lhsIrnMp
              ( _exprIgathRnMp,_exprImbSingleANm,_exprIself) =
                  expr_ _exprOrnMp 
          in  ( _lhsOgathRnMp,_lhsOmbSingleANm,_lhsOself)))
sem_Expr_WrapCnstr :: T_ECnstr  ->
                      T_Expr 
sem_Expr_WrapCnstr cnstr_  =
    (\ _lhsIrnMp ->
         (let _lhsOmbSingleANm :: (Maybe ANm)
              _lhsOgathRnMp :: RnMp
              _lhsOself :: Expr 
              _cnstrOrnMp :: RnMp
              _cnstrIgathRnMp :: RnMp
              _cnstrIself :: ECnstr 
              -- "build/ruler2/ARule/EqnDest1NmAG.ag"(line 21, column 21)
              _lhsOmbSingleANm =
                  Nothing
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  _cnstrIgathRnMp
              -- self rule
              _self =
                  Expr_WrapCnstr _cnstrIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _cnstrOrnMp =
                  _lhsIrnMp
              ( _cnstrIgathRnMp,_cnstrIself) =
                  cnstr_ _cnstrOrnMp 
          in  ( _lhsOgathRnMp,_lhsOmbSingleANm,_lhsOself)))
-- MbExpr ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         rnMp                 : RnMp
      synthesized attributes:
         gathRnMp             : RnMp
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
type T_MbExpr  = RnMp ->
                 ( RnMp,MbExpr )
sem_MbExpr_Just :: T_Expr  ->
                   T_MbExpr 
sem_MbExpr_Just just_  =
    (\ _lhsIrnMp ->
         (let _lhsOgathRnMp :: RnMp
              _lhsOself :: MbExpr 
              _justOrnMp :: RnMp
              _justIgathRnMp :: RnMp
              _justImbSingleANm :: (Maybe ANm)
              _justIself :: Expr 
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  _justIgathRnMp
              -- self rule
              _self =
                  Just _justIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _justOrnMp =
                  _lhsIrnMp
              ( _justIgathRnMp,_justImbSingleANm,_justIself) =
                  just_ _justOrnMp 
          in  ( _lhsOgathRnMp,_lhsOself)))
sem_MbExpr_Nothing :: T_MbExpr 
sem_MbExpr_Nothing  =
    (\ _lhsIrnMp ->
         (let _lhsOgathRnMp :: RnMp
              _lhsOself :: MbExpr 
              -- use rule "build/ruler2/ARule/AVarRename.ag"(line 51, column 50)
              _lhsOgathRnMp =
                  Map.empty
              -- self rule
              _self =
                  Nothing
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOgathRnMp,_lhsOself)))
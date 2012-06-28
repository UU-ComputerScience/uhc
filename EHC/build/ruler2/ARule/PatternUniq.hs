

-- UUAGC 0.9.39.1 (build/ruler2/ARule/PatternUniq.ag)
module ARule.PatternUniq(arlUniq) where

import qualified Data.Set as Set
import Common
import Opts
import ARule.ARule
import Expr.Expr
import FmGam
import ARule.RwSubst









arlUniq :: FmGam Expr -> [Nm] -> ARule -> ARule
arlUniq fg co rl
  = replUniq_Syn_AGARuleItf r2
  where r1 = sem_AGARuleItf (AGARuleItf_AGItf rl)
        r2 = wrap_AGARuleItf r1
                (Inh_AGARuleItf {opts_Inh_AGARuleItf = defaultOpts
                                ,croNmL_Inh_AGARuleItf = co
                                ,fmGam_Inh_AGARuleItf = fg
                                })

-- AEqn --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
      chained attribute:
         uniqSeqNr            : Int
      synthesized attribute:
         replUniq             : SELF 
   alternatives:
      alternative Eqn:
         child dest           : AEqnDest 
         child val            : AExpr 
         visit 0:
            local replUniq    : _
      alternative Err:
         child expr           : Expr 
         visit 0:
            local replUniq    : _
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
               Opts ->
               Int ->
               ( AEqn ,Int)
sem_AEqn_Eqn :: T_AEqnDest  ->
                T_AExpr  ->
                T_AEqn 
sem_AEqn_Eqn dest_ val_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: AEqn 
              _lhsOuniqSeqNr :: Int
              _destOfmGam :: (FmGam Expr)
              _destOopts :: Opts
              _destOuniqSeqNr :: Int
              _valOfmGam :: (FmGam Expr)
              _valOopts :: Opts
              _valOuniqSeqNr :: Int
              _destIreplUniq :: AEqnDest 
              _destIuniqSeqNr :: Int
              _valIreplUniq :: AExpr 
              _valIuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  AEqn_Eqn _destIreplUniq _valIreplUniq
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (up)
              _lhsOuniqSeqNr =
                  _valIuniqSeqNr
              -- copy rule (down)
              _destOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _destOopts =
                  _lhsIopts
              -- copy rule (down)
              _destOuniqSeqNr =
                  _lhsIuniqSeqNr
              -- copy rule (down)
              _valOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _valOopts =
                  _lhsIopts
              -- copy rule (chain)
              _valOuniqSeqNr =
                  _destIuniqSeqNr
              ( _destIreplUniq,_destIuniqSeqNr) =
                  dest_ _destOfmGam _destOopts _destOuniqSeqNr 
              ( _valIreplUniq,_valIuniqSeqNr) =
                  val_ _valOfmGam _valOopts _valOuniqSeqNr 
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_AEqn_Err :: T_Expr  ->
                T_AEqn 
sem_AEqn_Err expr_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: AEqn 
              _lhsOuniqSeqNr :: Int
              _exprOfmGam :: (FmGam Expr)
              _exprOopts :: Opts
              _exprOuniqSeqNr :: Int
              _exprIreplUniq :: Expr 
              _exprIuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  AEqn_Err _exprIreplUniq
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (up)
              _lhsOuniqSeqNr =
                  _exprIuniqSeqNr
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOuniqSeqNr =
                  _lhsIuniqSeqNr
              ( _exprIreplUniq,_exprIuniqSeqNr) =
                  expr_ _exprOfmGam _exprOopts _exprOuniqSeqNr 
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
-- AEqnDest ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
      chained attribute:
         uniqSeqNr            : Int
      synthesized attribute:
         replUniq             : SELF 
   alternatives:
      alternative Many:
         child dests          : AEqnDests 
         visit 0:
            local replUniq    : _
      alternative One:
         child anm            : ANm 
         visit 0:
            local replUniq    : _
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
                   Opts ->
                   Int ->
                   ( AEqnDest ,Int)
sem_AEqnDest_Many :: T_AEqnDests  ->
                     T_AEqnDest 
sem_AEqnDest_Many dests_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: AEqnDest 
              _lhsOuniqSeqNr :: Int
              _destsOfmGam :: (FmGam Expr)
              _destsOopts :: Opts
              _destsOuniqSeqNr :: Int
              _destsIreplUniq :: AEqnDests 
              _destsIuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  AEqnDest_Many _destsIreplUniq
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (up)
              _lhsOuniqSeqNr =
                  _destsIuniqSeqNr
              -- copy rule (down)
              _destsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _destsOopts =
                  _lhsIopts
              -- copy rule (down)
              _destsOuniqSeqNr =
                  _lhsIuniqSeqNr
              ( _destsIreplUniq,_destsIuniqSeqNr) =
                  dests_ _destsOfmGam _destsOopts _destsOuniqSeqNr 
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_AEqnDest_One :: T_ANm  ->
                    T_AEqnDest 
sem_AEqnDest_One anm_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: AEqnDest 
              _lhsOuniqSeqNr :: Int
              _anmOopts :: Opts
              _anmOuniqSeqNr :: Int
              _anmIreplUniq :: ANm 
              _anmIuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  AEqnDest_One _anmIreplUniq
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (up)
              _lhsOuniqSeqNr =
                  _anmIuniqSeqNr
              -- copy rule (down)
              _anmOopts =
                  _lhsIopts
              -- copy rule (down)
              _anmOuniqSeqNr =
                  _lhsIuniqSeqNr
              ( _anmIreplUniq,_anmIuniqSeqNr) =
                  anm_ _anmOopts _anmOuniqSeqNr 
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
-- AEqnDests ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
      chained attribute:
         uniqSeqNr            : Int
      synthesized attribute:
         replUniq             : SELF 
   alternatives:
      alternative Cons:
         child hd             : AEqnDest 
         child tl             : AEqnDests 
         visit 0:
            local replUniq    : _
      alternative Nil:
         visit 0:
            local replUniq    : _
-}
-- cata
sem_AEqnDests :: AEqnDests  ->
                 T_AEqnDests 
sem_AEqnDests list  =
    (Prelude.foldr sem_AEqnDests_Cons sem_AEqnDests_Nil (Prelude.map sem_AEqnDest list) )
-- semantic domain
type T_AEqnDests  = (FmGam Expr) ->
                    Opts ->
                    Int ->
                    ( AEqnDests ,Int)
sem_AEqnDests_Cons :: T_AEqnDest  ->
                      T_AEqnDests  ->
                      T_AEqnDests 
sem_AEqnDests_Cons hd_ tl_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: AEqnDests 
              _lhsOuniqSeqNr :: Int
              _hdOfmGam :: (FmGam Expr)
              _hdOopts :: Opts
              _hdOuniqSeqNr :: Int
              _tlOfmGam :: (FmGam Expr)
              _tlOopts :: Opts
              _tlOuniqSeqNr :: Int
              _hdIreplUniq :: AEqnDest 
              _hdIuniqSeqNr :: Int
              _tlIreplUniq :: AEqnDests 
              _tlIuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  (:) _hdIreplUniq _tlIreplUniq
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (up)
              _lhsOuniqSeqNr =
                  _tlIuniqSeqNr
              -- copy rule (down)
              _hdOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOuniqSeqNr =
                  _lhsIuniqSeqNr
              -- copy rule (down)
              _tlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOuniqSeqNr =
                  _hdIuniqSeqNr
              ( _hdIreplUniq,_hdIuniqSeqNr) =
                  hd_ _hdOfmGam _hdOopts _hdOuniqSeqNr 
              ( _tlIreplUniq,_tlIuniqSeqNr) =
                  tl_ _tlOfmGam _tlOopts _tlOuniqSeqNr 
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_AEqnDests_Nil :: T_AEqnDests 
sem_AEqnDests_Nil  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: AEqnDests 
              _lhsOuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  []
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (chain)
              _lhsOuniqSeqNr =
                  _lhsIuniqSeqNr
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
-- AEqns -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
      chained attribute:
         uniqSeqNr            : Int
      synthesized attribute:
         replUniq             : SELF 
   alternatives:
      alternative Cons:
         child hd             : AEqn 
         child tl             : AEqns 
         visit 0:
            local replUniq    : _
      alternative Nil:
         visit 0:
            local replUniq    : _
-}
-- cata
sem_AEqns :: AEqns  ->
             T_AEqns 
sem_AEqns list  =
    (Prelude.foldr sem_AEqns_Cons sem_AEqns_Nil (Prelude.map sem_AEqn list) )
-- semantic domain
type T_AEqns  = (FmGam Expr) ->
                Opts ->
                Int ->
                ( AEqns ,Int)
sem_AEqns_Cons :: T_AEqn  ->
                  T_AEqns  ->
                  T_AEqns 
sem_AEqns_Cons hd_ tl_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: AEqns 
              _lhsOuniqSeqNr :: Int
              _hdOfmGam :: (FmGam Expr)
              _hdOopts :: Opts
              _hdOuniqSeqNr :: Int
              _tlOfmGam :: (FmGam Expr)
              _tlOopts :: Opts
              _tlOuniqSeqNr :: Int
              _hdIreplUniq :: AEqn 
              _hdIuniqSeqNr :: Int
              _tlIreplUniq :: AEqns 
              _tlIuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  (:) _hdIreplUniq _tlIreplUniq
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (up)
              _lhsOuniqSeqNr =
                  _tlIuniqSeqNr
              -- copy rule (down)
              _hdOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOuniqSeqNr =
                  _lhsIuniqSeqNr
              -- copy rule (down)
              _tlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOuniqSeqNr =
                  _hdIuniqSeqNr
              ( _hdIreplUniq,_hdIuniqSeqNr) =
                  hd_ _hdOfmGam _hdOopts _hdOuniqSeqNr 
              ( _tlIreplUniq,_tlIuniqSeqNr) =
                  tl_ _tlOfmGam _tlOopts _tlOuniqSeqNr 
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_AEqns_Nil :: T_AEqns 
sem_AEqns_Nil  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: AEqns 
              _lhsOuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  []
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (chain)
              _lhsOuniqSeqNr =
                  _lhsIuniqSeqNr
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
-- AExpr -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
      chained attribute:
         uniqSeqNr            : Int
      synthesized attribute:
         replUniq             : SELF 
   alternatives:
      alternative Expr:
         child expr           : Expr 
         visit 0:
            local replUniq    : _
-}
-- cata
sem_AExpr :: AExpr  ->
             T_AExpr 
sem_AExpr (AExpr_Expr _expr )  =
    (sem_AExpr_Expr (sem_Expr _expr ) )
-- semantic domain
type T_AExpr  = (FmGam Expr) ->
                Opts ->
                Int ->
                ( AExpr ,Int)
sem_AExpr_Expr :: T_Expr  ->
                  T_AExpr 
sem_AExpr_Expr expr_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: AExpr 
              _lhsOuniqSeqNr :: Int
              _exprOfmGam :: (FmGam Expr)
              _exprOopts :: Opts
              _exprOuniqSeqNr :: Int
              _exprIreplUniq :: Expr 
              _exprIuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  AExpr_Expr _exprIreplUniq
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (up)
              _lhsOuniqSeqNr =
                  _exprIuniqSeqNr
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOuniqSeqNr =
                  _lhsIuniqSeqNr
              ( _exprIreplUniq,_exprIuniqSeqNr) =
                  expr_ _exprOfmGam _exprOopts _exprOuniqSeqNr 
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
-- AGARuleItf --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         croNmL               : [Nm]
         fmGam                : FmGam Expr
         opts                 : Opts
      synthesized attribute:
         replUniq             : ARule 
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
type T_AGARuleItf  = ([Nm]) ->
                     (FmGam Expr) ->
                     Opts ->
                     ( ARule )
data Inh_AGARuleItf  = Inh_AGARuleItf {croNmL_Inh_AGARuleItf :: ([Nm]),fmGam_Inh_AGARuleItf :: (FmGam Expr),opts_Inh_AGARuleItf :: Opts}
data Syn_AGARuleItf  = Syn_AGARuleItf {replUniq_Syn_AGARuleItf :: ARule }
wrap_AGARuleItf :: T_AGARuleItf  ->
                   Inh_AGARuleItf  ->
                   Syn_AGARuleItf 
wrap_AGARuleItf sem (Inh_AGARuleItf _lhsIcroNmL _lhsIfmGam _lhsIopts )  =
    (let ( _lhsOreplUniq) = sem _lhsIcroNmL _lhsIfmGam _lhsIopts 
     in  (Syn_AGARuleItf _lhsOreplUniq ))
sem_AGARuleItf_AGItf :: T_ARule  ->
                        T_AGARuleItf 
sem_AGARuleItf_AGItf rule_  =
    (\ _lhsIcroNmL
       _lhsIfmGam
       _lhsIopts ->
         (let _ruleOuniqThrDst :: ANm 
              _lhsOreplUniq :: ARule 
              _ruleOfmGam :: (FmGam Expr)
              _ruleOopts :: Opts
              _ruleIreplUniq :: ARule 
              -- "build/ruler2/ARule/PatternUniq.ag"(line 57, column 21)
              _ruleOuniqThrDst =
                  case _lhsIcroNmL of
                    (_:[d]) -> ANm_Lhs (fmNmUniq _lhsIopts _lhsIfmGam 0) []
                    (_:d:_) -> ANm_Node d (fmNmUniq _lhsIopts _lhsIfmGam 0)
              -- copy rule (up)
              _lhsOreplUniq =
                  _ruleIreplUniq
              -- copy rule (down)
              _ruleOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _ruleOopts =
                  _lhsIopts
              ( _ruleIreplUniq) =
                  rule_ _ruleOfmGam _ruleOopts _ruleOuniqThrDst 
          in  ( _lhsOreplUniq)))
-- AGExprItf ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
   alternatives:
      alternative AGItf:
         child expr           : Expr 
         visit 0:
            local uniqSeqNr   : _
-}
-- cata
sem_AGExprItf :: AGExprItf  ->
                 T_AGExprItf 
sem_AGExprItf (AGExprItf_AGItf _expr )  =
    (sem_AGExprItf_AGItf (sem_Expr _expr ) )
-- semantic domain
type T_AGExprItf  = (FmGam Expr) ->
                    Opts ->
                    ( )
sem_AGExprItf_AGItf :: T_Expr  ->
                       T_AGExprItf 
sem_AGExprItf_AGItf expr_  =
    (\ _lhsIfmGam
       _lhsIopts ->
         (let _exprOfmGam :: (FmGam Expr)
              _exprOopts :: Opts
              _exprOuniqSeqNr :: Int
              _exprIreplUniq :: Expr 
              _exprIuniqSeqNr :: Int
              -- "build/ruler2/ARule/PatternUniq.ag"(line 49, column 21)
              _uniqSeqNr =
                  0
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (from local)
              _exprOuniqSeqNr =
                  _uniqSeqNr
              ( _exprIreplUniq,_exprIuniqSeqNr) =
                  expr_ _exprOfmGam _exprOopts _exprOuniqSeqNr 
          in  ( )))
-- ANm ---------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         opts                 : Opts
      chained attribute:
         uniqSeqNr            : Int
      synthesized attribute:
         replUniq             : SELF 
   alternatives:
      alternative Fld:
         child nm             : {Nm}
         visit 0:
            local replUniq    : _
      alternative Lhs:
         child nm             : {Nm}
         child props          : {[AtProp]}
         visit 0:
            local replUniq    : _
      alternative Loc:
         child nm             : {Nm}
         child props          : {[AtProp]}
         visit 0:
            local replUniq    : _
      alternative Node:
         child ndNm           : {Nm}
         child nm             : {Nm}
         visit 0:
            local replUniq    : _
      alternative Wild:
         visit 0:
            local replUniq    : _
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
type T_ANm  = Opts ->
              Int ->
              ( ANm ,Int)
sem_ANm_Fld :: Nm ->
               T_ANm 
sem_ANm_Fld nm_  =
    (\ _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: ANm 
              _lhsOuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  ANm_Fld nm_
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (chain)
              _lhsOuniqSeqNr =
                  _lhsIuniqSeqNr
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_ANm_Lhs :: Nm ->
               ([AtProp]) ->
               T_ANm 
sem_ANm_Lhs nm_ props_  =
    (\ _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: ANm 
              _lhsOuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  ANm_Lhs nm_ props_
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (chain)
              _lhsOuniqSeqNr =
                  _lhsIuniqSeqNr
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_ANm_Loc :: Nm ->
               ([AtProp]) ->
               T_ANm 
sem_ANm_Loc nm_ props_  =
    (\ _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: ANm 
              _lhsOuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  ANm_Loc nm_ props_
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (chain)
              _lhsOuniqSeqNr =
                  _lhsIuniqSeqNr
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_ANm_Node :: Nm ->
                Nm ->
                T_ANm 
sem_ANm_Node ndNm_ nm_  =
    (\ _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: ANm 
              _lhsOuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  ANm_Node ndNm_ nm_
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (chain)
              _lhsOuniqSeqNr =
                  _lhsIuniqSeqNr
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_ANm_Wild :: T_ANm 
sem_ANm_Wild  =
    (\ _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: ANm 
              _lhsOuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  ANm_Wild
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (chain)
              _lhsOuniqSeqNr =
                  _lhsIuniqSeqNr
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
-- ARule -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
         uniqThrDst           : ANm 
      synthesized attribute:
         replUniq             : SELF 
   alternatives:
      alternative Rule:
         child ndNmL          : {[Nm]}
         child rlNm           : {Nm}
         child info           : {[String]}
         child eqns           : AEqns 
         visit 0:
            local mkUniqEqnL  : _
            local replUniq    : _
-}
-- cata
sem_ARule :: ARule  ->
             T_ARule 
sem_ARule (ARule_Rule _ndNmL _rlNm _info _eqns )  =
    (sem_ARule_Rule _ndNmL _rlNm _info (sem_AEqns _eqns ) )
-- semantic domain
type T_ARule  = (FmGam Expr) ->
                Opts ->
                ANm  ->
                ( ARule )
sem_ARule_Rule :: ([Nm]) ->
                  Nm ->
                  ([String]) ->
                  T_AEqns  ->
                  T_ARule 
sem_ARule_Rule ndNmL_ rlNm_ info_ eqns_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqThrDst ->
         (let _eqnsOuniqSeqNr :: Int
              _lhsOreplUniq :: ARule 
              _eqnsOfmGam :: (FmGam Expr)
              _eqnsOopts :: Opts
              _eqnsIreplUniq :: AEqns 
              _eqnsIuniqSeqNr :: Int
              -- "build/ruler2/ARule/PatternUniq.ag"(line 42, column 21)
              _eqnsOuniqSeqNr =
                  0
              -- "build/ruler2/ARule/PatternUniq.ag"(line 62, column 21)
              _mkUniqEqnL =
                  if _eqnsIuniqSeqNr > 0
                  then [AEqn_Eqn
                          (AEqnDest_Many
                            (map AEqnDest_One
                                 (_lhsIuniqThrDst : [ ANm_Loc (fmNmUniq _lhsIopts _lhsIfmGam u) [] | u <- [1 .. _eqnsIuniqSeqNr] ])
                            )
                          )
                          (AExpr_Expr
                            (mkExprApp (exprSubst (_lhsIopts {optGenFM = FmFmtCmd}) _lhsIfmGam . Expr_Var . nmFunMkUniq $ _eqnsIuniqSeqNr)
                                       [mkALhs (fmNmUniq _lhsIopts _lhsIfmGam 0)]
                            )
                          )
                       ]
                  else []
              -- "build/ruler2/ARule/PatternUniq.ag"(line 81, column 21)
              _lhsOreplUniq =
                  ARule_Rule ndNmL_ rlNm_ info_ (_mkUniqEqnL ++ _eqnsIreplUniq)
              -- self rule
              _replUniq =
                  ARule_Rule ndNmL_ rlNm_ info_ _eqnsIreplUniq
              -- copy rule (down)
              _eqnsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _eqnsOopts =
                  _lhsIopts
              ( _eqnsIreplUniq,_eqnsIuniqSeqNr) =
                  eqns_ _eqnsOfmGam _eqnsOopts _eqnsOuniqSeqNr 
          in  ( _lhsOreplUniq)))
-- ARules ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
         uniqThrDst           : ANm 
      synthesized attribute:
         replUniq             : SELF 
   alternatives:
      alternative Cons:
         child hd             : ARule 
         child tl             : ARules 
         visit 0:
            local replUniq    : _
      alternative Nil:
         visit 0:
            local replUniq    : _
-}
-- cata
sem_ARules :: ARules  ->
              T_ARules 
sem_ARules list  =
    (Prelude.foldr sem_ARules_Cons sem_ARules_Nil (Prelude.map sem_ARule list) )
-- semantic domain
type T_ARules  = (FmGam Expr) ->
                 Opts ->
                 ANm  ->
                 ( ARules )
sem_ARules_Cons :: T_ARule  ->
                   T_ARules  ->
                   T_ARules 
sem_ARules_Cons hd_ tl_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqThrDst ->
         (let _lhsOreplUniq :: ARules 
              _hdOfmGam :: (FmGam Expr)
              _hdOopts :: Opts
              _hdOuniqThrDst :: ANm 
              _tlOfmGam :: (FmGam Expr)
              _tlOopts :: Opts
              _tlOuniqThrDst :: ANm 
              _hdIreplUniq :: ARule 
              _tlIreplUniq :: ARules 
              -- self rule
              _replUniq =
                  (:) _hdIreplUniq _tlIreplUniq
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (down)
              _hdOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOuniqThrDst =
                  _lhsIuniqThrDst
              -- copy rule (down)
              _tlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOuniqThrDst =
                  _lhsIuniqThrDst
              ( _hdIreplUniq) =
                  hd_ _hdOfmGam _hdOopts _hdOuniqThrDst 
              ( _tlIreplUniq) =
                  tl_ _tlOfmGam _tlOopts _tlOuniqThrDst 
          in  ( _lhsOreplUniq)))
sem_ARules_Nil :: T_ARules 
sem_ARules_Nil  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqThrDst ->
         (let _lhsOreplUniq :: ARules 
              -- self rule
              _replUniq =
                  []
              -- self rule
              _lhsOreplUniq =
                  _replUniq
          in  ( _lhsOreplUniq)))
-- ECnstr ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
      chained attribute:
         uniqSeqNr            : Int
      synthesized attribute:
         replUniq             : SELF 
   alternatives:
      alternative Empty:
         visit 0:
            local replUniq    : _
      alternative Ty:
         child nms            : {[Nm]}
         visit 0:
            local replUniq    : _
      alternative Var:
         child nm             : {Nm}
         visit 0:
            local replUniq    : _
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
                 Opts ->
                 Int ->
                 ( ECnstr ,Int)
sem_ECnstr_Empty :: T_ECnstr 
sem_ECnstr_Empty  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: ECnstr 
              _lhsOuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  ECnstr_Empty
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (chain)
              _lhsOuniqSeqNr =
                  _lhsIuniqSeqNr
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_ECnstr_Ty :: ([Nm]) ->
                 T_ECnstr 
sem_ECnstr_Ty nms_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: ECnstr 
              _lhsOuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  ECnstr_Ty nms_
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (chain)
              _lhsOuniqSeqNr =
                  _lhsIuniqSeqNr
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_ECnstr_Var :: Nm ->
                  T_ECnstr 
sem_ECnstr_Var nm_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: ECnstr 
              _lhsOuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  ECnstr_Var nm_
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (chain)
              _lhsOuniqSeqNr =
                  _lhsIuniqSeqNr
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
-- Expr --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
      chained attribute:
         uniqSeqNr            : Int
      synthesized attribute:
         replUniq             : SELF 
   alternatives:
      alternative AVar:
         child anm            : ANm 
         visit 0:
            local replUniq    : _
      alternative App:
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local replUniq    : _
      alternative AppTop:
         child expr           : Expr 
         visit 0:
            local replUniq    : _
      alternative ChildOrder:
         child seqNr          : {Int}
         child expr           : Expr 
         visit 0:
            local replUniq    : _
      alternative Cnstr:
         child expr           : Expr 
         child cnstr          : ECnstr 
         visit 0:
            local replUniq    : _
      alternative Empty:
         visit 0:
            local replUniq    : _
      alternative Expr:
         child expr           : Expr 
         visit 0:
            local replUniq    : _
      alternative Int:
         child int            : {String}
         visit 0:
            local replUniq    : _
      alternative LF:
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local replUniq    : _
      alternative Named:
         child nm             : {Nm}
         child expr           : Expr 
         visit 0:
            local replUniq    : _
      alternative Op:
         child nm             : {Nm}
         child nmExpr         : Expr 
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local replUniq    : _
      alternative Paren:
         child expr           : Expr 
         visit 0:
            local replUniq    : _
      alternative Retain:
         child expr           : Expr 
         visit 0:
            local replUniq    : _
      alternative SP:
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local replUniq    : _
      alternative Sel:
         child expr           : Expr 
         child selMbExpr      : MbExpr 
         visit 0:
            local replUniq    : _
      alternative SelTop:
         child expr           : Expr 
         visit 0:
            local replUniq    : _
      alternative StrAsIs:
         child str            : {String}
         visit 0:
            local replUniq    : _
      alternative StrText:
         child str            : {String}
         visit 0:
            local replUniq    : _
      alternative Undefined:
         visit 0:
            local replUniq    : _
      alternative Uniq:
         visit 0:
            local uniqSeqNr   : _
            local nm          : _
            local replUniq    : _
      alternative Var:
         child nm             : {Nm}
         visit 0:
            local replUniq    : _
      alternative Wrap:
         child wrKind         : {WrKind}
         child expr           : Expr 
         visit 0:
            local replUniq    : _
      alternative WrapCnstr:
         child cnstr          : ECnstr 
         visit 0:
            local replUniq    : _
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
               Opts ->
               Int ->
               ( Expr ,Int)
sem_Expr_AVar :: T_ANm  ->
                 T_Expr 
sem_Expr_AVar anm_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: Expr 
              _lhsOuniqSeqNr :: Int
              _anmOopts :: Opts
              _anmOuniqSeqNr :: Int
              _anmIreplUniq :: ANm 
              _anmIuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  Expr_AVar _anmIreplUniq
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (up)
              _lhsOuniqSeqNr =
                  _anmIuniqSeqNr
              -- copy rule (down)
              _anmOopts =
                  _lhsIopts
              -- copy rule (down)
              _anmOuniqSeqNr =
                  _lhsIuniqSeqNr
              ( _anmIreplUniq,_anmIuniqSeqNr) =
                  anm_ _anmOopts _anmOuniqSeqNr 
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_Expr_App :: T_Expr  ->
                T_Expr  ->
                T_Expr 
sem_Expr_App lExpr_ rExpr_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: Expr 
              _lhsOuniqSeqNr :: Int
              _lExprOfmGam :: (FmGam Expr)
              _lExprOopts :: Opts
              _lExprOuniqSeqNr :: Int
              _rExprOfmGam :: (FmGam Expr)
              _rExprOopts :: Opts
              _rExprOuniqSeqNr :: Int
              _lExprIreplUniq :: Expr 
              _lExprIuniqSeqNr :: Int
              _rExprIreplUniq :: Expr 
              _rExprIuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  Expr_App _lExprIreplUniq _rExprIreplUniq
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (up)
              _lhsOuniqSeqNr =
                  _rExprIuniqSeqNr
              -- copy rule (down)
              _lExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _lExprOopts =
                  _lhsIopts
              -- copy rule (down)
              _lExprOuniqSeqNr =
                  _lhsIuniqSeqNr
              -- copy rule (down)
              _rExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _rExprOopts =
                  _lhsIopts
              -- copy rule (chain)
              _rExprOuniqSeqNr =
                  _lExprIuniqSeqNr
              ( _lExprIreplUniq,_lExprIuniqSeqNr) =
                  lExpr_ _lExprOfmGam _lExprOopts _lExprOuniqSeqNr 
              ( _rExprIreplUniq,_rExprIuniqSeqNr) =
                  rExpr_ _rExprOfmGam _rExprOopts _rExprOuniqSeqNr 
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_Expr_AppTop :: T_Expr  ->
                   T_Expr 
sem_Expr_AppTop expr_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: Expr 
              _lhsOuniqSeqNr :: Int
              _exprOfmGam :: (FmGam Expr)
              _exprOopts :: Opts
              _exprOuniqSeqNr :: Int
              _exprIreplUniq :: Expr 
              _exprIuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  Expr_AppTop _exprIreplUniq
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (up)
              _lhsOuniqSeqNr =
                  _exprIuniqSeqNr
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOuniqSeqNr =
                  _lhsIuniqSeqNr
              ( _exprIreplUniq,_exprIuniqSeqNr) =
                  expr_ _exprOfmGam _exprOopts _exprOuniqSeqNr 
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_Expr_ChildOrder :: Int ->
                       T_Expr  ->
                       T_Expr 
sem_Expr_ChildOrder seqNr_ expr_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: Expr 
              _lhsOuniqSeqNr :: Int
              _exprOfmGam :: (FmGam Expr)
              _exprOopts :: Opts
              _exprOuniqSeqNr :: Int
              _exprIreplUniq :: Expr 
              _exprIuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  Expr_ChildOrder seqNr_ _exprIreplUniq
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (up)
              _lhsOuniqSeqNr =
                  _exprIuniqSeqNr
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOuniqSeqNr =
                  _lhsIuniqSeqNr
              ( _exprIreplUniq,_exprIuniqSeqNr) =
                  expr_ _exprOfmGam _exprOopts _exprOuniqSeqNr 
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_Expr_Cnstr :: T_Expr  ->
                  T_ECnstr  ->
                  T_Expr 
sem_Expr_Cnstr expr_ cnstr_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: Expr 
              _lhsOuniqSeqNr :: Int
              _exprOfmGam :: (FmGam Expr)
              _exprOopts :: Opts
              _exprOuniqSeqNr :: Int
              _cnstrOfmGam :: (FmGam Expr)
              _cnstrOopts :: Opts
              _cnstrOuniqSeqNr :: Int
              _exprIreplUniq :: Expr 
              _exprIuniqSeqNr :: Int
              _cnstrIreplUniq :: ECnstr 
              _cnstrIuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  Expr_Cnstr _exprIreplUniq _cnstrIreplUniq
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (up)
              _lhsOuniqSeqNr =
                  _cnstrIuniqSeqNr
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOuniqSeqNr =
                  _lhsIuniqSeqNr
              -- copy rule (down)
              _cnstrOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _cnstrOopts =
                  _lhsIopts
              -- copy rule (chain)
              _cnstrOuniqSeqNr =
                  _exprIuniqSeqNr
              ( _exprIreplUniq,_exprIuniqSeqNr) =
                  expr_ _exprOfmGam _exprOopts _exprOuniqSeqNr 
              ( _cnstrIreplUniq,_cnstrIuniqSeqNr) =
                  cnstr_ _cnstrOfmGam _cnstrOopts _cnstrOuniqSeqNr 
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_Expr_Empty :: T_Expr 
sem_Expr_Empty  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: Expr 
              _lhsOuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  Expr_Empty
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (chain)
              _lhsOuniqSeqNr =
                  _lhsIuniqSeqNr
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_Expr_Expr :: T_Expr  ->
                 T_Expr 
sem_Expr_Expr expr_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: Expr 
              _lhsOuniqSeqNr :: Int
              _exprOfmGam :: (FmGam Expr)
              _exprOopts :: Opts
              _exprOuniqSeqNr :: Int
              _exprIreplUniq :: Expr 
              _exprIuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  Expr_Expr _exprIreplUniq
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (up)
              _lhsOuniqSeqNr =
                  _exprIuniqSeqNr
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOuniqSeqNr =
                  _lhsIuniqSeqNr
              ( _exprIreplUniq,_exprIuniqSeqNr) =
                  expr_ _exprOfmGam _exprOopts _exprOuniqSeqNr 
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_Expr_Int :: String ->
                T_Expr 
sem_Expr_Int int_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: Expr 
              _lhsOuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  Expr_Int int_
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (chain)
              _lhsOuniqSeqNr =
                  _lhsIuniqSeqNr
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_Expr_LF :: T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_LF lExpr_ rExpr_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: Expr 
              _lhsOuniqSeqNr :: Int
              _lExprOfmGam :: (FmGam Expr)
              _lExprOopts :: Opts
              _lExprOuniqSeqNr :: Int
              _rExprOfmGam :: (FmGam Expr)
              _rExprOopts :: Opts
              _rExprOuniqSeqNr :: Int
              _lExprIreplUniq :: Expr 
              _lExprIuniqSeqNr :: Int
              _rExprIreplUniq :: Expr 
              _rExprIuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  Expr_LF _lExprIreplUniq _rExprIreplUniq
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (up)
              _lhsOuniqSeqNr =
                  _rExprIuniqSeqNr
              -- copy rule (down)
              _lExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _lExprOopts =
                  _lhsIopts
              -- copy rule (down)
              _lExprOuniqSeqNr =
                  _lhsIuniqSeqNr
              -- copy rule (down)
              _rExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _rExprOopts =
                  _lhsIopts
              -- copy rule (chain)
              _rExprOuniqSeqNr =
                  _lExprIuniqSeqNr
              ( _lExprIreplUniq,_lExprIuniqSeqNr) =
                  lExpr_ _lExprOfmGam _lExprOopts _lExprOuniqSeqNr 
              ( _rExprIreplUniq,_rExprIuniqSeqNr) =
                  rExpr_ _rExprOfmGam _rExprOopts _rExprOuniqSeqNr 
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_Expr_Named :: Nm ->
                  T_Expr  ->
                  T_Expr 
sem_Expr_Named nm_ expr_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: Expr 
              _lhsOuniqSeqNr :: Int
              _exprOfmGam :: (FmGam Expr)
              _exprOopts :: Opts
              _exprOuniqSeqNr :: Int
              _exprIreplUniq :: Expr 
              _exprIuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  Expr_Named nm_ _exprIreplUniq
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (up)
              _lhsOuniqSeqNr =
                  _exprIuniqSeqNr
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOuniqSeqNr =
                  _lhsIuniqSeqNr
              ( _exprIreplUniq,_exprIuniqSeqNr) =
                  expr_ _exprOfmGam _exprOopts _exprOuniqSeqNr 
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_Expr_Op :: Nm ->
               T_Expr  ->
               T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_Op nm_ nmExpr_ lExpr_ rExpr_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: Expr 
              _lhsOuniqSeqNr :: Int
              _nmExprOfmGam :: (FmGam Expr)
              _nmExprOopts :: Opts
              _nmExprOuniqSeqNr :: Int
              _lExprOfmGam :: (FmGam Expr)
              _lExprOopts :: Opts
              _lExprOuniqSeqNr :: Int
              _rExprOfmGam :: (FmGam Expr)
              _rExprOopts :: Opts
              _rExprOuniqSeqNr :: Int
              _nmExprIreplUniq :: Expr 
              _nmExprIuniqSeqNr :: Int
              _lExprIreplUniq :: Expr 
              _lExprIuniqSeqNr :: Int
              _rExprIreplUniq :: Expr 
              _rExprIuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  Expr_Op nm_ _nmExprIreplUniq _lExprIreplUniq _rExprIreplUniq
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (up)
              _lhsOuniqSeqNr =
                  _rExprIuniqSeqNr
              -- copy rule (down)
              _nmExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _nmExprOopts =
                  _lhsIopts
              -- copy rule (down)
              _nmExprOuniqSeqNr =
                  _lhsIuniqSeqNr
              -- copy rule (down)
              _lExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _lExprOopts =
                  _lhsIopts
              -- copy rule (chain)
              _lExprOuniqSeqNr =
                  _nmExprIuniqSeqNr
              -- copy rule (down)
              _rExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _rExprOopts =
                  _lhsIopts
              -- copy rule (chain)
              _rExprOuniqSeqNr =
                  _lExprIuniqSeqNr
              ( _nmExprIreplUniq,_nmExprIuniqSeqNr) =
                  nmExpr_ _nmExprOfmGam _nmExprOopts _nmExprOuniqSeqNr 
              ( _lExprIreplUniq,_lExprIuniqSeqNr) =
                  lExpr_ _lExprOfmGam _lExprOopts _lExprOuniqSeqNr 
              ( _rExprIreplUniq,_rExprIuniqSeqNr) =
                  rExpr_ _rExprOfmGam _rExprOopts _rExprOuniqSeqNr 
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_Expr_Paren :: T_Expr  ->
                  T_Expr 
sem_Expr_Paren expr_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: Expr 
              _lhsOuniqSeqNr :: Int
              _exprOfmGam :: (FmGam Expr)
              _exprOopts :: Opts
              _exprOuniqSeqNr :: Int
              _exprIreplUniq :: Expr 
              _exprIuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  Expr_Paren _exprIreplUniq
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (up)
              _lhsOuniqSeqNr =
                  _exprIuniqSeqNr
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOuniqSeqNr =
                  _lhsIuniqSeqNr
              ( _exprIreplUniq,_exprIuniqSeqNr) =
                  expr_ _exprOfmGam _exprOopts _exprOuniqSeqNr 
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_Expr_Retain :: T_Expr  ->
                   T_Expr 
sem_Expr_Retain expr_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: Expr 
              _lhsOuniqSeqNr :: Int
              _exprOfmGam :: (FmGam Expr)
              _exprOopts :: Opts
              _exprOuniqSeqNr :: Int
              _exprIreplUniq :: Expr 
              _exprIuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  Expr_Retain _exprIreplUniq
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (up)
              _lhsOuniqSeqNr =
                  _exprIuniqSeqNr
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOuniqSeqNr =
                  _lhsIuniqSeqNr
              ( _exprIreplUniq,_exprIuniqSeqNr) =
                  expr_ _exprOfmGam _exprOopts _exprOuniqSeqNr 
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_Expr_SP :: T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_SP lExpr_ rExpr_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: Expr 
              _lhsOuniqSeqNr :: Int
              _lExprOfmGam :: (FmGam Expr)
              _lExprOopts :: Opts
              _lExprOuniqSeqNr :: Int
              _rExprOfmGam :: (FmGam Expr)
              _rExprOopts :: Opts
              _rExprOuniqSeqNr :: Int
              _lExprIreplUniq :: Expr 
              _lExprIuniqSeqNr :: Int
              _rExprIreplUniq :: Expr 
              _rExprIuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  Expr_SP _lExprIreplUniq _rExprIreplUniq
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (up)
              _lhsOuniqSeqNr =
                  _rExprIuniqSeqNr
              -- copy rule (down)
              _lExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _lExprOopts =
                  _lhsIopts
              -- copy rule (down)
              _lExprOuniqSeqNr =
                  _lhsIuniqSeqNr
              -- copy rule (down)
              _rExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _rExprOopts =
                  _lhsIopts
              -- copy rule (chain)
              _rExprOuniqSeqNr =
                  _lExprIuniqSeqNr
              ( _lExprIreplUniq,_lExprIuniqSeqNr) =
                  lExpr_ _lExprOfmGam _lExprOopts _lExprOuniqSeqNr 
              ( _rExprIreplUniq,_rExprIuniqSeqNr) =
                  rExpr_ _rExprOfmGam _rExprOopts _rExprOuniqSeqNr 
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_Expr_Sel :: T_Expr  ->
                T_MbExpr  ->
                T_Expr 
sem_Expr_Sel expr_ selMbExpr_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: Expr 
              _lhsOuniqSeqNr :: Int
              _exprOfmGam :: (FmGam Expr)
              _exprOopts :: Opts
              _exprOuniqSeqNr :: Int
              _selMbExprOfmGam :: (FmGam Expr)
              _selMbExprOopts :: Opts
              _selMbExprOuniqSeqNr :: Int
              _exprIreplUniq :: Expr 
              _exprIuniqSeqNr :: Int
              _selMbExprIreplUniq :: MbExpr 
              _selMbExprIuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  Expr_Sel _exprIreplUniq _selMbExprIreplUniq
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (up)
              _lhsOuniqSeqNr =
                  _selMbExprIuniqSeqNr
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOuniqSeqNr =
                  _lhsIuniqSeqNr
              -- copy rule (down)
              _selMbExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _selMbExprOopts =
                  _lhsIopts
              -- copy rule (chain)
              _selMbExprOuniqSeqNr =
                  _exprIuniqSeqNr
              ( _exprIreplUniq,_exprIuniqSeqNr) =
                  expr_ _exprOfmGam _exprOopts _exprOuniqSeqNr 
              ( _selMbExprIreplUniq,_selMbExprIuniqSeqNr) =
                  selMbExpr_ _selMbExprOfmGam _selMbExprOopts _selMbExprOuniqSeqNr 
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_Expr_SelTop :: T_Expr  ->
                   T_Expr 
sem_Expr_SelTop expr_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: Expr 
              _lhsOuniqSeqNr :: Int
              _exprOfmGam :: (FmGam Expr)
              _exprOopts :: Opts
              _exprOuniqSeqNr :: Int
              _exprIreplUniq :: Expr 
              _exprIuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  Expr_SelTop _exprIreplUniq
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (up)
              _lhsOuniqSeqNr =
                  _exprIuniqSeqNr
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOuniqSeqNr =
                  _lhsIuniqSeqNr
              ( _exprIreplUniq,_exprIuniqSeqNr) =
                  expr_ _exprOfmGam _exprOopts _exprOuniqSeqNr 
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_Expr_StrAsIs :: String ->
                    T_Expr 
sem_Expr_StrAsIs str_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: Expr 
              _lhsOuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  Expr_StrAsIs str_
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (chain)
              _lhsOuniqSeqNr =
                  _lhsIuniqSeqNr
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_Expr_StrText :: String ->
                    T_Expr 
sem_Expr_StrText str_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: Expr 
              _lhsOuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  Expr_StrText str_
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (chain)
              _lhsOuniqSeqNr =
                  _lhsIuniqSeqNr
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_Expr_Undefined :: T_Expr 
sem_Expr_Undefined  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: Expr 
              _lhsOuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  Expr_Undefined
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (chain)
              _lhsOuniqSeqNr =
                  _lhsIuniqSeqNr
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_Expr_Uniq :: T_Expr 
sem_Expr_Uniq  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOuniqSeqNr :: Int
              _lhsOreplUniq :: Expr 
              -- "build/ruler2/ARule/PatternUniq.ag"(line 45, column 21)
              _uniqSeqNr =
                  _lhsIuniqSeqNr + 1
              -- "build/ruler2/ARule/PatternUniq.ag"(line 46, column 21)
              _lhsOuniqSeqNr =
                  _uniqSeqNr
              -- "build/ruler2/ARule/PatternUniq.ag"(line 52, column 21)
              _nm =
                  fmNmUniq _lhsIopts _lhsIfmGam _uniqSeqNr
              -- "build/ruler2/ARule/PatternUniq.ag"(line 84, column 21)
              _lhsOreplUniq =
                  mkALoc _nm
              -- self rule
              _replUniq =
                  Expr_Uniq
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_Expr_Var :: Nm ->
                T_Expr 
sem_Expr_Var nm_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: Expr 
              _lhsOuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  Expr_Var nm_
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (chain)
              _lhsOuniqSeqNr =
                  _lhsIuniqSeqNr
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_Expr_Wrap :: WrKind ->
                 T_Expr  ->
                 T_Expr 
sem_Expr_Wrap wrKind_ expr_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: Expr 
              _lhsOuniqSeqNr :: Int
              _exprOfmGam :: (FmGam Expr)
              _exprOopts :: Opts
              _exprOuniqSeqNr :: Int
              _exprIreplUniq :: Expr 
              _exprIuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  Expr_Wrap wrKind_ _exprIreplUniq
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (up)
              _lhsOuniqSeqNr =
                  _exprIuniqSeqNr
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOuniqSeqNr =
                  _lhsIuniqSeqNr
              ( _exprIreplUniq,_exprIuniqSeqNr) =
                  expr_ _exprOfmGam _exprOopts _exprOuniqSeqNr 
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_Expr_WrapCnstr :: T_ECnstr  ->
                      T_Expr 
sem_Expr_WrapCnstr cnstr_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: Expr 
              _lhsOuniqSeqNr :: Int
              _cnstrOfmGam :: (FmGam Expr)
              _cnstrOopts :: Opts
              _cnstrOuniqSeqNr :: Int
              _cnstrIreplUniq :: ECnstr 
              _cnstrIuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  Expr_WrapCnstr _cnstrIreplUniq
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (up)
              _lhsOuniqSeqNr =
                  _cnstrIuniqSeqNr
              -- copy rule (down)
              _cnstrOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _cnstrOopts =
                  _lhsIopts
              -- copy rule (down)
              _cnstrOuniqSeqNr =
                  _lhsIuniqSeqNr
              ( _cnstrIreplUniq,_cnstrIuniqSeqNr) =
                  cnstr_ _cnstrOfmGam _cnstrOopts _cnstrOuniqSeqNr 
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
-- MbExpr ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
      chained attribute:
         uniqSeqNr            : Int
      synthesized attribute:
         replUniq             : SELF 
   alternatives:
      alternative Just:
         child just           : Expr 
         visit 0:
            local replUniq    : _
      alternative Nothing:
         visit 0:
            local replUniq    : _
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
                 Opts ->
                 Int ->
                 ( MbExpr ,Int)
sem_MbExpr_Just :: T_Expr  ->
                   T_MbExpr 
sem_MbExpr_Just just_  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: MbExpr 
              _lhsOuniqSeqNr :: Int
              _justOfmGam :: (FmGam Expr)
              _justOopts :: Opts
              _justOuniqSeqNr :: Int
              _justIreplUniq :: Expr 
              _justIuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  Just _justIreplUniq
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (up)
              _lhsOuniqSeqNr =
                  _justIuniqSeqNr
              -- copy rule (down)
              _justOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _justOopts =
                  _lhsIopts
              -- copy rule (down)
              _justOuniqSeqNr =
                  _lhsIuniqSeqNr
              ( _justIreplUniq,_justIuniqSeqNr) =
                  just_ _justOfmGam _justOopts _justOuniqSeqNr 
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
sem_MbExpr_Nothing :: T_MbExpr 
sem_MbExpr_Nothing  =
    (\ _lhsIfmGam
       _lhsIopts
       _lhsIuniqSeqNr ->
         (let _lhsOreplUniq :: MbExpr 
              _lhsOuniqSeqNr :: Int
              -- self rule
              _replUniq =
                  Nothing
              -- self rule
              _lhsOreplUniq =
                  _replUniq
              -- copy rule (chain)
              _lhsOuniqSeqNr =
                  _lhsIuniqSeqNr
          in  ( _lhsOreplUniq,_lhsOuniqSeqNr)))
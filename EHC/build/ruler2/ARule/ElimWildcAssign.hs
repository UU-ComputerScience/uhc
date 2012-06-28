

-- UUAGC 0.9.39.1 (build/ruler2/ARule/ElimWildcAssign.ag)
module ARule.ElimWildcAssign(arlElimWild) where

import qualified Data.Map as Map
import Common
import Expr.Expr
import ARule.ARule










arlElimWild :: ARule -> ARule
arlElimWild rl
  = replEw_Syn_AGARuleItf r2
  where r1 = sem_AGARuleItf (AGARuleItf_AGItf rl)
        r2 = wrap_AGARuleItf r1
                (Inh_AGARuleItf)

-- AEqn --------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         replEw               : SELF 
         replEwEqns           : [AEqn]
   alternatives:
      alternative Eqn:
         child dest           : AEqnDest 
         child val            : AExpr 
         visit 0:
            local replEw      : _
      alternative Err:
         child expr           : Expr 
         visit 0:
            local replEw      : _
-}
-- cata
sem_AEqn :: AEqn  ->
            T_AEqn 
sem_AEqn (AEqn_Eqn _dest _val )  =
    (sem_AEqn_Eqn (sem_AEqnDest _dest ) (sem_AExpr _val ) )
sem_AEqn (AEqn_Err _expr )  =
    (sem_AEqn_Err (sem_Expr _expr ) )
-- semantic domain
type T_AEqn  = ( AEqn ,([AEqn]))
sem_AEqn_Eqn :: T_AEqnDest  ->
                T_AExpr  ->
                T_AEqn 
sem_AEqn_Eqn dest_ val_  =
    (let _lhsOreplEwEqns :: ([AEqn])
         _lhsOreplEw :: AEqn 
         _destIreplEw :: AEqnDest 
         _valIreplEw :: AExpr 
         -- "build/ruler2/ARule/ElimWildcAssign.ag"(line 34, column 21)
         _lhsOreplEwEqns =
             case _destIreplEw of
               AEqnDest_One ANm_Wild -> []
               _                     -> [_replEw]
         -- self rule
         _replEw =
             AEqn_Eqn _destIreplEw _valIreplEw
         -- self rule
         _lhsOreplEw =
             _replEw
         ( _destIreplEw) =
             dest_ 
         ( _valIreplEw) =
             val_ 
     in  ( _lhsOreplEw,_lhsOreplEwEqns))
sem_AEqn_Err :: T_Expr  ->
                T_AEqn 
sem_AEqn_Err expr_  =
    (let _lhsOreplEwEqns :: ([AEqn])
         _lhsOreplEw :: AEqn 
         _exprIreplEw :: Expr 
         -- "build/ruler2/ARule/ElimWildcAssign.ag"(line 37, column 21)
         _lhsOreplEwEqns =
             [_replEw]
         -- self rule
         _replEw =
             AEqn_Err _exprIreplEw
         -- self rule
         _lhsOreplEw =
             _replEw
         ( _exprIreplEw) =
             expr_ 
     in  ( _lhsOreplEw,_lhsOreplEwEqns))
-- AEqnDest ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         replEw               : SELF 
   alternatives:
      alternative Many:
         child dests          : AEqnDests 
         visit 0:
            local replEw      : _
      alternative One:
         child anm            : ANm 
         visit 0:
            local replEw      : _
-}
-- cata
sem_AEqnDest :: AEqnDest  ->
                T_AEqnDest 
sem_AEqnDest (AEqnDest_Many _dests )  =
    (sem_AEqnDest_Many (sem_AEqnDests _dests ) )
sem_AEqnDest (AEqnDest_One _anm )  =
    (sem_AEqnDest_One (sem_ANm _anm ) )
-- semantic domain
type T_AEqnDest  = ( AEqnDest )
sem_AEqnDest_Many :: T_AEqnDests  ->
                     T_AEqnDest 
sem_AEqnDest_Many dests_  =
    (let _lhsOreplEw :: AEqnDest 
         _destsIreplEw :: AEqnDests 
         -- self rule
         _replEw =
             AEqnDest_Many _destsIreplEw
         -- self rule
         _lhsOreplEw =
             _replEw
         ( _destsIreplEw) =
             dests_ 
     in  ( _lhsOreplEw))
sem_AEqnDest_One :: T_ANm  ->
                    T_AEqnDest 
sem_AEqnDest_One anm_  =
    (let _lhsOreplEw :: AEqnDest 
         _anmIreplEw :: ANm 
         -- self rule
         _replEw =
             AEqnDest_One _anmIreplEw
         -- self rule
         _lhsOreplEw =
             _replEw
         ( _anmIreplEw) =
             anm_ 
     in  ( _lhsOreplEw))
-- AEqnDests ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         replEw               : SELF 
   alternatives:
      alternative Cons:
         child hd             : AEqnDest 
         child tl             : AEqnDests 
         visit 0:
            local replEw      : _
      alternative Nil:
         visit 0:
            local replEw      : _
-}
-- cata
sem_AEqnDests :: AEqnDests  ->
                 T_AEqnDests 
sem_AEqnDests list  =
    (Prelude.foldr sem_AEqnDests_Cons sem_AEqnDests_Nil (Prelude.map sem_AEqnDest list) )
-- semantic domain
type T_AEqnDests  = ( AEqnDests )
sem_AEqnDests_Cons :: T_AEqnDest  ->
                      T_AEqnDests  ->
                      T_AEqnDests 
sem_AEqnDests_Cons hd_ tl_  =
    (let _lhsOreplEw :: AEqnDests 
         _hdIreplEw :: AEqnDest 
         _tlIreplEw :: AEqnDests 
         -- self rule
         _replEw =
             (:) _hdIreplEw _tlIreplEw
         -- self rule
         _lhsOreplEw =
             _replEw
         ( _hdIreplEw) =
             hd_ 
         ( _tlIreplEw) =
             tl_ 
     in  ( _lhsOreplEw))
sem_AEqnDests_Nil :: T_AEqnDests 
sem_AEqnDests_Nil  =
    (let _lhsOreplEw :: AEqnDests 
         -- self rule
         _replEw =
             []
         -- self rule
         _lhsOreplEw =
             _replEw
     in  ( _lhsOreplEw))
-- AEqns -------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         replEw               : SELF 
         replEwEqns           : [AEqn]
   alternatives:
      alternative Cons:
         child hd             : AEqn 
         child tl             : AEqns 
         visit 0:
            local replEw      : _
      alternative Nil:
         visit 0:
            local replEw      : _
-}
-- cata
sem_AEqns :: AEqns  ->
             T_AEqns 
sem_AEqns list  =
    (Prelude.foldr sem_AEqns_Cons sem_AEqns_Nil (Prelude.map sem_AEqn list) )
-- semantic domain
type T_AEqns  = ( AEqns ,([AEqn]))
sem_AEqns_Cons :: T_AEqn  ->
                  T_AEqns  ->
                  T_AEqns 
sem_AEqns_Cons hd_ tl_  =
    (let _lhsOreplEwEqns :: ([AEqn])
         _lhsOreplEw :: AEqns 
         _hdIreplEw :: AEqn 
         _hdIreplEwEqns :: ([AEqn])
         _tlIreplEw :: AEqns 
         _tlIreplEwEqns :: ([AEqn])
         -- use rule "build/ruler2/ARule/ElimWildcAssign.ag"(line 31, column 34)
         _lhsOreplEwEqns =
             _hdIreplEwEqns ++ _tlIreplEwEqns
         -- self rule
         _replEw =
             (:) _hdIreplEw _tlIreplEw
         -- self rule
         _lhsOreplEw =
             _replEw
         ( _hdIreplEw,_hdIreplEwEqns) =
             hd_ 
         ( _tlIreplEw,_tlIreplEwEqns) =
             tl_ 
     in  ( _lhsOreplEw,_lhsOreplEwEqns))
sem_AEqns_Nil :: T_AEqns 
sem_AEqns_Nil  =
    (let _lhsOreplEwEqns :: ([AEqn])
         _lhsOreplEw :: AEqns 
         -- use rule "build/ruler2/ARule/ElimWildcAssign.ag"(line 31, column 34)
         _lhsOreplEwEqns =
             []
         -- self rule
         _replEw =
             []
         -- self rule
         _lhsOreplEw =
             _replEw
     in  ( _lhsOreplEw,_lhsOreplEwEqns))
-- AExpr -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         replEw               : SELF 
   alternatives:
      alternative Expr:
         child expr           : Expr 
         visit 0:
            local replEw      : _
-}
-- cata
sem_AExpr :: AExpr  ->
             T_AExpr 
sem_AExpr (AExpr_Expr _expr )  =
    (sem_AExpr_Expr (sem_Expr _expr ) )
-- semantic domain
type T_AExpr  = ( AExpr )
sem_AExpr_Expr :: T_Expr  ->
                  T_AExpr 
sem_AExpr_Expr expr_  =
    (let _lhsOreplEw :: AExpr 
         _exprIreplEw :: Expr 
         -- self rule
         _replEw =
             AExpr_Expr _exprIreplEw
         -- self rule
         _lhsOreplEw =
             _replEw
         ( _exprIreplEw) =
             expr_ 
     in  ( _lhsOreplEw))
-- AGARuleItf --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         replEw               : ARule 
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
data Syn_AGARuleItf  = Syn_AGARuleItf {replEw_Syn_AGARuleItf :: ARule }
wrap_AGARuleItf :: T_AGARuleItf  ->
                   Inh_AGARuleItf  ->
                   Syn_AGARuleItf 
wrap_AGARuleItf sem (Inh_AGARuleItf )  =
    (let ( _lhsOreplEw) = sem 
     in  (Syn_AGARuleItf _lhsOreplEw ))
sem_AGARuleItf_AGItf :: T_ARule  ->
                        T_AGARuleItf 
sem_AGARuleItf_AGItf rule_  =
    (let _lhsOreplEw :: ARule 
         _ruleIreplEw :: ARule 
         -- copy rule (up)
         _lhsOreplEw =
             _ruleIreplEw
         ( _ruleIreplEw) =
             rule_ 
     in  ( _lhsOreplEw))
-- AGExprItf ---------------------------------------------------
{-
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
type T_AGExprItf  = ( )
sem_AGExprItf_AGItf :: T_Expr  ->
                       T_AGExprItf 
sem_AGExprItf_AGItf expr_  =
    (let _exprIreplEw :: Expr 
         ( _exprIreplEw) =
             expr_ 
     in  ( ))
-- ANm ---------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         replEw               : SELF 
   alternatives:
      alternative Fld:
         child nm             : {Nm}
         visit 0:
            local replEw      : _
      alternative Lhs:
         child nm             : {Nm}
         child props          : {[AtProp]}
         visit 0:
            local replEw      : _
      alternative Loc:
         child nm             : {Nm}
         child props          : {[AtProp]}
         visit 0:
            local replEw      : _
      alternative Node:
         child ndNm           : {Nm}
         child nm             : {Nm}
         visit 0:
            local replEw      : _
      alternative Wild:
         visit 0:
            local replEw      : _
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
    (let _lhsOreplEw :: ANm 
         -- self rule
         _replEw =
             ANm_Fld nm_
         -- self rule
         _lhsOreplEw =
             _replEw
     in  ( _lhsOreplEw))
sem_ANm_Lhs :: Nm ->
               ([AtProp]) ->
               T_ANm 
sem_ANm_Lhs nm_ props_  =
    (let _lhsOreplEw :: ANm 
         -- self rule
         _replEw =
             ANm_Lhs nm_ props_
         -- self rule
         _lhsOreplEw =
             _replEw
     in  ( _lhsOreplEw))
sem_ANm_Loc :: Nm ->
               ([AtProp]) ->
               T_ANm 
sem_ANm_Loc nm_ props_  =
    (let _lhsOreplEw :: ANm 
         -- self rule
         _replEw =
             ANm_Loc nm_ props_
         -- self rule
         _lhsOreplEw =
             _replEw
     in  ( _lhsOreplEw))
sem_ANm_Node :: Nm ->
                Nm ->
                T_ANm 
sem_ANm_Node ndNm_ nm_  =
    (let _lhsOreplEw :: ANm 
         -- self rule
         _replEw =
             ANm_Node ndNm_ nm_
         -- self rule
         _lhsOreplEw =
             _replEw
     in  ( _lhsOreplEw))
sem_ANm_Wild :: T_ANm 
sem_ANm_Wild  =
    (let _lhsOreplEw :: ANm 
         -- self rule
         _replEw =
             ANm_Wild
         -- self rule
         _lhsOreplEw =
             _replEw
     in  ( _lhsOreplEw))
-- ARule -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         replEw               : SELF 
   alternatives:
      alternative Rule:
         child ndNmL          : {[Nm]}
         child rlNm           : {Nm}
         child info           : {[String]}
         child eqns           : AEqns 
         visit 0:
            local replEw      : _
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
    (let _lhsOreplEw :: ARule 
         _eqnsIreplEw :: AEqns 
         _eqnsIreplEwEqns :: ([AEqn])
         -- "build/ruler2/ARule/ElimWildcAssign.ag"(line 40, column 21)
         _lhsOreplEw =
             ARule_Rule ndNmL_ rlNm_ info_ _eqnsIreplEwEqns
         -- self rule
         _replEw =
             ARule_Rule ndNmL_ rlNm_ info_ _eqnsIreplEw
         ( _eqnsIreplEw,_eqnsIreplEwEqns) =
             eqns_ 
     in  ( _lhsOreplEw))
-- ARules ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         replEw               : SELF 
   alternatives:
      alternative Cons:
         child hd             : ARule 
         child tl             : ARules 
         visit 0:
            local replEw      : _
      alternative Nil:
         visit 0:
            local replEw      : _
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
    (let _lhsOreplEw :: ARules 
         _hdIreplEw :: ARule 
         _tlIreplEw :: ARules 
         -- self rule
         _replEw =
             (:) _hdIreplEw _tlIreplEw
         -- self rule
         _lhsOreplEw =
             _replEw
         ( _hdIreplEw) =
             hd_ 
         ( _tlIreplEw) =
             tl_ 
     in  ( _lhsOreplEw))
sem_ARules_Nil :: T_ARules 
sem_ARules_Nil  =
    (let _lhsOreplEw :: ARules 
         -- self rule
         _replEw =
             []
         -- self rule
         _lhsOreplEw =
             _replEw
     in  ( _lhsOreplEw))
-- ECnstr ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         replEw               : SELF 
   alternatives:
      alternative Empty:
         visit 0:
            local replEw      : _
      alternative Ty:
         child nms            : {[Nm]}
         visit 0:
            local replEw      : _
      alternative Var:
         child nm             : {Nm}
         visit 0:
            local replEw      : _
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
    (let _lhsOreplEw :: ECnstr 
         -- self rule
         _replEw =
             ECnstr_Empty
         -- self rule
         _lhsOreplEw =
             _replEw
     in  ( _lhsOreplEw))
sem_ECnstr_Ty :: ([Nm]) ->
                 T_ECnstr 
sem_ECnstr_Ty nms_  =
    (let _lhsOreplEw :: ECnstr 
         -- self rule
         _replEw =
             ECnstr_Ty nms_
         -- self rule
         _lhsOreplEw =
             _replEw
     in  ( _lhsOreplEw))
sem_ECnstr_Var :: Nm ->
                  T_ECnstr 
sem_ECnstr_Var nm_  =
    (let _lhsOreplEw :: ECnstr 
         -- self rule
         _replEw =
             ECnstr_Var nm_
         -- self rule
         _lhsOreplEw =
             _replEw
     in  ( _lhsOreplEw))
-- Expr --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         replEw               : SELF 
   alternatives:
      alternative AVar:
         child anm            : ANm 
         visit 0:
            local replEw      : _
      alternative App:
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local replEw      : _
      alternative AppTop:
         child expr           : Expr 
         visit 0:
            local replEw      : _
      alternative ChildOrder:
         child seqNr          : {Int}
         child expr           : Expr 
         visit 0:
            local replEw      : _
      alternative Cnstr:
         child expr           : Expr 
         child cnstr          : ECnstr 
         visit 0:
            local replEw      : _
      alternative Empty:
         visit 0:
            local replEw      : _
      alternative Expr:
         child expr           : Expr 
         visit 0:
            local replEw      : _
      alternative Int:
         child int            : {String}
         visit 0:
            local replEw      : _
      alternative LF:
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local replEw      : _
      alternative Named:
         child nm             : {Nm}
         child expr           : Expr 
         visit 0:
            local replEw      : _
      alternative Op:
         child nm             : {Nm}
         child nmExpr         : Expr 
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local replEw      : _
      alternative Paren:
         child expr           : Expr 
         visit 0:
            local replEw      : _
      alternative Retain:
         child expr           : Expr 
         visit 0:
            local replEw      : _
      alternative SP:
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local replEw      : _
      alternative Sel:
         child expr           : Expr 
         child selMbExpr      : MbExpr 
         visit 0:
            local replEw      : _
      alternative SelTop:
         child expr           : Expr 
         visit 0:
            local replEw      : _
      alternative StrAsIs:
         child str            : {String}
         visit 0:
            local replEw      : _
      alternative StrText:
         child str            : {String}
         visit 0:
            local replEw      : _
      alternative Undefined:
         visit 0:
            local replEw      : _
      alternative Uniq:
         visit 0:
            local replEw      : _
      alternative Var:
         child nm             : {Nm}
         visit 0:
            local replEw      : _
      alternative Wrap:
         child wrKind         : {WrKind}
         child expr           : Expr 
         visit 0:
            local replEw      : _
      alternative WrapCnstr:
         child cnstr          : ECnstr 
         visit 0:
            local replEw      : _
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
type T_Expr  = ( Expr )
sem_Expr_AVar :: T_ANm  ->
                 T_Expr 
sem_Expr_AVar anm_  =
    (let _lhsOreplEw :: Expr 
         _anmIreplEw :: ANm 
         -- self rule
         _replEw =
             Expr_AVar _anmIreplEw
         -- self rule
         _lhsOreplEw =
             _replEw
         ( _anmIreplEw) =
             anm_ 
     in  ( _lhsOreplEw))
sem_Expr_App :: T_Expr  ->
                T_Expr  ->
                T_Expr 
sem_Expr_App lExpr_ rExpr_  =
    (let _lhsOreplEw :: Expr 
         _lExprIreplEw :: Expr 
         _rExprIreplEw :: Expr 
         -- self rule
         _replEw =
             Expr_App _lExprIreplEw _rExprIreplEw
         -- self rule
         _lhsOreplEw =
             _replEw
         ( _lExprIreplEw) =
             lExpr_ 
         ( _rExprIreplEw) =
             rExpr_ 
     in  ( _lhsOreplEw))
sem_Expr_AppTop :: T_Expr  ->
                   T_Expr 
sem_Expr_AppTop expr_  =
    (let _lhsOreplEw :: Expr 
         _exprIreplEw :: Expr 
         -- self rule
         _replEw =
             Expr_AppTop _exprIreplEw
         -- self rule
         _lhsOreplEw =
             _replEw
         ( _exprIreplEw) =
             expr_ 
     in  ( _lhsOreplEw))
sem_Expr_ChildOrder :: Int ->
                       T_Expr  ->
                       T_Expr 
sem_Expr_ChildOrder seqNr_ expr_  =
    (let _lhsOreplEw :: Expr 
         _exprIreplEw :: Expr 
         -- self rule
         _replEw =
             Expr_ChildOrder seqNr_ _exprIreplEw
         -- self rule
         _lhsOreplEw =
             _replEw
         ( _exprIreplEw) =
             expr_ 
     in  ( _lhsOreplEw))
sem_Expr_Cnstr :: T_Expr  ->
                  T_ECnstr  ->
                  T_Expr 
sem_Expr_Cnstr expr_ cnstr_  =
    (let _lhsOreplEw :: Expr 
         _exprIreplEw :: Expr 
         _cnstrIreplEw :: ECnstr 
         -- self rule
         _replEw =
             Expr_Cnstr _exprIreplEw _cnstrIreplEw
         -- self rule
         _lhsOreplEw =
             _replEw
         ( _exprIreplEw) =
             expr_ 
         ( _cnstrIreplEw) =
             cnstr_ 
     in  ( _lhsOreplEw))
sem_Expr_Empty :: T_Expr 
sem_Expr_Empty  =
    (let _lhsOreplEw :: Expr 
         -- self rule
         _replEw =
             Expr_Empty
         -- self rule
         _lhsOreplEw =
             _replEw
     in  ( _lhsOreplEw))
sem_Expr_Expr :: T_Expr  ->
                 T_Expr 
sem_Expr_Expr expr_  =
    (let _lhsOreplEw :: Expr 
         _exprIreplEw :: Expr 
         -- self rule
         _replEw =
             Expr_Expr _exprIreplEw
         -- self rule
         _lhsOreplEw =
             _replEw
         ( _exprIreplEw) =
             expr_ 
     in  ( _lhsOreplEw))
sem_Expr_Int :: String ->
                T_Expr 
sem_Expr_Int int_  =
    (let _lhsOreplEw :: Expr 
         -- self rule
         _replEw =
             Expr_Int int_
         -- self rule
         _lhsOreplEw =
             _replEw
     in  ( _lhsOreplEw))
sem_Expr_LF :: T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_LF lExpr_ rExpr_  =
    (let _lhsOreplEw :: Expr 
         _lExprIreplEw :: Expr 
         _rExprIreplEw :: Expr 
         -- self rule
         _replEw =
             Expr_LF _lExprIreplEw _rExprIreplEw
         -- self rule
         _lhsOreplEw =
             _replEw
         ( _lExprIreplEw) =
             lExpr_ 
         ( _rExprIreplEw) =
             rExpr_ 
     in  ( _lhsOreplEw))
sem_Expr_Named :: Nm ->
                  T_Expr  ->
                  T_Expr 
sem_Expr_Named nm_ expr_  =
    (let _lhsOreplEw :: Expr 
         _exprIreplEw :: Expr 
         -- self rule
         _replEw =
             Expr_Named nm_ _exprIreplEw
         -- self rule
         _lhsOreplEw =
             _replEw
         ( _exprIreplEw) =
             expr_ 
     in  ( _lhsOreplEw))
sem_Expr_Op :: Nm ->
               T_Expr  ->
               T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_Op nm_ nmExpr_ lExpr_ rExpr_  =
    (let _lhsOreplEw :: Expr 
         _nmExprIreplEw :: Expr 
         _lExprIreplEw :: Expr 
         _rExprIreplEw :: Expr 
         -- self rule
         _replEw =
             Expr_Op nm_ _nmExprIreplEw _lExprIreplEw _rExprIreplEw
         -- self rule
         _lhsOreplEw =
             _replEw
         ( _nmExprIreplEw) =
             nmExpr_ 
         ( _lExprIreplEw) =
             lExpr_ 
         ( _rExprIreplEw) =
             rExpr_ 
     in  ( _lhsOreplEw))
sem_Expr_Paren :: T_Expr  ->
                  T_Expr 
sem_Expr_Paren expr_  =
    (let _lhsOreplEw :: Expr 
         _exprIreplEw :: Expr 
         -- self rule
         _replEw =
             Expr_Paren _exprIreplEw
         -- self rule
         _lhsOreplEw =
             _replEw
         ( _exprIreplEw) =
             expr_ 
     in  ( _lhsOreplEw))
sem_Expr_Retain :: T_Expr  ->
                   T_Expr 
sem_Expr_Retain expr_  =
    (let _lhsOreplEw :: Expr 
         _exprIreplEw :: Expr 
         -- self rule
         _replEw =
             Expr_Retain _exprIreplEw
         -- self rule
         _lhsOreplEw =
             _replEw
         ( _exprIreplEw) =
             expr_ 
     in  ( _lhsOreplEw))
sem_Expr_SP :: T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_SP lExpr_ rExpr_  =
    (let _lhsOreplEw :: Expr 
         _lExprIreplEw :: Expr 
         _rExprIreplEw :: Expr 
         -- self rule
         _replEw =
             Expr_SP _lExprIreplEw _rExprIreplEw
         -- self rule
         _lhsOreplEw =
             _replEw
         ( _lExprIreplEw) =
             lExpr_ 
         ( _rExprIreplEw) =
             rExpr_ 
     in  ( _lhsOreplEw))
sem_Expr_Sel :: T_Expr  ->
                T_MbExpr  ->
                T_Expr 
sem_Expr_Sel expr_ selMbExpr_  =
    (let _lhsOreplEw :: Expr 
         _exprIreplEw :: Expr 
         _selMbExprIreplEw :: MbExpr 
         -- self rule
         _replEw =
             Expr_Sel _exprIreplEw _selMbExprIreplEw
         -- self rule
         _lhsOreplEw =
             _replEw
         ( _exprIreplEw) =
             expr_ 
         ( _selMbExprIreplEw) =
             selMbExpr_ 
     in  ( _lhsOreplEw))
sem_Expr_SelTop :: T_Expr  ->
                   T_Expr 
sem_Expr_SelTop expr_  =
    (let _lhsOreplEw :: Expr 
         _exprIreplEw :: Expr 
         -- self rule
         _replEw =
             Expr_SelTop _exprIreplEw
         -- self rule
         _lhsOreplEw =
             _replEw
         ( _exprIreplEw) =
             expr_ 
     in  ( _lhsOreplEw))
sem_Expr_StrAsIs :: String ->
                    T_Expr 
sem_Expr_StrAsIs str_  =
    (let _lhsOreplEw :: Expr 
         -- self rule
         _replEw =
             Expr_StrAsIs str_
         -- self rule
         _lhsOreplEw =
             _replEw
     in  ( _lhsOreplEw))
sem_Expr_StrText :: String ->
                    T_Expr 
sem_Expr_StrText str_  =
    (let _lhsOreplEw :: Expr 
         -- self rule
         _replEw =
             Expr_StrText str_
         -- self rule
         _lhsOreplEw =
             _replEw
     in  ( _lhsOreplEw))
sem_Expr_Undefined :: T_Expr 
sem_Expr_Undefined  =
    (let _lhsOreplEw :: Expr 
         -- self rule
         _replEw =
             Expr_Undefined
         -- self rule
         _lhsOreplEw =
             _replEw
     in  ( _lhsOreplEw))
sem_Expr_Uniq :: T_Expr 
sem_Expr_Uniq  =
    (let _lhsOreplEw :: Expr 
         -- self rule
         _replEw =
             Expr_Uniq
         -- self rule
         _lhsOreplEw =
             _replEw
     in  ( _lhsOreplEw))
sem_Expr_Var :: Nm ->
                T_Expr 
sem_Expr_Var nm_  =
    (let _lhsOreplEw :: Expr 
         -- self rule
         _replEw =
             Expr_Var nm_
         -- self rule
         _lhsOreplEw =
             _replEw
     in  ( _lhsOreplEw))
sem_Expr_Wrap :: WrKind ->
                 T_Expr  ->
                 T_Expr 
sem_Expr_Wrap wrKind_ expr_  =
    (let _lhsOreplEw :: Expr 
         _exprIreplEw :: Expr 
         -- self rule
         _replEw =
             Expr_Wrap wrKind_ _exprIreplEw
         -- self rule
         _lhsOreplEw =
             _replEw
         ( _exprIreplEw) =
             expr_ 
     in  ( _lhsOreplEw))
sem_Expr_WrapCnstr :: T_ECnstr  ->
                      T_Expr 
sem_Expr_WrapCnstr cnstr_  =
    (let _lhsOreplEw :: Expr 
         _cnstrIreplEw :: ECnstr 
         -- self rule
         _replEw =
             Expr_WrapCnstr _cnstrIreplEw
         -- self rule
         _lhsOreplEw =
             _replEw
         ( _cnstrIreplEw) =
             cnstr_ 
     in  ( _lhsOreplEw))
-- MbExpr ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         replEw               : SELF 
   alternatives:
      alternative Just:
         child just           : Expr 
         visit 0:
            local replEw      : _
      alternative Nothing:
         visit 0:
            local replEw      : _
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
    (let _lhsOreplEw :: MbExpr 
         _justIreplEw :: Expr 
         -- self rule
         _replEw =
             Just _justIreplEw
         -- self rule
         _lhsOreplEw =
             _replEw
         ( _justIreplEw) =
             just_ 
     in  ( _lhsOreplEw))
sem_MbExpr_Nothing :: T_MbExpr 
sem_MbExpr_Nothing  =
    (let _lhsOreplEw :: MbExpr 
         -- self rule
         _replEw =
             Nothing
         -- self rule
         _lhsOreplEw =
             _replEw
     in  ( _lhsOreplEw))
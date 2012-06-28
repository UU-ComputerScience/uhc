

-- UUAGC 0.9.39.1 (build/ruler2/Expr/CoGam.ag)
module Expr.CoGam(module Gam, ChOrdGam, exprCoGam, coGamNmL) where

import Data.List
import qualified Data.Map as Map
import Common
import Expr.Expr
import Gam









exprCoGam :: Expr -> ChOrdGam
exprCoGam e
  = coGam_Syn_AGExprItf r2
  where r1 = sem_AGExprItf (AGExprItf_AGItf e)
        r2 = wrap_AGExprItf r1
                (Inh_AGExprItf)



type ChOrdGam = Gam Nm Int



coGamNmL :: ChOrdGam -> [Nm]
coGamNmL coGam
  = [nmLhs] ++ l ++ [nmLhs]
  where l = map fst . sortBy (\(_,o1) (_,o2) -> compare o1 o2) . gamAssocsShadow $ coGam

-- AGExprItf ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         coGam                : ChOrdGam
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
type T_AGExprItf  = ( ChOrdGam)
data Inh_AGExprItf  = Inh_AGExprItf {}
data Syn_AGExprItf  = Syn_AGExprItf {coGam_Syn_AGExprItf :: ChOrdGam}
wrap_AGExprItf :: T_AGExprItf  ->
                  Inh_AGExprItf  ->
                  Syn_AGExprItf 
wrap_AGExprItf sem (Inh_AGExprItf )  =
    (let ( _lhsOcoGam) = sem 
     in  (Syn_AGExprItf _lhsOcoGam ))
sem_AGExprItf_AGItf :: T_Expr  ->
                       T_AGExprItf 
sem_AGExprItf_AGItf expr_  =
    (let _lhsOcoGam :: ChOrdGam
         _exprIcoGam :: ChOrdGam
         _exprImbChNm :: (Maybe Nm)
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             _exprIcoGam
         ( _exprIcoGam,_exprImbChNm) =
             expr_ 
     in  ( _lhsOcoGam))
-- ANm ---------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         coGam                : ChOrdGam
         mbChNm               : Maybe Nm
   alternatives:
      alternative Fld:
         child nm             : {Nm}
      alternative Lhs:
         child nm             : {Nm}
         child props          : {[AtProp]}
      alternative Loc:
         child nm             : {Nm}
         child props          : {[AtProp]}
      alternative Node:
         child ndNm           : {Nm}
         child nm             : {Nm}
      alternative Wild:
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
type T_ANm  = ( ChOrdGam,(Maybe Nm))
sem_ANm_Fld :: Nm ->
               T_ANm 
sem_ANm_Fld nm_  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         -- "build/ruler2/Expr/CoGam.ag"(line 44, column 21)
         _lhsOmbChNm =
             Just nm_
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             emptyGam
     in  ( _lhsOcoGam,_lhsOmbChNm))
sem_ANm_Lhs :: Nm ->
               ([AtProp]) ->
               T_ANm 
sem_ANm_Lhs nm_ props_  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         -- "build/ruler2/Expr/CoGam.ag"(line 45, column 21)
         _lhsOmbChNm =
             Nothing
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             emptyGam
     in  ( _lhsOcoGam,_lhsOmbChNm))
sem_ANm_Loc :: Nm ->
               ([AtProp]) ->
               T_ANm 
sem_ANm_Loc nm_ props_  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         -- "build/ruler2/Expr/CoGam.ag"(line 45, column 21)
         _lhsOmbChNm =
             Nothing
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             emptyGam
     in  ( _lhsOcoGam,_lhsOmbChNm))
sem_ANm_Node :: Nm ->
                Nm ->
                T_ANm 
sem_ANm_Node ndNm_ nm_  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         -- "build/ruler2/Expr/CoGam.ag"(line 45, column 21)
         _lhsOmbChNm =
             Nothing
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             emptyGam
     in  ( _lhsOcoGam,_lhsOmbChNm))
sem_ANm_Wild :: T_ANm 
sem_ANm_Wild  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         -- "build/ruler2/Expr/CoGam.ag"(line 45, column 21)
         _lhsOmbChNm =
             Nothing
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             emptyGam
     in  ( _lhsOcoGam,_lhsOmbChNm))
-- ECnstr ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         coGam                : ChOrdGam
   alternatives:
      alternative Empty:
      alternative Ty:
         child nms            : {[Nm]}
      alternative Var:
         child nm             : {Nm}
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
type T_ECnstr  = ( ChOrdGam)
sem_ECnstr_Empty :: T_ECnstr 
sem_ECnstr_Empty  =
    (let _lhsOcoGam :: ChOrdGam
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             emptyGam
     in  ( _lhsOcoGam))
sem_ECnstr_Ty :: ([Nm]) ->
                 T_ECnstr 
sem_ECnstr_Ty nms_  =
    (let _lhsOcoGam :: ChOrdGam
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             emptyGam
     in  ( _lhsOcoGam))
sem_ECnstr_Var :: Nm ->
                  T_ECnstr 
sem_ECnstr_Var nm_  =
    (let _lhsOcoGam :: ChOrdGam
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             emptyGam
     in  ( _lhsOcoGam))
-- Expr --------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         coGam                : ChOrdGam
         mbChNm               : Maybe Nm
   alternatives:
      alternative AVar:
         child anm            : ANm 
      alternative App:
         child lExpr          : Expr 
         child rExpr          : Expr 
      alternative AppTop:
         child expr           : Expr 
      alternative ChildOrder:
         child seqNr          : {Int}
         child expr           : Expr 
      alternative Cnstr:
         child expr           : Expr 
         child cnstr          : ECnstr 
      alternative Empty:
      alternative Expr:
         child expr           : Expr 
      alternative Int:
         child int            : {String}
      alternative LF:
         child lExpr          : Expr 
         child rExpr          : Expr 
      alternative Named:
         child nm             : {Nm}
         child expr           : Expr 
      alternative Op:
         child nm             : {Nm}
         child nmExpr         : Expr 
         child lExpr          : Expr 
         child rExpr          : Expr 
      alternative Paren:
         child expr           : Expr 
      alternative Retain:
         child expr           : Expr 
      alternative SP:
         child lExpr          : Expr 
         child rExpr          : Expr 
      alternative Sel:
         child expr           : Expr 
         child selMbExpr      : MbExpr 
      alternative SelTop:
         child expr           : Expr 
      alternative StrAsIs:
         child str            : {String}
      alternative StrText:
         child str            : {String}
      alternative Undefined:
      alternative Uniq:
      alternative Var:
         child nm             : {Nm}
      alternative Wrap:
         child wrKind         : {WrKind}
         child expr           : Expr 
      alternative WrapCnstr:
         child cnstr          : ECnstr 
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
type T_Expr  = ( ChOrdGam,(Maybe Nm))
sem_Expr_AVar :: T_ANm  ->
                 T_Expr 
sem_Expr_AVar anm_  =
    (let _lhsOcoGam :: ChOrdGam
         _lhsOmbChNm :: (Maybe Nm)
         _anmIcoGam :: ChOrdGam
         _anmImbChNm :: (Maybe Nm)
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             _anmIcoGam
         -- copy rule (up)
         _lhsOmbChNm =
             _anmImbChNm
         ( _anmIcoGam,_anmImbChNm) =
             anm_ 
     in  ( _lhsOcoGam,_lhsOmbChNm))
sem_Expr_App :: T_Expr  ->
                T_Expr  ->
                T_Expr 
sem_Expr_App lExpr_ rExpr_  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         _lExprIcoGam :: ChOrdGam
         _lExprImbChNm :: (Maybe Nm)
         _rExprIcoGam :: ChOrdGam
         _rExprImbChNm :: (Maybe Nm)
         -- "build/ruler2/Expr/CoGam.ag"(line 48, column 21)
         _lhsOmbChNm =
             Nothing
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             _lExprIcoGam `gamUnion` _rExprIcoGam
         ( _lExprIcoGam,_lExprImbChNm) =
             lExpr_ 
         ( _rExprIcoGam,_rExprImbChNm) =
             rExpr_ 
     in  ( _lhsOcoGam,_lhsOmbChNm))
sem_Expr_AppTop :: T_Expr  ->
                   T_Expr 
sem_Expr_AppTop expr_  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         _exprIcoGam :: ChOrdGam
         _exprImbChNm :: (Maybe Nm)
         -- "build/ruler2/Expr/CoGam.ag"(line 48, column 21)
         _lhsOmbChNm =
             Nothing
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             _exprIcoGam
         ( _exprIcoGam,_exprImbChNm) =
             expr_ 
     in  ( _lhsOcoGam,_lhsOmbChNm))
sem_Expr_ChildOrder :: Int ->
                       T_Expr  ->
                       T_Expr 
sem_Expr_ChildOrder seqNr_ expr_  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         _exprIcoGam :: ChOrdGam
         _exprImbChNm :: (Maybe Nm)
         -- "build/ruler2/Expr/CoGam.ag"(line 48, column 21)
         _lhsOmbChNm =
             Nothing
         -- "build/ruler2/Expr/CoGam.ag"(line 51, column 21)
         _lhsOcoGam =
             case _exprImbChNm of
                 Just n -> gamSingleton n seqNr_
                 Nothing -> emptyGam
         ( _exprIcoGam,_exprImbChNm) =
             expr_ 
     in  ( _lhsOcoGam,_lhsOmbChNm))
sem_Expr_Cnstr :: T_Expr  ->
                  T_ECnstr  ->
                  T_Expr 
sem_Expr_Cnstr expr_ cnstr_  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         _exprIcoGam :: ChOrdGam
         _exprImbChNm :: (Maybe Nm)
         _cnstrIcoGam :: ChOrdGam
         -- "build/ruler2/Expr/CoGam.ag"(line 48, column 21)
         _lhsOmbChNm =
             Nothing
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             _exprIcoGam `gamUnion` _cnstrIcoGam
         ( _exprIcoGam,_exprImbChNm) =
             expr_ 
         ( _cnstrIcoGam) =
             cnstr_ 
     in  ( _lhsOcoGam,_lhsOmbChNm))
sem_Expr_Empty :: T_Expr 
sem_Expr_Empty  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         -- "build/ruler2/Expr/CoGam.ag"(line 48, column 21)
         _lhsOmbChNm =
             Nothing
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             emptyGam
     in  ( _lhsOcoGam,_lhsOmbChNm))
sem_Expr_Expr :: T_Expr  ->
                 T_Expr 
sem_Expr_Expr expr_  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         _exprIcoGam :: ChOrdGam
         _exprImbChNm :: (Maybe Nm)
         -- "build/ruler2/Expr/CoGam.ag"(line 48, column 21)
         _lhsOmbChNm =
             Nothing
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             _exprIcoGam
         ( _exprIcoGam,_exprImbChNm) =
             expr_ 
     in  ( _lhsOcoGam,_lhsOmbChNm))
sem_Expr_Int :: String ->
                T_Expr 
sem_Expr_Int int_  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         -- "build/ruler2/Expr/CoGam.ag"(line 48, column 21)
         _lhsOmbChNm =
             Nothing
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             emptyGam
     in  ( _lhsOcoGam,_lhsOmbChNm))
sem_Expr_LF :: T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_LF lExpr_ rExpr_  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         _lExprIcoGam :: ChOrdGam
         _lExprImbChNm :: (Maybe Nm)
         _rExprIcoGam :: ChOrdGam
         _rExprImbChNm :: (Maybe Nm)
         -- "build/ruler2/Expr/CoGam.ag"(line 48, column 21)
         _lhsOmbChNm =
             Nothing
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             _lExprIcoGam `gamUnion` _rExprIcoGam
         ( _lExprIcoGam,_lExprImbChNm) =
             lExpr_ 
         ( _rExprIcoGam,_rExprImbChNm) =
             rExpr_ 
     in  ( _lhsOcoGam,_lhsOmbChNm))
sem_Expr_Named :: Nm ->
                  T_Expr  ->
                  T_Expr 
sem_Expr_Named nm_ expr_  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         _exprIcoGam :: ChOrdGam
         _exprImbChNm :: (Maybe Nm)
         -- "build/ruler2/Expr/CoGam.ag"(line 48, column 21)
         _lhsOmbChNm =
             Nothing
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             _exprIcoGam
         ( _exprIcoGam,_exprImbChNm) =
             expr_ 
     in  ( _lhsOcoGam,_lhsOmbChNm))
sem_Expr_Op :: Nm ->
               T_Expr  ->
               T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_Op nm_ nmExpr_ lExpr_ rExpr_  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         _nmExprIcoGam :: ChOrdGam
         _nmExprImbChNm :: (Maybe Nm)
         _lExprIcoGam :: ChOrdGam
         _lExprImbChNm :: (Maybe Nm)
         _rExprIcoGam :: ChOrdGam
         _rExprImbChNm :: (Maybe Nm)
         -- "build/ruler2/Expr/CoGam.ag"(line 48, column 21)
         _lhsOmbChNm =
             Nothing
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             _nmExprIcoGam `gamUnion` _lExprIcoGam `gamUnion` _rExprIcoGam
         ( _nmExprIcoGam,_nmExprImbChNm) =
             nmExpr_ 
         ( _lExprIcoGam,_lExprImbChNm) =
             lExpr_ 
         ( _rExprIcoGam,_rExprImbChNm) =
             rExpr_ 
     in  ( _lhsOcoGam,_lhsOmbChNm))
sem_Expr_Paren :: T_Expr  ->
                  T_Expr 
sem_Expr_Paren expr_  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         _exprIcoGam :: ChOrdGam
         _exprImbChNm :: (Maybe Nm)
         -- "build/ruler2/Expr/CoGam.ag"(line 48, column 21)
         _lhsOmbChNm =
             Nothing
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             _exprIcoGam
         ( _exprIcoGam,_exprImbChNm) =
             expr_ 
     in  ( _lhsOcoGam,_lhsOmbChNm))
sem_Expr_Retain :: T_Expr  ->
                   T_Expr 
sem_Expr_Retain expr_  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         _exprIcoGam :: ChOrdGam
         _exprImbChNm :: (Maybe Nm)
         -- "build/ruler2/Expr/CoGam.ag"(line 48, column 21)
         _lhsOmbChNm =
             Nothing
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             _exprIcoGam
         ( _exprIcoGam,_exprImbChNm) =
             expr_ 
     in  ( _lhsOcoGam,_lhsOmbChNm))
sem_Expr_SP :: T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_SP lExpr_ rExpr_  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         _lExprIcoGam :: ChOrdGam
         _lExprImbChNm :: (Maybe Nm)
         _rExprIcoGam :: ChOrdGam
         _rExprImbChNm :: (Maybe Nm)
         -- "build/ruler2/Expr/CoGam.ag"(line 48, column 21)
         _lhsOmbChNm =
             Nothing
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             _lExprIcoGam `gamUnion` _rExprIcoGam
         ( _lExprIcoGam,_lExprImbChNm) =
             lExpr_ 
         ( _rExprIcoGam,_rExprImbChNm) =
             rExpr_ 
     in  ( _lhsOcoGam,_lhsOmbChNm))
sem_Expr_Sel :: T_Expr  ->
                T_MbExpr  ->
                T_Expr 
sem_Expr_Sel expr_ selMbExpr_  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         _exprIcoGam :: ChOrdGam
         _exprImbChNm :: (Maybe Nm)
         _selMbExprIcoGam :: ChOrdGam
         -- "build/ruler2/Expr/CoGam.ag"(line 48, column 21)
         _lhsOmbChNm =
             Nothing
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             _exprIcoGam `gamUnion` _selMbExprIcoGam
         ( _exprIcoGam,_exprImbChNm) =
             expr_ 
         ( _selMbExprIcoGam) =
             selMbExpr_ 
     in  ( _lhsOcoGam,_lhsOmbChNm))
sem_Expr_SelTop :: T_Expr  ->
                   T_Expr 
sem_Expr_SelTop expr_  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         _exprIcoGam :: ChOrdGam
         _exprImbChNm :: (Maybe Nm)
         -- "build/ruler2/Expr/CoGam.ag"(line 48, column 21)
         _lhsOmbChNm =
             Nothing
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             _exprIcoGam
         ( _exprIcoGam,_exprImbChNm) =
             expr_ 
     in  ( _lhsOcoGam,_lhsOmbChNm))
sem_Expr_StrAsIs :: String ->
                    T_Expr 
sem_Expr_StrAsIs str_  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         -- "build/ruler2/Expr/CoGam.ag"(line 48, column 21)
         _lhsOmbChNm =
             Nothing
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             emptyGam
     in  ( _lhsOcoGam,_lhsOmbChNm))
sem_Expr_StrText :: String ->
                    T_Expr 
sem_Expr_StrText str_  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         -- "build/ruler2/Expr/CoGam.ag"(line 48, column 21)
         _lhsOmbChNm =
             Nothing
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             emptyGam
     in  ( _lhsOcoGam,_lhsOmbChNm))
sem_Expr_Undefined :: T_Expr 
sem_Expr_Undefined  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         -- "build/ruler2/Expr/CoGam.ag"(line 48, column 21)
         _lhsOmbChNm =
             Nothing
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             emptyGam
     in  ( _lhsOcoGam,_lhsOmbChNm))
sem_Expr_Uniq :: T_Expr 
sem_Expr_Uniq  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         -- "build/ruler2/Expr/CoGam.ag"(line 48, column 21)
         _lhsOmbChNm =
             Nothing
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             emptyGam
     in  ( _lhsOcoGam,_lhsOmbChNm))
sem_Expr_Var :: Nm ->
                T_Expr 
sem_Expr_Var nm_  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         -- "build/ruler2/Expr/CoGam.ag"(line 48, column 21)
         _lhsOmbChNm =
             Nothing
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             emptyGam
     in  ( _lhsOcoGam,_lhsOmbChNm))
sem_Expr_Wrap :: WrKind ->
                 T_Expr  ->
                 T_Expr 
sem_Expr_Wrap wrKind_ expr_  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         _exprIcoGam :: ChOrdGam
         _exprImbChNm :: (Maybe Nm)
         -- "build/ruler2/Expr/CoGam.ag"(line 48, column 21)
         _lhsOmbChNm =
             Nothing
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             _exprIcoGam
         ( _exprIcoGam,_exprImbChNm) =
             expr_ 
     in  ( _lhsOcoGam,_lhsOmbChNm))
sem_Expr_WrapCnstr :: T_ECnstr  ->
                      T_Expr 
sem_Expr_WrapCnstr cnstr_  =
    (let _lhsOmbChNm :: (Maybe Nm)
         _lhsOcoGam :: ChOrdGam
         _cnstrIcoGam :: ChOrdGam
         -- "build/ruler2/Expr/CoGam.ag"(line 48, column 21)
         _lhsOmbChNm =
             Nothing
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             _cnstrIcoGam
         ( _cnstrIcoGam) =
             cnstr_ 
     in  ( _lhsOcoGam,_lhsOmbChNm))
-- MbExpr ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         coGam                : ChOrdGam
   alternatives:
      alternative Just:
         child just           : Expr 
      alternative Nothing:
-}
-- cata
sem_MbExpr :: MbExpr  ->
              T_MbExpr 
sem_MbExpr (Prelude.Just x )  =
    (sem_MbExpr_Just (sem_Expr x ) )
sem_MbExpr Prelude.Nothing  =
    sem_MbExpr_Nothing
-- semantic domain
type T_MbExpr  = ( ChOrdGam)
sem_MbExpr_Just :: T_Expr  ->
                   T_MbExpr 
sem_MbExpr_Just just_  =
    (let _lhsOcoGam :: ChOrdGam
         _justIcoGam :: ChOrdGam
         _justImbChNm :: (Maybe Nm)
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             _justIcoGam
         ( _justIcoGam,_justImbChNm) =
             just_ 
     in  ( _lhsOcoGam))
sem_MbExpr_Nothing :: T_MbExpr 
sem_MbExpr_Nothing  =
    (let _lhsOcoGam :: ChOrdGam
         -- use rule "build/ruler2/Expr/CoGam.ag"(line 40, column 40)
         _lhsOcoGam =
             emptyGam
     in  ( _lhsOcoGam))
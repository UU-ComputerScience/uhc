

-- UUAGC 0.9.39.1 (build/ruler2/Expr/LaTeX.ag)
module Expr.LaTeX(exprFmtTeX) where

import qualified Data.Map as Map
import EH.Util.Utils
import EH.Util.Pretty
import EH.Util.PrettyUtils
import Common
import Opts
import LaTeXFmtUtils
import Expr.Expr
import ARule.RwSubst (fmNmFmtCmd)
import FmGam
import WrKindGam











exprFmtTeX :: Opts -> FmGam Expr -> Expr -> PP_Doc
exprFmtTeX o g e
  = ppLaTeX_Syn_AGExprItf r2
  where r1 = sem_AGExprItf (AGExprItf_AGItf e)
        r2 = wrap_AGExprItf r1
                (Inh_AGExprItf { opts_Inh_AGExprItf = o, fmGam_Inh_AGExprItf = g })


data ExprKind = EKEmp | EKNm Nm | EKOther deriving Eq

-- AGExprItf ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
      synthesized attribute:
         ppLaTeX              : PP_Doc
   alternatives:
      alternative AGItf:
         child expr           : Expr 
         visit 0:
            local needToParen : _
-}
-- cata
sem_AGExprItf :: AGExprItf  ->
                 T_AGExprItf 
sem_AGExprItf (AGExprItf_AGItf _expr )  =
    (sem_AGExprItf_AGItf (sem_Expr _expr ) )
-- semantic domain
type T_AGExprItf  = (FmGam Expr) ->
                    Opts ->
                    ( PP_Doc)
data Inh_AGExprItf  = Inh_AGExprItf {fmGam_Inh_AGExprItf :: (FmGam Expr),opts_Inh_AGExprItf :: Opts}
data Syn_AGExprItf  = Syn_AGExprItf {ppLaTeX_Syn_AGExprItf :: PP_Doc}
wrap_AGExprItf :: T_AGExprItf  ->
                  Inh_AGExprItf  ->
                  Syn_AGExprItf 
wrap_AGExprItf sem (Inh_AGExprItf _lhsIfmGam _lhsIopts )  =
    (let ( _lhsOppLaTeX) = sem _lhsIfmGam _lhsIopts 
     in  (Syn_AGExprItf _lhsOppLaTeX ))
sem_AGExprItf_AGItf :: T_Expr  ->
                       T_AGExprItf 
sem_AGExprItf_AGItf expr_  =
    (\ _lhsIfmGam
       _lhsIopts ->
         (let _lhsOppLaTeX :: PP_Doc
              _exprOfmGam :: (FmGam Expr)
              _exprOneedToParen :: Bool
              _exprOopts :: Opts
              _exprIexprKind :: ExprKind
              _exprIppLaTeX :: PP_Doc
              _exprIselL :: ([Maybe (Nm,PP_Doc)])
              _exprItxt :: Nm
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 68, column 21)
              _needToParen =
                  True
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 6, column 35)
              _lhsOppLaTeX =
                  _exprIppLaTeX
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (from local)
              _exprOneedToParen =
                  _needToParen
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              ( _exprIexprKind,_exprIppLaTeX,_exprIselL,_exprItxt) =
                  expr_ _exprOfmGam _exprOneedToParen _exprOopts 
          in  ( _lhsOppLaTeX)))
-- ANm ---------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         opts                 : Opts
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
type T_ANm  = Opts ->
              ( )
sem_ANm_Fld :: Nm ->
               T_ANm 
sem_ANm_Fld nm_  =
    (\ _lhsIopts ->
         (let 
          in  ( )))
sem_ANm_Lhs :: Nm ->
               ([AtProp]) ->
               T_ANm 
sem_ANm_Lhs nm_ props_  =
    (\ _lhsIopts ->
         (let 
          in  ( )))
sem_ANm_Loc :: Nm ->
               ([AtProp]) ->
               T_ANm 
sem_ANm_Loc nm_ props_  =
    (\ _lhsIopts ->
         (let 
          in  ( )))
sem_ANm_Node :: Nm ->
                Nm ->
                T_ANm 
sem_ANm_Node ndNm_ nm_  =
    (\ _lhsIopts ->
         (let 
          in  ( )))
sem_ANm_Wild :: T_ANm 
sem_ANm_Wild  =
    (\ _lhsIopts ->
         (let 
          in  ( )))
-- ECnstr ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
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
type T_ECnstr  = (FmGam Expr) ->
                 Opts ->
                 ( )
sem_ECnstr_Empty :: T_ECnstr 
sem_ECnstr_Empty  =
    (\ _lhsIfmGam
       _lhsIopts ->
         (let 
          in  ( )))
sem_ECnstr_Ty :: ([Nm]) ->
                 T_ECnstr 
sem_ECnstr_Ty nms_  =
    (\ _lhsIfmGam
       _lhsIopts ->
         (let 
          in  ( )))
sem_ECnstr_Var :: Nm ->
                  T_ECnstr 
sem_ECnstr_Var nm_  =
    (\ _lhsIfmGam
       _lhsIopts ->
         (let 
          in  ( )))
-- Expr --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         needToParen          : Bool
         opts                 : Opts
      synthesized attributes:
         exprKind             : ExprKind
         ppLaTeX              : PP_Doc
         selL                 : [Maybe (Nm,PP_Doc)]
         txt                  : Nm
   alternatives:
      alternative AVar:
         child anm            : ANm 
      alternative App:
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local ppLaTeX     : _
            local needToParen : _
      alternative AppTop:
         child expr           : Expr 
      alternative ChildOrder:
         child seqNr          : {Int}
         child expr           : Expr 
         visit 0:
            local ppLaTeX     : _
      alternative Cnstr:
         child expr           : Expr 
         child cnstr          : ECnstr 
         visit 0:
            local ppLaTeX     : _
      alternative Empty:
         visit 0:
            local ppLaTeX     : _
      alternative Expr:
         child expr           : Expr 
      alternative Int:
         child int            : {String}
         visit 0:
            local ppLaTeX     : _
      alternative LF:
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local ppLaTeX     : _
      alternative Named:
         child nm             : {Nm}
         child expr           : Expr 
      alternative Op:
         child nm             : {Nm}
         child nmExpr         : Expr 
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local ppLaTeX     : _
            local needToParen : _
      alternative Paren:
         child expr           : Expr 
         visit 0:
            local ppLaTeX     : _
            local needToParen : _
      alternative Retain:
         child expr           : Expr 
      alternative SP:
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local ppLaTeX     : _
            local needToParen : _
      alternative Sel:
         child expr           : Expr 
         child selMbExpr      : MbExpr 
      alternative SelTop:
         child expr           : Expr 
         visit 0:
            local ppLaTeXSel  : _
            local ppLaTeX     : _
            local needToParen : _
      alternative StrAsIs:
         child str            : {String}
         visit 0:
            local ppLaTeX     : _
      alternative StrText:
         child str            : {String}
         visit 0:
            local ppLaTeX     : _
      alternative Undefined:
         visit 0:
            local ppLaTeX     : _
      alternative Uniq:
      alternative Var:
         child nm             : {Nm}
         visit 0:
            local ppLaTeX     : _
      alternative Wrap:
         child wrKind         : {WrKind}
         child expr           : Expr 
         visit 0:
            local ppLaTeX     : _
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
type T_Expr  = (FmGam Expr) ->
               Bool ->
               Opts ->
               ( ExprKind,PP_Doc,([Maybe (Nm,PP_Doc)]),Nm)
sem_Expr_AVar :: T_ANm  ->
                 T_Expr 
sem_Expr_AVar anm_  =
    (\ _lhsIfmGam
       _lhsIneedToParen
       _lhsIopts ->
         (let _lhsOexprKind :: ExprKind
              _lhsOselL :: ([Maybe (Nm,PP_Doc)])
              _lhsOppLaTeX :: PP_Doc
              _lhsOtxt :: Nm
              _anmOopts :: Opts
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 83, column 21)
              _lhsOexprKind =
                  EKOther
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 93, column 21)
              _lhsOselL =
                  []
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 6, column 35)
              _lhsOppLaTeX =
                  empty
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 99, column 21)
              _lhsOtxt =
                  nmNone
              -- copy rule (down)
              _anmOopts =
                  _lhsIopts
          in  ( _lhsOexprKind,_lhsOppLaTeX,_lhsOselL,_lhsOtxt)))
sem_Expr_App :: T_Expr  ->
                T_Expr  ->
                T_Expr 
sem_Expr_App lExpr_ rExpr_  =
    (\ _lhsIfmGam
       _lhsIneedToParen
       _lhsIopts ->
         (let _lhsOexprKind :: ExprKind
              _lhsOselL :: ([Maybe (Nm,PP_Doc)])
              _lhsOtxt :: Nm
              _lhsOppLaTeX :: PP_Doc
              _lExprOfmGam :: (FmGam Expr)
              _lExprOneedToParen :: Bool
              _lExprOopts :: Opts
              _rExprOfmGam :: (FmGam Expr)
              _rExprOneedToParen :: Bool
              _rExprOopts :: Opts
              _lExprIexprKind :: ExprKind
              _lExprIppLaTeX :: PP_Doc
              _lExprIselL :: ([Maybe (Nm,PP_Doc)])
              _lExprItxt :: Nm
              _rExprIexprKind :: ExprKind
              _rExprIppLaTeX :: PP_Doc
              _rExprIselL :: ([Maybe (Nm,PP_Doc)])
              _rExprItxt :: Nm
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 12, column 21)
              _ppLaTeX =
                  _lExprIppLaTeX >#< _rExprIppLaTeX
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 65, column 21)
              _needToParen =
                  True
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 83, column 21)
              _lhsOexprKind =
                  EKOther
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 93, column 21)
              _lhsOselL =
                  []
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 104, column 21)
              _lhsOtxt =
                  nmNone
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 6, column 35)
              _lhsOppLaTeX =
                  _ppLaTeX
              -- copy rule (down)
              _lExprOfmGam =
                  _lhsIfmGam
              -- copy rule (from local)
              _lExprOneedToParen =
                  _needToParen
              -- copy rule (down)
              _lExprOopts =
                  _lhsIopts
              -- copy rule (down)
              _rExprOfmGam =
                  _lhsIfmGam
              -- copy rule (from local)
              _rExprOneedToParen =
                  _needToParen
              -- copy rule (down)
              _rExprOopts =
                  _lhsIopts
              ( _lExprIexprKind,_lExprIppLaTeX,_lExprIselL,_lExprItxt) =
                  lExpr_ _lExprOfmGam _lExprOneedToParen _lExprOopts 
              ( _rExprIexprKind,_rExprIppLaTeX,_rExprIselL,_rExprItxt) =
                  rExpr_ _rExprOfmGam _rExprOneedToParen _rExprOopts 
          in  ( _lhsOexprKind,_lhsOppLaTeX,_lhsOselL,_lhsOtxt)))
sem_Expr_AppTop :: T_Expr  ->
                   T_Expr 
sem_Expr_AppTop expr_  =
    (\ _lhsIfmGam
       _lhsIneedToParen
       _lhsIopts ->
         (let _lhsOselL :: ([Maybe (Nm,PP_Doc)])
              _lhsOppLaTeX :: PP_Doc
              _lhsOtxt :: Nm
              _lhsOexprKind :: ExprKind
              _exprOfmGam :: (FmGam Expr)
              _exprOneedToParen :: Bool
              _exprOopts :: Opts
              _exprIexprKind :: ExprKind
              _exprIppLaTeX :: PP_Doc
              _exprIselL :: ([Maybe (Nm,PP_Doc)])
              _exprItxt :: Nm
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 93, column 21)
              _lhsOselL =
                  []
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 6, column 35)
              _lhsOppLaTeX =
                  _exprIppLaTeX
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 99, column 21)
              _lhsOtxt =
                  _exprItxt
              -- copy rule (up)
              _lhsOexprKind =
                  _exprIexprKind
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOneedToParen =
                  _lhsIneedToParen
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              ( _exprIexprKind,_exprIppLaTeX,_exprIselL,_exprItxt) =
                  expr_ _exprOfmGam _exprOneedToParen _exprOopts 
          in  ( _lhsOexprKind,_lhsOppLaTeX,_lhsOselL,_lhsOtxt)))
sem_Expr_ChildOrder :: Int ->
                       T_Expr  ->
                       T_Expr 
sem_Expr_ChildOrder seqNr_ expr_  =
    (\ _lhsIfmGam
       _lhsIneedToParen
       _lhsIopts ->
         (let _lhsOexprKind :: ExprKind
              _lhsOselL :: ([Maybe (Nm,PP_Doc)])
              _lhsOppLaTeX :: PP_Doc
              _lhsOtxt :: Nm
              _exprOfmGam :: (FmGam Expr)
              _exprOneedToParen :: Bool
              _exprOopts :: Opts
              _exprIexprKind :: ExprKind
              _exprIppLaTeX :: PP_Doc
              _exprIselL :: ([Maybe (Nm,PP_Doc)])
              _exprItxt :: Nm
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 35, column 21)
              _ppLaTeX =
                  _exprIppLaTeX
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 83, column 21)
              _lhsOexprKind =
                  EKOther
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 93, column 21)
              _lhsOselL =
                  []
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 6, column 35)
              _lhsOppLaTeX =
                  _ppLaTeX
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 99, column 21)
              _lhsOtxt =
                  _exprItxt
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOneedToParen =
                  _lhsIneedToParen
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              ( _exprIexprKind,_exprIppLaTeX,_exprIselL,_exprItxt) =
                  expr_ _exprOfmGam _exprOneedToParen _exprOopts 
          in  ( _lhsOexprKind,_lhsOppLaTeX,_lhsOselL,_lhsOtxt)))
sem_Expr_Cnstr :: T_Expr  ->
                  T_ECnstr  ->
                  T_Expr 
sem_Expr_Cnstr expr_ cnstr_  =
    (\ _lhsIfmGam
       _lhsIneedToParen
       _lhsIopts ->
         (let _lhsOexprKind :: ExprKind
              _lhsOselL :: ([Maybe (Nm,PP_Doc)])
              _lhsOppLaTeX :: PP_Doc
              _lhsOtxt :: Nm
              _exprOfmGam :: (FmGam Expr)
              _exprOneedToParen :: Bool
              _exprOopts :: Opts
              _cnstrOfmGam :: (FmGam Expr)
              _cnstrOopts :: Opts
              _exprIexprKind :: ExprKind
              _exprIppLaTeX :: PP_Doc
              _exprIselL :: ([Maybe (Nm,PP_Doc)])
              _exprItxt :: Nm
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 23, column 21)
              _ppLaTeX =
                  _exprIppLaTeX
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 83, column 21)
              _lhsOexprKind =
                  EKOther
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 93, column 21)
              _lhsOselL =
                  []
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 6, column 35)
              _lhsOppLaTeX =
                  _ppLaTeX
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 99, column 21)
              _lhsOtxt =
                  _exprItxt
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOneedToParen =
                  _lhsIneedToParen
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _cnstrOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _cnstrOopts =
                  _lhsIopts
              ( _exprIexprKind,_exprIppLaTeX,_exprIselL,_exprItxt) =
                  expr_ _exprOfmGam _exprOneedToParen _exprOopts 
          in  ( _lhsOexprKind,_lhsOppLaTeX,_lhsOselL,_lhsOtxt)))
sem_Expr_Empty :: T_Expr 
sem_Expr_Empty  =
    (\ _lhsIfmGam
       _lhsIneedToParen
       _lhsIopts ->
         (let _lhsOexprKind :: ExprKind
              _lhsOselL :: ([Maybe (Nm,PP_Doc)])
              _lhsOppLaTeX :: PP_Doc
              _lhsOtxt :: Nm
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 37, column 21)
              _ppLaTeX =
                  empty
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 79, column 21)
              _lhsOexprKind =
                  EKEmp
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 93, column 21)
              _lhsOselL =
                  []
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 6, column 35)
              _lhsOppLaTeX =
                  _ppLaTeX
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 99, column 21)
              _lhsOtxt =
                  nmNone
          in  ( _lhsOexprKind,_lhsOppLaTeX,_lhsOselL,_lhsOtxt)))
sem_Expr_Expr :: T_Expr  ->
                 T_Expr 
sem_Expr_Expr expr_  =
    (\ _lhsIfmGam
       _lhsIneedToParen
       _lhsIopts ->
         (let _lhsOppLaTeX :: PP_Doc
              _lhsOexprKind :: ExprKind
              _lhsOselL :: ([Maybe (Nm,PP_Doc)])
              _lhsOtxt :: Nm
              _exprOfmGam :: (FmGam Expr)
              _exprOneedToParen :: Bool
              _exprOopts :: Opts
              _exprIexprKind :: ExprKind
              _exprIppLaTeX :: PP_Doc
              _exprIselL :: ([Maybe (Nm,PP_Doc)])
              _exprItxt :: Nm
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 36, column 21)
              _lhsOppLaTeX =
                  ensureTeXMath . switchLaTeXLhs' $ _exprIppLaTeX
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 83, column 21)
              _lhsOexprKind =
                  EKOther
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 93, column 21)
              _lhsOselL =
                  []
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 99, column 21)
              _lhsOtxt =
                  _exprItxt
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOneedToParen =
                  _lhsIneedToParen
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              ( _exprIexprKind,_exprIppLaTeX,_exprIselL,_exprItxt) =
                  expr_ _exprOfmGam _exprOneedToParen _exprOopts 
          in  ( _lhsOexprKind,_lhsOppLaTeX,_lhsOselL,_lhsOtxt)))
sem_Expr_Int :: String ->
                T_Expr 
sem_Expr_Int int_  =
    (\ _lhsIfmGam
       _lhsIneedToParen
       _lhsIopts ->
         (let _lhsOexprKind :: ExprKind
              _lhsOselL :: ([Maybe (Nm,PP_Doc)])
              _lhsOppLaTeX :: PP_Doc
              _lhsOtxt :: Nm
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 9, column 21)
              _ppLaTeX =
                  pp int_
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 83, column 21)
              _lhsOexprKind =
                  EKOther
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 93, column 21)
              _lhsOselL =
                  []
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 6, column 35)
              _lhsOppLaTeX =
                  _ppLaTeX
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 99, column 21)
              _lhsOtxt =
                  nmNone
          in  ( _lhsOexprKind,_lhsOppLaTeX,_lhsOselL,_lhsOtxt)))
sem_Expr_LF :: T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_LF lExpr_ rExpr_  =
    (\ _lhsIfmGam
       _lhsIneedToParen
       _lhsIopts ->
         (let _lhsOexprKind :: ExprKind
              _lhsOselL :: ([Maybe (Nm,PP_Doc)])
              _lhsOppLaTeX :: PP_Doc
              _lhsOtxt :: Nm
              _lExprOfmGam :: (FmGam Expr)
              _lExprOneedToParen :: Bool
              _lExprOopts :: Opts
              _rExprOfmGam :: (FmGam Expr)
              _rExprOneedToParen :: Bool
              _rExprOopts :: Opts
              _lExprIexprKind :: ExprKind
              _lExprIppLaTeX :: PP_Doc
              _lExprIselL :: ([Maybe (Nm,PP_Doc)])
              _lExprItxt :: Nm
              _rExprIexprKind :: ExprKind
              _rExprIppLaTeX :: PP_Doc
              _rExprIselL :: ([Maybe (Nm,PP_Doc)])
              _rExprItxt :: Nm
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 14, column 21)
              _ppLaTeX =
                  _lExprIppLaTeX >#< switchLaTeXLhs (mkTexCmdUse "quad" empty) >#< _rExprIppLaTeX
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 83, column 21)
              _lhsOexprKind =
                  EKOther
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 93, column 21)
              _lhsOselL =
                  []
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 6, column 35)
              _lhsOppLaTeX =
                  _ppLaTeX
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 99, column 21)
              _lhsOtxt =
                  _lExprItxt `const` _rExprItxt
              -- copy rule (down)
              _lExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _lExprOneedToParen =
                  _lhsIneedToParen
              -- copy rule (down)
              _lExprOopts =
                  _lhsIopts
              -- copy rule (down)
              _rExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _rExprOneedToParen =
                  _lhsIneedToParen
              -- copy rule (down)
              _rExprOopts =
                  _lhsIopts
              ( _lExprIexprKind,_lExprIppLaTeX,_lExprIselL,_lExprItxt) =
                  lExpr_ _lExprOfmGam _lExprOneedToParen _lExprOopts 
              ( _rExprIexprKind,_rExprIppLaTeX,_rExprIselL,_rExprItxt) =
                  rExpr_ _rExprOfmGam _rExprOneedToParen _rExprOopts 
          in  ( _lhsOexprKind,_lhsOppLaTeX,_lhsOselL,_lhsOtxt)))
sem_Expr_Named :: Nm ->
                  T_Expr  ->
                  T_Expr 
sem_Expr_Named nm_ expr_  =
    (\ _lhsIfmGam
       _lhsIneedToParen
       _lhsIopts ->
         (let _lhsOexprKind :: ExprKind
              _lhsOselL :: ([Maybe (Nm,PP_Doc)])
              _lhsOppLaTeX :: PP_Doc
              _lhsOtxt :: Nm
              _exprOfmGam :: (FmGam Expr)
              _exprOneedToParen :: Bool
              _exprOopts :: Opts
              _exprIexprKind :: ExprKind
              _exprIppLaTeX :: PP_Doc
              _exprIselL :: ([Maybe (Nm,PP_Doc)])
              _exprItxt :: Nm
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 83, column 21)
              _lhsOexprKind =
                  EKOther
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 93, column 21)
              _lhsOselL =
                  []
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 6, column 35)
              _lhsOppLaTeX =
                  _exprIppLaTeX
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 99, column 21)
              _lhsOtxt =
                  _exprItxt
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOneedToParen =
                  _lhsIneedToParen
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              ( _exprIexprKind,_exprIppLaTeX,_exprIselL,_exprItxt) =
                  expr_ _exprOfmGam _exprOneedToParen _exprOopts 
          in  ( _lhsOexprKind,_lhsOppLaTeX,_lhsOselL,_lhsOtxt)))
sem_Expr_Op :: Nm ->
               T_Expr  ->
               T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_Op nm_ nmExpr_ lExpr_ rExpr_  =
    (\ _lhsIfmGam
       _lhsIneedToParen
       _lhsIopts ->
         (let _lhsOexprKind :: ExprKind
              _lhsOselL :: ([Maybe (Nm,PP_Doc)])
              _lhsOtxt :: Nm
              _lhsOppLaTeX :: PP_Doc
              _nmExprOfmGam :: (FmGam Expr)
              _nmExprOneedToParen :: Bool
              _nmExprOopts :: Opts
              _lExprOfmGam :: (FmGam Expr)
              _lExprOneedToParen :: Bool
              _lExprOopts :: Opts
              _rExprOfmGam :: (FmGam Expr)
              _rExprOneedToParen :: Bool
              _rExprOopts :: Opts
              _nmExprIexprKind :: ExprKind
              _nmExprIppLaTeX :: PP_Doc
              _nmExprIselL :: ([Maybe (Nm,PP_Doc)])
              _nmExprItxt :: Nm
              _lExprIexprKind :: ExprKind
              _lExprIppLaTeX :: PP_Doc
              _lExprIselL :: ([Maybe (Nm,PP_Doc)])
              _lExprItxt :: Nm
              _rExprIexprKind :: ExprKind
              _rExprIppLaTeX :: PP_Doc
              _rExprIselL :: ([Maybe (Nm,PP_Doc)])
              _rExprItxt :: Nm
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 13, column 21)
              _ppLaTeX =
                  _lExprIppLaTeX >#< _nmExprIppLaTeX >#< _rExprIppLaTeX
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 65, column 21)
              _needToParen =
                  True
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 83, column 21)
              _lhsOexprKind =
                  EKOther
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 93, column 21)
              _lhsOselL =
                  []
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 104, column 21)
              _lhsOtxt =
                  nmNone
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 6, column 35)
              _lhsOppLaTeX =
                  _ppLaTeX
              -- copy rule (down)
              _nmExprOfmGam =
                  _lhsIfmGam
              -- copy rule (from local)
              _nmExprOneedToParen =
                  _needToParen
              -- copy rule (down)
              _nmExprOopts =
                  _lhsIopts
              -- copy rule (down)
              _lExprOfmGam =
                  _lhsIfmGam
              -- copy rule (from local)
              _lExprOneedToParen =
                  _needToParen
              -- copy rule (down)
              _lExprOopts =
                  _lhsIopts
              -- copy rule (down)
              _rExprOfmGam =
                  _lhsIfmGam
              -- copy rule (from local)
              _rExprOneedToParen =
                  _needToParen
              -- copy rule (down)
              _rExprOopts =
                  _lhsIopts
              ( _nmExprIexprKind,_nmExprIppLaTeX,_nmExprIselL,_nmExprItxt) =
                  nmExpr_ _nmExprOfmGam _nmExprOneedToParen _nmExprOopts 
              ( _lExprIexprKind,_lExprIppLaTeX,_lExprIselL,_lExprItxt) =
                  lExpr_ _lExprOfmGam _lExprOneedToParen _lExprOopts 
              ( _rExprIexprKind,_rExprIppLaTeX,_rExprIselL,_rExprItxt) =
                  rExpr_ _rExprOfmGam _rExprOneedToParen _rExprOopts 
          in  ( _lhsOexprKind,_lhsOppLaTeX,_lhsOselL,_lhsOtxt)))
sem_Expr_Paren :: T_Expr  ->
                  T_Expr 
sem_Expr_Paren expr_  =
    (\ _lhsIfmGam
       _lhsIneedToParen
       _lhsIopts ->
         (let _lhsOselL :: ([Maybe (Nm,PP_Doc)])
              _lhsOppLaTeX :: PP_Doc
              _lhsOtxt :: Nm
              _lhsOexprKind :: ExprKind
              _exprOfmGam :: (FmGam Expr)
              _exprOneedToParen :: Bool
              _exprOopts :: Opts
              _exprIexprKind :: ExprKind
              _exprIppLaTeX :: PP_Doc
              _exprIselL :: ([Maybe (Nm,PP_Doc)])
              _exprItxt :: Nm
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 22, column 21)
              _ppLaTeX =
                  (if _lhsIneedToParen then ppParens else id) _exprIppLaTeX
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 65, column 21)
              _needToParen =
                  True
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 93, column 21)
              _lhsOselL =
                  []
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 6, column 35)
              _lhsOppLaTeX =
                  _ppLaTeX
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 99, column 21)
              _lhsOtxt =
                  _exprItxt
              -- copy rule (up)
              _lhsOexprKind =
                  _exprIexprKind
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (from local)
              _exprOneedToParen =
                  _needToParen
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              ( _exprIexprKind,_exprIppLaTeX,_exprIselL,_exprItxt) =
                  expr_ _exprOfmGam _exprOneedToParen _exprOopts 
          in  ( _lhsOexprKind,_lhsOppLaTeX,_lhsOselL,_lhsOtxt)))
sem_Expr_Retain :: T_Expr  ->
                   T_Expr 
sem_Expr_Retain expr_  =
    (\ _lhsIfmGam
       _lhsIneedToParen
       _lhsIopts ->
         (let _lhsOexprKind :: ExprKind
              _lhsOselL :: ([Maybe (Nm,PP_Doc)])
              _lhsOppLaTeX :: PP_Doc
              _lhsOtxt :: Nm
              _exprOfmGam :: (FmGam Expr)
              _exprOneedToParen :: Bool
              _exprOopts :: Opts
              _exprIexprKind :: ExprKind
              _exprIppLaTeX :: PP_Doc
              _exprIselL :: ([Maybe (Nm,PP_Doc)])
              _exprItxt :: Nm
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 83, column 21)
              _lhsOexprKind =
                  EKOther
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 93, column 21)
              _lhsOselL =
                  []
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 6, column 35)
              _lhsOppLaTeX =
                  _exprIppLaTeX
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 99, column 21)
              _lhsOtxt =
                  _exprItxt
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOneedToParen =
                  _lhsIneedToParen
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              ( _exprIexprKind,_exprIppLaTeX,_exprIselL,_exprItxt) =
                  expr_ _exprOfmGam _exprOneedToParen _exprOopts 
          in  ( _lhsOexprKind,_lhsOppLaTeX,_lhsOselL,_lhsOtxt)))
sem_Expr_SP :: T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_SP lExpr_ rExpr_  =
    (\ _lhsIfmGam
       _lhsIneedToParen
       _lhsIopts ->
         (let _lhsOexprKind :: ExprKind
              _lhsOselL :: ([Maybe (Nm,PP_Doc)])
              _lhsOppLaTeX :: PP_Doc
              _lhsOtxt :: Nm
              _lExprOfmGam :: (FmGam Expr)
              _lExprOneedToParen :: Bool
              _lExprOopts :: Opts
              _rExprOfmGam :: (FmGam Expr)
              _rExprOneedToParen :: Bool
              _rExprOopts :: Opts
              _lExprIexprKind :: ExprKind
              _lExprIppLaTeX :: PP_Doc
              _lExprIselL :: ([Maybe (Nm,PP_Doc)])
              _lExprItxt :: Nm
              _rExprIexprKind :: ExprKind
              _rExprIppLaTeX :: PP_Doc
              _rExprIselL :: ([Maybe (Nm,PP_Doc)])
              _rExprItxt :: Nm
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 15, column 21)
              _ppLaTeX =
                  let c = case _rExprIexprKind of
                            EKNm n -> case show n of
                                        (c:_) | isAlpha c -> (>#<)
                                              | otherwise -> (>|<)
                                        _                 -> (>#<)
                            _      -> (>#<)
                  in  _lExprIppLaTeX `c` _rExprIppLaTeX
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 65, column 21)
              _needToParen =
                  True
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 83, column 21)
              _lhsOexprKind =
                  EKOther
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 93, column 21)
              _lhsOselL =
                  []
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 6, column 35)
              _lhsOppLaTeX =
                  _ppLaTeX
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 99, column 21)
              _lhsOtxt =
                  _lExprItxt `const` _rExprItxt
              -- copy rule (down)
              _lExprOfmGam =
                  _lhsIfmGam
              -- copy rule (from local)
              _lExprOneedToParen =
                  _needToParen
              -- copy rule (down)
              _lExprOopts =
                  _lhsIopts
              -- copy rule (down)
              _rExprOfmGam =
                  _lhsIfmGam
              -- copy rule (from local)
              _rExprOneedToParen =
                  _needToParen
              -- copy rule (down)
              _rExprOopts =
                  _lhsIopts
              ( _lExprIexprKind,_lExprIppLaTeX,_lExprIselL,_lExprItxt) =
                  lExpr_ _lExprOfmGam _lExprOneedToParen _lExprOopts 
              ( _rExprIexprKind,_rExprIppLaTeX,_rExprIselL,_rExprItxt) =
                  rExpr_ _rExprOfmGam _rExprOneedToParen _rExprOopts 
          in  ( _lhsOexprKind,_lhsOppLaTeX,_lhsOselL,_lhsOtxt)))
sem_Expr_Sel :: T_Expr  ->
                T_MbExpr  ->
                T_Expr 
sem_Expr_Sel expr_ selMbExpr_  =
    (\ _lhsIfmGam
       _lhsIneedToParen
       _lhsIopts ->
         (let _lhsOppLaTeX :: PP_Doc
              _selMbExprOneedToParen :: Bool
              _exprOneedToParen :: Bool
              _lhsOexprKind :: ExprKind
              _lhsOselL :: ([Maybe (Nm,PP_Doc)])
              _lhsOtxt :: Nm
              _exprOfmGam :: (FmGam Expr)
              _exprOopts :: Opts
              _selMbExprOfmGam :: (FmGam Expr)
              _selMbExprOopts :: Opts
              _exprIexprKind :: ExprKind
              _exprIppLaTeX :: PP_Doc
              _exprIselL :: ([Maybe (Nm,PP_Doc)])
              _exprItxt :: Nm
              _selMbExprImbPPLaTeX :: (Maybe (Nm,PP_Doc))
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 26, column 21)
              _lhsOppLaTeX =
                  _exprIppLaTeX
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 57, column 21)
              _selMbExprOneedToParen =
                  False
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 58, column 21)
              _exprOneedToParen =
                  case _selMbExprImbPPLaTeX of
                      Just (n,_) | nmIsOver n
                        -> False
                      _ -> case _exprIexprKind of
                             EKNm _ -> False
                             _      -> _lhsIneedToParen
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 81, column 21)
              _lhsOexprKind =
                  _exprIexprKind
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 92, column 21)
              _lhsOselL =
                  _selMbExprImbPPLaTeX : _exprIselL
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 99, column 21)
              _lhsOtxt =
                  _exprItxt
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _selMbExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _selMbExprOopts =
                  _lhsIopts
              ( _exprIexprKind,_exprIppLaTeX,_exprIselL,_exprItxt) =
                  expr_ _exprOfmGam _exprOneedToParen _exprOopts 
              ( _selMbExprImbPPLaTeX) =
                  selMbExpr_ _selMbExprOfmGam _selMbExprOneedToParen _selMbExprOopts 
          in  ( _lhsOexprKind,_lhsOppLaTeX,_lhsOselL,_lhsOtxt)))
sem_Expr_SelTop :: T_Expr  ->
                   T_Expr 
sem_Expr_SelTop expr_  =
    (\ _lhsIfmGam
       _lhsIneedToParen
       _lhsIopts ->
         (let _lhsOexprKind :: ExprKind
              _lhsOselL :: ([Maybe (Nm,PP_Doc)])
              _lhsOtxt :: Nm
              _lhsOppLaTeX :: PP_Doc
              _exprOfmGam :: (FmGam Expr)
              _exprOneedToParen :: Bool
              _exprOopts :: Opts
              _exprIexprKind :: ExprKind
              _exprIppLaTeX :: PP_Doc
              _exprIselL :: ([Maybe (Nm,PP_Doc)])
              _exprItxt :: Nm
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 24, column 21)
              _ppLaTeXSel =
                  ppSelLaTeX ((== nmOverl),(== nmOverVec)) _exprIppLaTeX (reverse _exprIselL)
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 25, column 21)
              _ppLaTeX =
                  if _exprIexprKind == EKEmp then empty else _ppLaTeXSel
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 65, column 21)
              _needToParen =
                  True
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 83, column 21)
              _lhsOexprKind =
                  EKOther
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 93, column 21)
              _lhsOselL =
                  []
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 104, column 21)
              _lhsOtxt =
                  nmNone
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 6, column 35)
              _lhsOppLaTeX =
                  _ppLaTeX
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (from local)
              _exprOneedToParen =
                  _needToParen
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              ( _exprIexprKind,_exprIppLaTeX,_exprIselL,_exprItxt) =
                  expr_ _exprOfmGam _exprOneedToParen _exprOopts 
          in  ( _lhsOexprKind,_lhsOppLaTeX,_lhsOselL,_lhsOtxt)))
sem_Expr_StrAsIs :: String ->
                    T_Expr 
sem_Expr_StrAsIs str_  =
    (\ _lhsIfmGam
       _lhsIneedToParen
       _lhsIopts ->
         (let _lhsOexprKind :: ExprKind
              _lhsOselL :: ([Maybe (Nm,PP_Doc)])
              _lhsOtxt :: Nm
              _lhsOppLaTeX :: PP_Doc
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 11, column 21)
              _ppLaTeX =
                  pp str_
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 83, column 21)
              _lhsOexprKind =
                  EKOther
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 93, column 21)
              _lhsOselL =
                  []
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 103, column 21)
              _lhsOtxt =
                  Nm str_
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 6, column 35)
              _lhsOppLaTeX =
                  _ppLaTeX
          in  ( _lhsOexprKind,_lhsOppLaTeX,_lhsOselL,_lhsOtxt)))
sem_Expr_StrText :: String ->
                    T_Expr 
sem_Expr_StrText str_  =
    (\ _lhsIfmGam
       _lhsIneedToParen
       _lhsIopts ->
         (let _lhsOexprKind :: ExprKind
              _lhsOselL :: ([Maybe (Nm,PP_Doc)])
              _lhsOtxt :: Nm
              _lhsOppLaTeX :: PP_Doc
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 10, column 21)
              _ppLaTeX =
                  switchLaTeXLhs (mkMBox (text str_))
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 83, column 21)
              _lhsOexprKind =
                  EKOther
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 93, column 21)
              _lhsOselL =
                  []
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 103, column 21)
              _lhsOtxt =
                  Nm str_
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 6, column 35)
              _lhsOppLaTeX =
                  _ppLaTeX
          in  ( _lhsOexprKind,_lhsOppLaTeX,_lhsOselL,_lhsOtxt)))
sem_Expr_Undefined :: T_Expr 
sem_Expr_Undefined  =
    (\ _lhsIfmGam
       _lhsIneedToParen
       _lhsIopts ->
         (let _lhsOexprKind :: ExprKind
              _lhsOselL :: ([Maybe (Nm,PP_Doc)])
              _lhsOppLaTeX :: PP_Doc
              _lhsOtxt :: Nm
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 38, column 21)
              _ppLaTeX =
                  pp (fmNmFmtCmd _lhsIopts _lhsIfmGam $ Nm "rulerUndefinedExtern")
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 83, column 21)
              _lhsOexprKind =
                  EKOther
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 93, column 21)
              _lhsOselL =
                  []
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 6, column 35)
              _lhsOppLaTeX =
                  _ppLaTeX
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 99, column 21)
              _lhsOtxt =
                  nmNone
          in  ( _lhsOexprKind,_lhsOppLaTeX,_lhsOselL,_lhsOtxt)))
sem_Expr_Uniq :: T_Expr 
sem_Expr_Uniq  =
    (\ _lhsIfmGam
       _lhsIneedToParen
       _lhsIopts ->
         (let _lhsOexprKind :: ExprKind
              _lhsOselL :: ([Maybe (Nm,PP_Doc)])
              _lhsOppLaTeX :: PP_Doc
              _lhsOtxt :: Nm
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 83, column 21)
              _lhsOexprKind =
                  EKOther
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 93, column 21)
              _lhsOselL =
                  []
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 6, column 35)
              _lhsOppLaTeX =
                  empty
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 99, column 21)
              _lhsOtxt =
                  nmNone
          in  ( _lhsOexprKind,_lhsOppLaTeX,_lhsOselL,_lhsOtxt)))
sem_Expr_Var :: Nm ->
                T_Expr 
sem_Expr_Var nm_  =
    (\ _lhsIfmGam
       _lhsIneedToParen
       _lhsIopts ->
         (let _lhsOexprKind :: ExprKind
              _lhsOselL :: ([Maybe (Nm,PP_Doc)])
              _lhsOtxt :: Nm
              _lhsOppLaTeX :: PP_Doc
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 27, column 21)
              _ppLaTeX =
                  ppNmLaTeX . nmLhs2TeXSafe $ nm_
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 80, column 21)
              _lhsOexprKind =
                  EKNm nm_
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 93, column 21)
              _lhsOselL =
                  []
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 102, column 21)
              _lhsOtxt =
                  nm_
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 6, column 35)
              _lhsOppLaTeX =
                  _ppLaTeX
          in  ( _lhsOexprKind,_lhsOppLaTeX,_lhsOselL,_lhsOtxt)))
sem_Expr_Wrap :: WrKind ->
                 T_Expr  ->
                 T_Expr 
sem_Expr_Wrap wrKind_ expr_  =
    (\ _lhsIfmGam
       _lhsIneedToParen
       _lhsIopts ->
         (let _lhsOexprKind :: ExprKind
              _lhsOselL :: ([Maybe (Nm,PP_Doc)])
              _lhsOppLaTeX :: PP_Doc
              _lhsOtxt :: Nm
              _exprOfmGam :: (FmGam Expr)
              _exprOneedToParen :: Bool
              _exprOopts :: Opts
              _exprIexprKind :: ExprKind
              _exprIppLaTeX :: PP_Doc
              _exprIselL :: ([Maybe (Nm,PP_Doc)])
              _exprItxt :: Nm
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 28, column 21)
              _ppLaTeX =
                  let wr o c e
                        = switchLaTeXLhs (mkTexCmdUse (show o) empty) >#< e >#< switchLaTeXLhs (mkTexCmdUse (show c) empty)
                  in  case gamLookup wrKind_ wrKindGam of
                        Just i -> wr beg end _exprIppLaTeX
                               where beg = fmNmFmtCmd _lhsIopts _lhsIfmGam $ wkBegCmd i
                                     end = fmNmFmtCmd _lhsIopts _lhsIfmGam $ wkEndCmd i
                        _      -> _exprIppLaTeX
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 83, column 21)
              _lhsOexprKind =
                  EKOther
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 93, column 21)
              _lhsOselL =
                  []
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 6, column 35)
              _lhsOppLaTeX =
                  _ppLaTeX
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 99, column 21)
              _lhsOtxt =
                  _exprItxt
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOneedToParen =
                  _lhsIneedToParen
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              ( _exprIexprKind,_exprIppLaTeX,_exprIselL,_exprItxt) =
                  expr_ _exprOfmGam _exprOneedToParen _exprOopts 
          in  ( _lhsOexprKind,_lhsOppLaTeX,_lhsOselL,_lhsOtxt)))
sem_Expr_WrapCnstr :: T_ECnstr  ->
                      T_Expr 
sem_Expr_WrapCnstr cnstr_  =
    (\ _lhsIfmGam
       _lhsIneedToParen
       _lhsIopts ->
         (let _lhsOexprKind :: ExprKind
              _lhsOselL :: ([Maybe (Nm,PP_Doc)])
              _lhsOppLaTeX :: PP_Doc
              _lhsOtxt :: Nm
              _cnstrOfmGam :: (FmGam Expr)
              _cnstrOopts :: Opts
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 83, column 21)
              _lhsOexprKind =
                  EKOther
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 93, column 21)
              _lhsOselL =
                  []
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 6, column 35)
              _lhsOppLaTeX =
                  empty
              -- use rule "build/ruler2/Expr/LaTeXAG.ag"(line 99, column 21)
              _lhsOtxt =
                  nmNone
              -- copy rule (down)
              _cnstrOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _cnstrOopts =
                  _lhsIopts
          in  ( _lhsOexprKind,_lhsOppLaTeX,_lhsOselL,_lhsOtxt)))
-- MbExpr ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         needToParen          : Bool
         opts                 : Opts
      synthesized attribute:
         mbPPLaTeX            : Maybe (Nm,PP_Doc)
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
type T_MbExpr  = (FmGam Expr) ->
                 Bool ->
                 Opts ->
                 ( (Maybe (Nm,PP_Doc)))
sem_MbExpr_Just :: T_Expr  ->
                   T_MbExpr 
sem_MbExpr_Just just_  =
    (\ _lhsIfmGam
       _lhsIneedToParen
       _lhsIopts ->
         (let _lhsOmbPPLaTeX :: (Maybe (Nm,PP_Doc))
              _justOfmGam :: (FmGam Expr)
              _justOneedToParen :: Bool
              _justOopts :: Opts
              _justIexprKind :: ExprKind
              _justIppLaTeX :: PP_Doc
              _justIselL :: ([Maybe (Nm,PP_Doc)])
              _justItxt :: Nm
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 48, column 21)
              _lhsOmbPPLaTeX =
                  Just (_justItxt,_justIppLaTeX)
              -- copy rule (down)
              _justOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _justOneedToParen =
                  _lhsIneedToParen
              -- copy rule (down)
              _justOopts =
                  _lhsIopts
              ( _justIexprKind,_justIppLaTeX,_justIselL,_justItxt) =
                  just_ _justOfmGam _justOneedToParen _justOopts 
          in  ( _lhsOmbPPLaTeX)))
sem_MbExpr_Nothing :: T_MbExpr 
sem_MbExpr_Nothing  =
    (\ _lhsIfmGam
       _lhsIneedToParen
       _lhsIopts ->
         (let _lhsOmbPPLaTeX :: (Maybe (Nm,PP_Doc))
              -- "build/ruler2/Expr/LaTeXAG.ag"(line 47, column 21)
              _lhsOmbPPLaTeX =
                  Nothing
          in  ( _lhsOmbPPLaTeX)))
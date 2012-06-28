

-- UUAGC 0.9.39.1 (build/ruler2/AbsSyn/AbsSyn1.ag)
module AbsSyn.AbsSyn1(module Expr.Expr, module Ty.Ty, module ViewSel.ViewSel, module Admin
, AGItf (..), Decl (..), Decls, AttrIntro (..), AttrIntros, FldIntro (..), FldIntros
, AttrIntroDecl (..), AttrIntroDecls, AttrRename (..), AttrRenames, RuleJudgeIntro (..), RuleJudgeIntros
, RExpr (..), RExprs, RExprEqn (..), AttrEqn (..), AttrEqns) where

import Common
import Expr.Expr
import Ty.Ty
import ViewSel.ViewSel
import Admin (BldRename (..))












-- AGItf -------------------------------------------------------
data AGItf  = AGItf_AGItf (Decls ) 
-- AttrEqn -----------------------------------------------------
data AttrEqn  = AttrEqn_Del (Nm) 
              | AttrEqn_Eqn (Nm) (Expr) 
-- AttrEqns ----------------------------------------------------
type AttrEqns  = [AttrEqn ]
-- AttrIntro ---------------------------------------------------
data AttrIntro  = AttrIntro_Intro (([AtProp])) (Nm) (Nm) 
-- AttrIntroDecl -----------------------------------------------
data AttrIntroDecl  = AttrIntroDecl_Attrs (AttrIntros ) (AttrIntros ) (AttrIntros ) 
                    | AttrIntroDecl_AttrsProp (AttrIntros ) 
                    | AttrIntroDecl_Scheme (SPos) (Nm) (AttrRenames ) 
-- AttrIntroDecls ----------------------------------------------
type AttrIntroDecls  = [AttrIntroDecl ]
-- AttrIntros --------------------------------------------------
type AttrIntros  = [AttrIntro ]
-- AttrRename --------------------------------------------------
data AttrRename  = AttrRename_EqualTo (SPos) (Nm) (Nm) 
                 | AttrRename_Rename (SPos) (Nm) (Nm) 
-- AttrRenames -------------------------------------------------
type AttrRenames  = [AttrRename ]
-- Decl --------------------------------------------------------
data Decl  = Decl_Attr (AttrIntroDecls ) 
           | Decl_DataAST (SPos) (Nm) (([Nm])) (Decls ) 
           | Decl_DataASTAlt (SPos) (Nm) (Nm) ((Maybe Nm)) (FldIntros ) 
           | Decl_DataASTView (SPos) (Nm) (Decls ) 
           | Decl_Explain ((Maybe Nm)) (Expr) 
           | Decl_Extern (([Nm])) 
           | Decl_Fmt (FmKind) (AtDir) (Expr) (Expr) 
           | Decl_Include (SPos) (Nm) 
           | Decl_Preamble (FmKind) (String) 
           | Decl_RulView (SPos) (Nm) (RuleJudgeIntros ) (([[Nm]])) 
           | Decl_Rule (SPos) (Nm) ((Maybe Nm)) ((Maybe ViewSel)) ((Maybe String)) (Decls ) 
           | Decl_Rules (SPos) (Nm) (Nm) (ViewSel) (String) (Decls ) 
           | Decl_RulesGroup (SPos) (Nm) (Nm) (ViewSel) (String) (([(Nm,Nm)])) 
           | Decl_Scheme (SPos) (ScKind) (Nm) ((Maybe String)) (Decls ) 
           | Decl_SchemeDeriv (SPos) (ScKind) (Nm) (ScDeriv) ((Maybe String)) (Decls ) 
           | Decl_ScmView (Nm) (Decls ) 
           | Decl_ShpDel (SPos) (([FmKind])) 
           | Decl_ShpJudge (SPos) (FmKind) (Expr) 
           | Decl_ViewHierarchy (([[Nm]])) 
-- Decls -------------------------------------------------------
type Decls  = [Decl ]
-- FldIntro ----------------------------------------------------
data FldIntro  = FldIntro_Intro (Nm) (Ty) 
-- FldIntros ---------------------------------------------------
type FldIntros  = [FldIntro ]
-- RExpr -------------------------------------------------------
data RExpr  = RExpr_Del (SPos) (([Nm])) 
            | RExpr_Judge (SPos) ((Maybe Nm)) (Nm) (RExprEqn ) (Bool) 
-- RExprEqn ----------------------------------------------------
data RExprEqn  = RExprEqn_Attrs (AttrEqns ) 
               | RExprEqn_Expr (Expr) 
-- RExprs ------------------------------------------------------
type RExprs  = [RExpr ]
-- RuleJudgeIntro ----------------------------------------------
data RuleJudgeIntro  = RuleJudgeIntro_PrePost (([Nm])) (RExprs ) (RExprs ) 
                     | RuleJudgeIntro_RulesetRule (SPos) (Nm) (Nm) (([BldRename])) 
-- RuleJudgeIntros ---------------------------------------------
type RuleJudgeIntros  = [RuleJudgeIntro ]
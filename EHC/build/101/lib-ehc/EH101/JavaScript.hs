

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/JavaScript.ag)
module EH101.JavaScript(JavaScriptModule (..), Stat (..), StatL, NmExpr, NmExprL, Expr (..), ExprL, MbExpr, AGItf (..)
, Alt (..), AltL
, jsVarDecl
, exprMbVar) where

import EH101.Base.Common











-- | declare a var, or a field in object, depending on name being qualified
jsVarDecl :: HsName -> Expr -> Stat
jsVarDecl n e
  = case hsnQualifier n of
      Just _ -> Stat_Assign (Expr_Var n) e
      _      -> Stat_VarDecl n (Just e)



exprMbVar :: Expr -> Maybe HsName
exprMbVar (Expr_Var n) = Just n
exprMbVar _            = Nothing
-- AGItf -------------------------------------------------------
data AGItf  = AGItf_AGItf !(JavaScriptModule ) 
-- Alt ---------------------------------------------------------
data Alt  = Alt_Alt !(Int) !(StatL ) 
-- AltL --------------------------------------------------------
type AltL  = [Alt ]
-- Expr --------------------------------------------------------
data Expr  = Expr_Arr !(ExprL ) 
           | Expr_ArrInx !(Expr ) !(Expr ) 
           | Expr_Call !(Expr ) !(ExprL ) 
           | Expr_Char !(Char) 
           | Expr_False 
           | Expr_Fun !((Maybe HsName)) !(([HsName])) !(Stat ) 
           | Expr_If !(Expr ) !(Expr ) !(Expr ) 
           | Expr_Inline !(String) 
           | Expr_Int !(Integer) 
           | Expr_New !(Expr ) 
           | Expr_Obj !(NmExprL ) 
           | Expr_ObjFld !(Expr ) !(HsName) 
           | Expr_Op !(HsName) !(Expr ) !(Expr ) 
           | Expr_Sel !(Expr ) !(Expr ) 
           | Expr_Str !(String) 
           | Expr_This 
           | Expr_True 
           | Expr_Undefined 
           | Expr_Var !(HsName) 
-- ExprL -------------------------------------------------------
type ExprL  = [Expr ]
-- JavaScriptModule --------------------------------------------
data JavaScriptModule  = JavaScriptModule_Mod !(StatL ) !(StatL ) 
-- MbExpr ------------------------------------------------------
type MbExpr  = Maybe Expr 
-- NmExpr ------------------------------------------------------
type NmExpr  = ( (HsName),Expr )
-- NmExprL -----------------------------------------------------
type NmExprL  = [NmExpr ]
-- Stat --------------------------------------------------------
data Stat  = Stat_Assign !(Expr ) !(Expr ) 
           | Stat_Block !(StatL ) 
           | Stat_Break 
           | Stat_Expr !(Expr ) 
           | Stat_FunDecl !(Expr ) 
           | Stat_Ret !(Expr ) 
           | Stat_Switch !(Expr ) !(AltL ) 
           | Stat_Throw !(Expr ) 
           | Stat_VarDecl !(HsName) !(MbExpr ) 
-- StatL -------------------------------------------------------
type StatL  = [Stat ]
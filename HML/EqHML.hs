module EqHML where

import EH8.EH
import EH8.Base.HsName

instance Eq TyQuantifiedScheme where
 TyQuantifiedScheme_Bottom      == TyQuantifiedScheme_Bottom      = True
 TyQuantifiedScheme_Quant a1 b1 == TyQuantifiedScheme_Quant a2 b2 = a1 == a2 && b1 == b2
 _                              == _                              = False
  
instance Eq Scheme where
 Scheme_Simple a1 b1 == Scheme_Simple a2 b2 = a1 == a2 && b1 == b2
  
instance Eq TyScheme where
 TyScheme_Quant a1 b1  == TyScheme_Quant a2 b2  = a1 == a2 && b1 == b2
 TyScheme_SystemF a    == TyScheme_SystemF b    = a == b
 TyScheme_Bottom       == TyScheme_Bottom       = True
 TyScheme_Sugar a1 b1  == TyScheme_Sugar a2 b2  = a1 == a2 && b1 == b2
 TyScheme_Forall a1 b1 == TyScheme_Forall a2 b2 = a1 == a2 && b1 == b2
 _                     == _                     = False

instance Eq TyIndex where
 TyIndex_Group a1 b1 == TyIndex_Group a2 b2 = a1 == a2 && b1 == b2
 
instance Eq TyExpr where
 TyExpr_Con a          == TyExpr_Con b          = a == b
 TyExpr_App a1 b1      == TyExpr_App a2 b2      = a1 == a2 && b1 == b2
 TyExpr_AppTop a       == TyExpr_AppTop b       = a == b
 TyExpr_Parens a       == TyExpr_Parens b       = a == b
 TyExpr_Ann a1 b1      == TyExpr_Ann a2 b2      = a1 == a2 && b1 == b2
 TyExpr_Wild           == TyExpr_Wild           = True
 TyExpr_Mono           == TyExpr_Mono           = True
 TyExpr_Var a          == TyExpr_Var b          = a == b
 TyExpr_VarWild a      == TyExpr_VarWild b      = a == b
 TyExpr_Quant a1 b1 c1 == TyExpr_Quant a2 b2 c2 = a1 == a2 && b1 == b2 && c1 == c2
 TyExpr_Row a          == TyExpr_Row b          = a == b 
 
instance Eq TyExprAnn where
 TyExprAnn_Empty == TyExprAnn_Empty = True
 
instance Eq RowTyExpr where
 RowTyExpr_Empty        == RowTyExpr_Empty        = True
 RowTyExpr_Ext a1 b1 c1 == RowTyExpr_Ext a2 b2 c2 = a1 == a2 && b1 == b2 && c1 == c2
 _                      == _                      = False
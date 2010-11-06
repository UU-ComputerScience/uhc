{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Pretty printing module
module Pretty where

import EH8.EH
import EH8.Ty
import EH8.Base.HsName

import EH.Util.Pretty hiding (pp)

import Data.List
import GHC.Show

import qualified Data.Map as M

class Pretty a where
   pp :: a -> String
   
-- instance Pretty a => Show a where
   -- show = pp

instance (Pretty a, Pretty b) => Pretty (M.Map a b) where
   pp = pp . M.toList
   
instance Pretty a => Pretty [a] where
   pp = ("["++) . (++"]") . intercalate "," . map pp
   
instance (Pretty a, Pretty b) => Pretty (a, b) where
   pp (a,b) = "("++pp a++", "++pp b++")"
   
instance Pretty HsName where
   pp = show

instance Pretty TyIndex where
  pp (TyIndex_Group var bound) = "(" ++ pp var ++ " >= " ++ pp bound ++ ")"
  
instance Pretty TyScheme where
  pp TyScheme_Bottom                 = "_|_"
  pp (TyScheme_SystemF typ)          = pp typ
  pp (TyScheme_Quant bound expr)     = pp bound ++ ". " ++ pp expr
  pp p@(TyScheme_Sugar{})            = "S@"
  pp (TyScheme_Forall a p)           = "forall " ++ pp a ++ ". " ++ pp p
                                       
-- instance Pretty TyQuantifiedScheme where
  -- pp (TyQuantifiedScheme_Quant i b) = "forall (" ++ pp i ++ "). " ++ pp b
  -- pp TyQuantifiedScheme_Bottom      = "_|_"

instance Pretty Scheme where
  pp (Scheme_Simple nm b) = "(" ++ pp nm ++ " >= " ++ pp b ++ ")"
  
instance Pretty TyMono where
  pp (TyMono_Var a)   = pp a
  pp (TyMono_App a b) = pp a ++ " -> " ++ pp b
  pp (TyMono_Con a)   = pp a
  pp (TyMono_AppTop a)= pp a
  
instance Pretty TyPoly where
  pp (TyPoly_Var a)   = pp a
  pp (TyPoly_App a b) = pp a ++ " -> " ++ pp b
  pp (TyPoly_Con a)   = pp a
  -- pp (TyPoly_AppTop a)= pp a
  
instance Pretty TyQu where
  pp (TyQu_Forall _) = "forall"
  
instance Pretty Decl where
  pp (Decl_TySig nm decl) = pp nm  ++ " :: " ++ pp decl
  pp (Decl_Val pat expr ) = pp pat ++ " = "  ++ pp expr
  
instance Pretty TyExpr where
  pp (TyExpr_Con a)        = pp a
  pp (TyExpr_App a b)      = case a of
                               (TyExpr_App x a) 
                                   | pp x == "->" -> pp a ++ " " ++ pp x ++ " " ++ pp b
                               _  -> pp a ++ " " ++ pp b
  pp (TyExpr_AppTop a)     = pp a
  pp (TyExpr_Parens a)     = "(" ++ pp a ++ ")"
  pp (TyExpr_Ann ann expr) = "[ann] " ++ pp expr
  pp (TyExpr_Wild)         = ""
  pp (TyExpr_Mono)         = ""
  pp (TyExpr_Var a)        = pp a
  pp (TyExpr_VarWild a)    = pp a
  pp (TyExpr_Quant a b c)  = pp a ++ " " ++ pp b ++ ". " ++ pp c
  
instance Pretty ExprAnn where
  pp ExprAnn_Empty = ""

instance Pretty TyExprAnn where
  pp TyExprAnn_Empty = ""

instance Pretty PatExprAnn where
  pp PatExprAnn_Empty = ""  
  
-- instance Pretty Char where
  -- pp = (:[])
  
-- instance Pretty Int where
  -- pp a = showSignedInt 0 a ""
  
instance Show TyScheme where
  show = pp
  
instance Show TyIndex where
  show = pp
  
instance Show Expr where
  show = pp
  
instance Pretty PatExpr where
  pp (PatExpr_IConst a   ) = showSignedInt 0 a ""
  pp (PatExpr_CConst a   ) = [a]
  pp (PatExpr_Con nm     ) = pp nm
  pp (PatExpr_Var nm     ) = pp nm
  pp (PatExpr_VarAs nm ty) = pp nm ++ "@" ++ pp ty
  pp (PatExpr_App f a    ) = pp f  ++ " " ++ pp a
  pp (PatExpr_AppTop a   ) = pp a
  pp (PatExpr_Parens a   ) = "(" ++ pp a ++ ")"
  pp (PatExpr_Ann ann expr)= "[ann] " ++ pp expr
  pp (PatExpr_TypeAs ty e) = pp e ++ " :: " ++ pp ty
  
instance Pretty Expr where
  pp (Expr_IConst a     ) = showSignedInt 0 a ""
  pp (Expr_CConst a     ) = [a]
  pp (Expr_Con nm       ) = pp nm
  pp (Expr_Var nm       ) = pp nm
  pp (Expr_App f a      ) = pp f  ++ " " ++ pp a
  pp (Expr_Let s d b    ) = let decls = map pp d
                            in "Let " ++ unlines decls ++ "in " ++ pp b
  pp (Expr_Lam arg b    ) = "\\" ++ pp arg ++ " -> " ++ pp b
  pp (Expr_AppTop a     ) = pp a
  pp (Expr_Parens a     ) = "(" ++ pp a ++ ")"
  pp (Expr_Ann ann expr ) = "[ann] " ++ pp expr
  pp (Expr_TypeAs ty e  ) = pp e ++ " :: " ++ pp ty
  pp (Expr_AppImpred f a) = pp f ++ " " ++ pp a

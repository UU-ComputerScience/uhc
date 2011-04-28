{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
module PrettyUtil where

import HM (Expr(..), TyExpr(..))

type Name = String

-- | Generate a fresh variable
fresh :: Int -> (Name, Int)
fresh i = ('a' : show i, i +1)

instance Show Expr where
  showsPrec _ = pp
  
instance Show TyExpr where
  showsPrec _ = pp
  
-- deriving instance Show Expr
-- deriving instance Show TyExpr

class Pretty a where
   pp :: a -> ShowS
   
instance Pretty Expr where
   pp (Expr_IConst       int) = shows int
   pp (Expr_Var          var) = showString var
   pp (Expr_Con          con) = showString con
   pp (Expr_Paren        exp) = showParen True (pp exp)
   pp (Expr_Lam     arg body) = showString "\\" 
                              . showString arg
                              . showString " -> "
                              . pp body
   pp (Expr_App     func arg) = pp func
                              . showString " "
                              . pp arg
   pp (Expr_Let nm bind body) = showString "Let "
                              . showString nm 
                              . showString " = "
                              . pp bind 
                              . showString " in " 
                              . pp body
                              
instance Pretty TyExpr where
   pp (TyExpr_Var      var) = showString var
   pp (TyExpr_Con      con) = showString con
   pp (TyExpr_App func arg) = case func of
                                (TyExpr_App (TyExpr_Con "->") func) -> shows func
                                                                     . showString " -> "
                                                                     . shows arg
                                (TyExpr_Paren (TyExpr_App (TyExpr_Con "->") func))
                                                                    -> shows func
                                                                     . showString " -> "
                                                                     . shows arg
                                _                                   -> shows func
                                                                     . showString " "
                                                                     . shows arg
   pp (TyExpr_Paren    exp) = showParen True (pp exp)

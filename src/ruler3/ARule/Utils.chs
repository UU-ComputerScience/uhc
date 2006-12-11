-------------------------------------------------------------------------
-- Interface to all ARule transformations and other utilities
-------------------------------------------------------------------------

module ARule.Utils
  ( module ARule.ARule
  , module ARule.PatternUniq
  , module ARule.RwSubst
  , module ARule.AVarRename
  , module ARule.ElimCopyRule
  , module ARule.ElimWildcAssign
  , module ARule.PrettyPrint

  , exprFmtTeXSubst
  )
  where

import UU.Pretty
import Common
import Opts
import FmGam
import Expr.Expr
import Expr.LaTeX
import ARule.ARule
import ARule.PatternUniq
import ARule.RwSubst
import ARule.AVarRename
import ARule.ElimCopyRule
import ARule.ElimWildcAssign
import ARule.PrettyPrint

exprFmtTeXSubst :: Opts -> FmGam Expr -> Expr -> PP_Doc
exprFmtTeXSubst o fmg = exprFmtTeX o fmg . exprSubst o fmg

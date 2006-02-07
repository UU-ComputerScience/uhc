-------------------------------------------------------------------------
-- Interface to all ARule transformations and other utilities
-------------------------------------------------------------------------

module ARuleUtils
  ( module ARule
  , module ARulePatternUniq
  , module ARuleRwSubst
  , module ARuleAVarRename
  , module ARuleElimCopyRule
  , module ARuleElimWildcAssign
  , module ARulePrettyPrint

  , exprFmtTeXSubst
  )
  where

import UU.Pretty
import Common
import Opts
import FmGam
import Expr
import ExprLaTeX
import ARule
import ARulePatternUniq
import ARuleRwSubst
import ARuleAVarRename
import ARuleElimCopyRule
import ARuleElimWildcAssign
import ARulePrettyPrint

exprFmtTeXSubst :: Opts -> FmGam Expr -> Expr -> PP_Doc
exprFmtTeXSubst o fmg = exprFmtTeX o . exprSubst o fmg

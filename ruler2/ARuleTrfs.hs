-------------------------------------------------------------------------
-- Interface to all ARule transformations
-------------------------------------------------------------------------

module ARuleTrfs
  ( module ARule
  , module ARulePatternUniq
  , module ARuleRwSubst
  , module ARuleAVarRename
  , module ARuleCopyRuleElim
  , module ARuleElimWildcAssign
  
  , exprFmtTeXSubst
  )
  where

import UU.Pretty
import Common
import FmGam
import Expr
import ExprLaTeX
import ARule
import ARulePatternUniq
import ARuleRwSubst
import ARuleAVarRename
import ARuleCopyRuleElim
import ARuleElimWildcAssign

exprFmtTeXSubst :: Opts -> FmGam Expr -> Expr -> PP_Doc
exprFmtTeXSubst o fmg = exprFmtTeX o . exprSubst o fmg

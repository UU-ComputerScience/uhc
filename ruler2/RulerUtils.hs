-- $Id: EHTyFitsIn.chs 214 2005-05-28 17:52:29Z atze $

module RulerUtils
  ( jdGamFmExpr
  )
  where

import Common
import Expr
import FmGam
import JdGam

-------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------

jdGamFmExpr :: FmKind -> JdGam Expr -> Expr
jdGamFmExpr k = fkGamLookup exprUnk jdExpr [k,FmSpec]




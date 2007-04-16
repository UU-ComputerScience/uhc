-------------------------------------------------------------------------
-- Interface to all ARule transformations and other utilities
-------------------------------------------------------------------------

%%[1 hs module (ARule.Utils)
%%]

%%[1 hs export (module ARule.ARule, module ARule.PatternUniq, module ARule.RwSubst, module ARule.AVarRename)
%%]

%%[1 hs export (module ARule.ElimCopyRule, module ARule.ElimWildcAssign, module ARule.PrettyPrint)
%%]

%%[1 hs export (exprFmtTeXSubst)
%%]

%%[1 hs import (EH.Util.Pretty, Common, Opts, FmGam, Expr.Expr, Expr.LaTeX)
%%]

%%[1 hs import (ARule.ARule, ARule.PatternUniq, ARule.RwSubst, ARule.AVarRename)
%%]

%%[1 hs import (ARule.ElimCopyRule, ARule.ElimWildcAssign, ARule.PrettyPrint)
%%]

%%[1 hs

exprFmtTeXSubst :: Opts -> FmGam Expr -> Expr -> PP_Doc
exprFmtTeXSubst o fmg = exprFmtTeX o fmg . exprSubst o fmg

%%]

-------------------------------------------------------------------------
-- ECnstr Gamma
-------------------------------------------------------------------------

%%[1 hs module (ECnstrGam)
%%]

%%[1 hs export (module Gam, ECnstrGam, ecGamLookup, ecGamInsert, ecGamFromList)
%%]

%%[1 hs import (qualified Data.Map as Map, Common, Gam, Expr.Expr)
%%]

-------------------------------------------------------------------------
-- Gam
-------------------------------------------------------------------------

%%[1 hs
type ECnstrGam = Gam Expr ECnstr

ecGamLookup :: Expr -> ECnstrGam -> Maybe ECnstr
ecGamLookup e g = gamLookup (ecStrip e) g

ecGamInsert :: Expr -> ECnstr -> ECnstrGam -> ECnstrGam
ecGamInsert e c g = gamInsert (ecStrip e) c g

ecGamFromList :: [(Expr,ECnstr)] -> ECnstrGam
ecGamFromList l = gamFromAssocs [ (ecStrip e,c) | (e,c) <- l ]
%%]

-------------------------------------------------------------------------
-- Stripping for ECnstr
-------------------------------------------------------------------------

%%[1 hs
ecStrip :: Expr -> Expr
ecStrip (Expr_Paren  e  ) = e
-- ecStrip (Expr_AppTop e  ) = e
ecStrip (Expr_Cnstr  e _) = e
ecStrip (Expr_Retain e  ) = e
ecStrip (Expr_Named  _ e) = e
ecStrip e                 = e
%%]

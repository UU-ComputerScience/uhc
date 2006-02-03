-------------------------------------------------------------------------
-- ECnstr Gamma
-------------------------------------------------------------------------

module ECnstrGam
  ( module Gam
  
  , ECnstrGam
  , ecGamLookup, ecGamInsert, ecGamFromList
  )
  where

-- import Data.Maybe
-- import qualified Data.Set as Set
import qualified Data.Map as Map
import Common
import Gam
import Expr

-------------------------------------------------------------------------
-- Gam
-------------------------------------------------------------------------

type ECnstrGam = Gam Expr ECnstr

ecGamLookup :: Expr -> ECnstrGam -> Maybe ECnstr
ecGamLookup e g = Map.lookup (ecStrip e) g

ecGamInsert :: Expr -> ECnstr -> ECnstrGam -> ECnstrGam
ecGamInsert e c g = Map.insert (ecStrip e) c g

ecGamFromList :: [(Expr,ECnstr)] -> ECnstrGam
ecGamFromList l = Map.fromList [ (ecStrip e,c) | (e,c) <- l ]

-------------------------------------------------------------------------
-- Stripping for ECnstr
-------------------------------------------------------------------------

ecStrip :: Expr -> Expr
ecStrip (Expr_Paren  e  ) = e
-- ecStrip (Expr_AppTop e  ) = e
ecStrip (Expr_Cnstr  e _) = e
ecStrip (Expr_Retain e  ) = e
ecStrip (Expr_Named  _ e) = e
ecStrip e                 = e


-- $Id: EHTyFitsIn.chs 214 2005-05-28 17:52:29Z atze $

module RulerUtils
  ( jdGamFmExpr
  , rlLtxGamTranspose
  , gamTranspose
  )
  where

import qualified Data.Map as Map
import Common
import Expr
import FmGam
import JdGam

-------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------

jdGamFmExpr :: FmKind -> JdGam Expr -> Expr
jdGamFmExpr k = fkGamLookup exprUnk jdExpr [k,FmSpec]

-------------------------------------------------------------------------
-- Transpose gam of gam
-------------------------------------------------------------------------

gamTranspose
  :: Ord k =>
     (i -> Gam k v,k -> k -> i -> v -> i')
       -> Gam k i -> Gam k (Gam k i')
gamTranspose (getG,mkI) g
  = Map.fromListWith Map.union [ (k2,Map.singleton k1 (mkI k1 k2 v1 v2)) | (k1,v1) <- Map.toList g, (k2,v2) <- Map.toList (getG v1) ]

rlLtxGamTranspose :: Ord k => Gam k (Gam k (n,v)) -> Gam k (Gam k (n,v))
rlLtxGamTranspose g
  = Map.fromListWith Map.union [ (v,Map.singleton r (n,d)) | (r,vm) <- Map.toList g, (v,(n,d)) <- Map.toList vm ]




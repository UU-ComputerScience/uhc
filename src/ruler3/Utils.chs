-- $Id: EHTyFitsIn.chs 214 2005-05-28 17:52:29Z atze $

%%[1 hs module (Utils)
%%]

%%[1 hs export (jdGamFmExpr, rlLtxGamTranspose, gamTranspose, sc2DATA, rl2SEM)
%%]

%%[1 hs import (qualified Data.Map as Map, Common, Expr.Expr, FmGam, JdShpGam, Admin)
%%]

%%[1 hs

-------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------

jdGamFmExpr :: FmKind -> JdShpGam Expr -> Expr
jdGamFmExpr k = fkGamLookup exprUnk jdshExpr [k,FmSpec]

-------------------------------------------------------------------------
-- Transpose gam of gam
-------------------------------------------------------------------------

gamTranspose
  :: Ord k =>
     (i -> Gam k v,k -> k -> i -> v -> i')
       -> Gam k i -> Gam k (Gam k i')
gamTranspose (getG,mkI) g
  = gamFromAssocsWith gamUnion [ (k2,gamSingleton k1 (mkI k1 k2 v1 v2)) | (k1,v1) <- gamAssocsShadow g, (k2,v2) <- gamAssocsShadow (getG v1) ]

rlLtxGamTranspose :: Ord k => Gam k (Gam k (n,v)) -> Gam k (Gam k (n,v))
rlLtxGamTranspose g
  = gamFromAssocsWith gamUnion [ (v,gamSingleton r (n,d)) | (r,vm) <- gamAssocsShadow g, (v,(n,d)) <- gamAssocsShadow vm ]

-------------------------------------------------------------------------
-- RulerAdmin: AG name info
-------------------------------------------------------------------------

sc2DATA :: ScInfo e -> DtInvGam -> Nm
sc2DATA si dg
  = case scMbAGStr si of
      Just s  -> Nm s
      Nothing -> case gamLookup (scNm si) dg of
                   Just i  -> dtiAGNm i
                   Nothing -> scNm si

rl2SEM :: RlInfo e -> DtInvGam -> Nm -> Nm -> Nm -> Nm
rl2SEM rlInfo dg scNm rlNm vwNm
  = case rlMbAGStr rlInfo of
      Just s  -> Nm s
      Nothing -> case dtVwRlInvGamLookup scNm vwNm rlNm dg of
                   Just (_,_,i) -> daiAGNm i
                   Nothing      -> rlNm

%%]

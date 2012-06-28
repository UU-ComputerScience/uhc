module EH101.Gam.Instantiate
( gamInst1Exists
, valGamInst1Exists
, tyKiGamInst1Exists
, gamInst1ExistsWithVarMp
, valGamInst1ExistsWithVarMp
, tyKiGamInst1ExistsWithVarMp )
where
import EH101.Base.Common
import EH101.Ty
import EH101.VarMp
import EH101.Ty.Trf.Instantiate
import EH101.Gam
import EH101.Gam.ValGam
import EH101.Gam.TyKiGam



{-# LINE 26 "src/ehc/Gam/Instantiate.chs" #-}
gamInst1Exists :: Ord k => (v -> Ty,v -> Ty -> v) -> UID -> Gam k v -> Gam k v
gamInst1Exists (extr,upd) u
  =  fst . gamMapThr (\(n,t) u -> let (u',ue) = mkNewLevUID u in ((n,upd t (tyInst1Exists ue (extr t))),u')) u

{-# LINE 32 "src/ehc/Gam/Instantiate.chs" #-}
gamInst1ExistsWithVarMp :: Ord key => (info -> Ty,Ty -> info -> info) -> VarMp -> UID -> Gam key info -> (Gam key info,VarMp)
gamInst1ExistsWithVarMp getset gamVarMp u g
  = (g',m)
  where (g',m,_)
          = gamDoTyWithVarMp
              getset
              (\_ (t,_) m u -> let (u',ue) = mkNewLevUID u
                           in  (tyInst1Exists ue t,m,u')
              )
              gamVarMp u g

{-# LINE 49 "src/ehc/Gam/Instantiate.chs" #-}
valGamInst1Exists :: UID -> ValGam -> ValGam
valGamInst1Exists = gamInst1Exists (vgiTy,(\vgi t -> vgi {vgiTy=t}))

{-# LINE 54 "src/ehc/Gam/Instantiate.chs" #-}
valGamInst1ExistsWithVarMp :: VarMp -> UID -> ValGam -> (ValGam,VarMp)
valGamInst1ExistsWithVarMp = gamInst1ExistsWithVarMp vgiGetSet

{-# LINE 63 "src/ehc/Gam/Instantiate.chs" #-}
tyKiGamInst1Exists :: UID -> TyKiGam -> TyKiGam
tyKiGamInst1Exists = gamInst1Exists (tkgiKi,(\i k -> i {tkgiKi=k}))

{-# LINE 68 "src/ehc/Gam/Instantiate.chs" #-}
tyKiGamInst1ExistsWithVarMp :: VarMp -> UID -> TyKiGam -> (TyKiGam,VarMp)
tyKiGamInst1ExistsWithVarMp = gamInst1ExistsWithVarMp tkgiGetSet


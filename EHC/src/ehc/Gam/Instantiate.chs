%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instantiation over gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer) module {%{EH}Gam.Instantiate}
%%]

%%[(4 hmtyinfer) hs import ({%{EH}Base.Common})
%%]
%%[(4 hmtyinfer) hs import ({%{EH}Ty},{%{EH}VarMp})
%%]

%%[(4 hmtyinfer) import({%{EH}Ty.Trf.Instantiate})
%%]

%%[(4 hmtyinfer) hs import ({%{EH}Gam},{%{EH}Gam.ValGam})
%%]
%%[(6 hmtyinfer) hs import ({%{EH}Gam.TyKiGam})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instantiation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer).valGamInst1Exists export(gamInst1Exists)
gamInst1Exists :: Ord k => (v -> Ty,v -> Ty -> v) -> UID -> Gam k v -> Gam k v
gamInst1Exists (extr,upd) u
  =  fst . gamMapThr (\(n,t) u -> let (u',ue) = mkNewLevUID u in ((n,upd t (tyInst1Exists ue (extr t))),u')) u
%%]

%%[(8 hmtyinfer) export(gamInst1ExistsWithVarMp)
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
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% For ValGam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer).valGamInst1Exists export(valGamInst1Exists)
valGamInst1Exists :: UID -> ValGam -> ValGam
valGamInst1Exists = gamInst1Exists (vgiTy,(\vgi t -> vgi {vgiTy=t}))
%%]

%%[(8 hmtyinfer).valGamInst1ExistsWithVarMp export(valGamInst1ExistsWithVarMp)
valGamInst1ExistsWithVarMp :: VarMp -> UID -> ValGam -> (ValGam,VarMp)
valGamInst1ExistsWithVarMp = gamInst1ExistsWithVarMp vgiGetSet
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% For TyKiGam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(6 hmtyinfer) export(tyKiGamInst1Exists)
tyKiGamInst1Exists :: UID -> TyKiGam -> TyKiGam
tyKiGamInst1Exists = gamInst1Exists (tkgiKi,(\i k -> i {tkgiKi=k}))
%%]

%%[(8 hmtyinfer).valGamInst1ExistsWithVarMp export(tyKiGamInst1ExistsWithVarMp)
tyKiGamInst1ExistsWithVarMp :: VarMp -> UID -> TyKiGam -> (TyKiGam,VarMp)
tyKiGamInst1ExistsWithVarMp = gamInst1ExistsWithVarMp tkgiGetSet
%%]


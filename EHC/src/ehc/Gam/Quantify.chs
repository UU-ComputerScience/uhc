%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Quantification over gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(3 hmtyinfer) module {%{EH}Gam.Quantify}
%%]

%%[(3 hmtyinfer) hs import ({%{EH}Base.Common})
%%]
%%[(3 hmtyinfer) hs import ({%{EH}Ty})
%%]
%%[(3 hmtyinfer) hs import ({%{EH}VarMp})
%%]

%%[(3 hmtyinfer) import(qualified Data.Set as Set)
%%]

%%[(3 hmtyinfer) import({%{EH}Ty.Trf.Quantify})
%%]
%%[(3 hmtyinfer) import({%{EH}VarMp},{%{EH}Substitutable})
%%]

%%[(9 hmtyinfer || hmtyast) import({%{EH}Ty.Trf.MergePreds})
%%]

%%[(3 hmtyinfer) hs import ({%{EH}Gam},{%{EH}Gam.ValGam})
%%]
%%[(6 hmtyinfer) hs import ({%{EH}Gam.TyKiGam})
%%]
%%[(17 hmtyinfer) hs import ({%{EH}Gam.PolGam})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% For ValGam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(3 hmtyinfer || hmtyast).valGamQuantify export(valGamQuantify)
valGamQuantify :: TyVarIdS -> ValGam -> ValGam
valGamQuantify globTvS = valGamMapTy (\t -> valTyQuantify (`Set.member` globTvS) t)
%%]

%%[(6 hmtyinfer || hmtyast).valGamQuantify -3.valGamQuantify export(valGamQuantify)
valGamQuantify :: TyKiGam -> VarMp -> VarMp -> TyVarIdS -> ValGam -> ValGam
valGamQuantify tyKiGam tvKiVarMp gamVarMp globTvS
  = valGamMapTy (\t -> valTyQuantify (tvarKi tyKiGam tvKiVarMp gamVarMp) (`Set.member` globTvS) t)
%%]

%%[(8 hmtyinfer || hmtyast).valGamQuantifyWithVarMp -6.valGamQuantify export(valGamQuantifyWithVarMp)
valGamQuantifyWithVarMp :: TyKiGam -> VarMp -> VarMp -> TyVarIdS -> ValGam -> (ValGam,VarMp,VarMp)
valGamQuantifyWithVarMp tyKiGam tvKiVarMp gamVarMp globTvS gam
  = valGamDoWithVarMp
      (\_ (t,tyCycMp) m cycMp -> (valTyQuantify (tvarKi tyKiGam tvKiVarMp gamVarMp) (`Set.member` globTvS) t,m,tyCycMp |=> cycMp))
      gamVarMp emptyVarMp gam
%%]

%%[(9 hmtyinfer || hmtyast).valGamQuantify -3.valGamQuantify export(valGamQuantify)
valGamQuantify :: TyVarIdS -> [PredOcc] -> ValGam -> (ValGam,Gam HsName TyMergePredOut)
valGamQuantify globTvS prL g
  =  let  g' = gamMapElts  (\vgi ->  let  tmpo = tyMergePreds prL (vgiTy vgi)
                                          ty   = valTyQuantify (const kiStar) (`Set.member` globTvS) (tmpoTy tmpo)
                                     in   (vgi {vgiTy = ty},tmpo {tmpoTy = ty})
                           ) g
     in   gamUnzip g'
%%]

%%[(9 hmtyinfer || hmtyast).valGamQuantifyWithVarMp -8.valGamQuantifyWithVarMp export(valGamQuantifyWithVarMp)
valGamQuantifyWithVarMp :: TyKiGam -> VarMp -> VarMp -> TyVarIdS -> [PredOcc] -> ValGam -> (ValGam,VarMp,(VarMp,Gam HsName TyMergePredOut))
valGamQuantifyWithVarMp tyKiGam tvKiVarMp gamVarMp globTvS prL valGam
  = valGamDoWithVarMp quant gamVarMp (emptyVarMp,emptyGam) valGam
  where quant nm (t,tyCycVarMp) newVarMp (cycVarMp,tmpoGam)
          = (ty,newVarMp',(cycVarMp', tmpoGam'))
          where tmpo        = tyMergePreds prL t
                ty          = valTyQuantify (tvarKi tyKiGam tvKiVarMp gamVarMp) (`Set.member` globTvS) (tmpoTy tmpo)
                newVarMp'   = newVarMp -- tmpoImplsVarMp tmpo `varmpPlus` m
                tmpoGam'    = gamAdd nm (tmpo {tmpoTy = ty}) tmpoGam
                cycVarMp'   = tyCycVarMp |=> cycVarMp
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% For TyKiGam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(6 hmtyinfer || hmtyast).tyKiGamQuantify export(tyKiGamQuantify)
tyKiGamQuantify :: TyVarIdS -> TyKiGam -> TyKiGam
tyKiGamQuantify globTvS
  = gamMap (\(n,k) -> (n,k {tkgiKi = tyKiQuantify (`Set.member` globTvS) (tkgiKi k)}))
%%]

%%[(8 hmtyinfer || hmtyast).tyKiGamQuantifyWithVarMp -6.tyKiGamQuantify export(tyKiGamQuantifyWithVarMp)
tyKiGamQuantifyWithVarMp :: {- TyKiGam -> VarMp -> -} VarMp -> TyVarIdS -> TyKiGam -> (TyKiGam,VarMp,VarMp)
tyKiGamQuantifyWithVarMp {- tyKiGam tvKiVarMp -} gamVarMp globTvS gam
  = tyKiGamDoWithVarMp
      (\_ (t,tyCycMp) m cycMp -> (tyKiQuantify {- (tvarKi tyKiGam tvKiVarMp gamVarMp) -} (`Set.member` globTvS) t,m,tyCycMp |=> cycMp))
      gamVarMp emptyVarMp gam
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% For PolGam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(17 hmtyinfer || hmtyast) export( quantifyPolGam)
quantifyPolGam :: PolGam -> PolGam
quantifyPolGam gam
  = let fvs = ftv gam
        notElemFtvs tv = not $ elem tv fvs
     in mapPolGam (tyQuantify notElemFtvs) gam
%%]


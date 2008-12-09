%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gamma utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer) module {%{EH}Gam.Utils} import(Data.List,{%{EH}Base.Common},{%{EH}Base.Opts},{%{EH}Ty},{%{EH}Ty.FitsInCommon},{%{EH}Ty.FitsIn},{%{EH}Error},{%{EH}Gam},{%{EH}VarMp},{%{EH}Substitutable})
%%]

%%[(4_2 hmtyinfer) import({%{EH}Ty.Trf.ElimAlts}) export(valGamElimAlts)
%%]

%%[(50 hmtyinfer) import({%{EH}Ty.Trf.ElimEqual}) export(valGamElimEqual)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Alts elim
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4_2 hmtyinfer).valGamElimAlts
valGamElimAlts :: FIOpts -> FIEnv -> TyVarIdL -> UID -> VarMp -> ValGam -> (ValGam,VarMp,ErrGam)
valGamElimAlts opts env globTvL uniq gVarMp g
  =  let  (g',(c,eg,_))
            =  gamMapThr
                  (\(n,vgi) (c,eg,u)
                  	->  let  (u',u1) = mkNewLevUID u
                  	         fo = tyElimAlts (mkFitsInWrap' env) opts globTvL u1 (c |=> gVarMp) (c |=> vgiTy vgi)
                  	         cg = varmpFilterTyAltsMappedBy gVarMp (foVarMp fo)
                  	    in   ((n,vgi {vgiTy = foTy fo}),(foVarMp fo |=> c |=> cg,gamAdd n (foErrL fo) eg,u'))
                  )
                  (emptyVarMp,emptyGam,uniq) (gVarMp |=> g)
     in   (g',tyElimAltsCleanup gVarMp c,eg)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Equal elim
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 hmtyinfer).valGamElimEqual
valGamElimEqual :: ValGam -> ValGam
valGamElimEqual g
  =  let  (g',_)
            =  gamMapThr
                  (\(n,vgi) _
                  	->  let  (t,_) = tyElimEqual (vgiTy vgi)
                  	    in   ((n,vgi {vgiTy = t}),())
                  )
                  () g
     in   g'
%%]


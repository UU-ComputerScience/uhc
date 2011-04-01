%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gamma utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer) module {%{EH}Gam.Utils} import(Data.List,{%{EH}Base.Common},{%{EH}Opts},{%{EH}Ty},{%{EH}Ty.FitsInCommon},{%{EH}Ty.FitsIn},{%{EH}Error},{%{EH}Gam},{%{EH}VarMp},{%{EH}Substitutable})
%%]

%%[(4_2 hmtyinfer) import({%{EH}Ty.Trf.ElimAlts})
%%]

%%[50 import(qualified Data.Map as Map, qualified Data.Set as Set)
%%]

%%[50 import({%{EH}Ty.UsedNames},{%{EH}Module},{%{EH}Gam.ValGam})
%%]

%%[(40 hmtyinfer) import({%{EH}Ty.Trf.ElimEqual})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Alts elim
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4_2 hmtyinfer).valGamElimAlts export(valGamElimAlts)
valGamElimAlts :: FIOpts -> FIEnv -> TyVarIdL -> UID -> VarMp -> ValGam -> (ValGam,VarMp,ErrGam)
valGamElimAlts opts env globTvL uniq gVarMp g
  =  let  (g',(c,eg,_))
            =  gamMapThr
                  (\(n,vgi) (c,eg,u)
                  	->  let  (u',u1) = mkNewLevUID u
                  	         fo = tyElimAlts (mkFitsInWrap' env) opts globTvL u1 (c `varUpd` gVarMp) (c `varUpd` vgiTy vgi)
                  	         cg = varmpFilterTyAltsMappedBy gVarMp (foVarMp fo)
                  	    in   ((n,vgi {vgiTy = foTy fo}),(foVarMp fo `varUpd` c `varUpd` cg,gamAdd n (foErrL fo) eg,u'))
                  )
                  (emptyVarMp,emptyGam,uniq) (gVarMp `varUpd` g)
     in   (g',tyElimAltsCleanup gVarMp c,eg)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Equal elim
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(40 hmtyinfer).valGamElimEqual export(valGamElimEqual)
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Get used identifiers from types of values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(mentrelFilterMpExtendViaValGam)
-- | Lookup indirectly used identifiers, type constants from types of value bindings.
mentrelFilterMpExtendViaValGam :: HsName -> ValGam -> ModEntRelFilterMp -> ModEntRelFilterMp
mentrelFilterMpExtendViaValGam moduleNm valGam mentrelFilterMp
  = mentrelFilterMpUnions 
      (   [ mentrelFilterMp ]
       ++ [ maybe Map.empty (tyUsedNames moduleNm . vgiTy) $ valGamLookup n valGam
          | n <- Set.toList $ Map.findWithDefault Set.empty IdOcc_Val mentrelFilterMp
          ]
      )
%%]


% $Id$

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gamma utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4 import(List,EHCommon,EHOpts,EHTy,EHTyFitsIn,EHError,EHGam,EHCnstr,EHSubstitutable)
%%]

%%[4_2 import(EHTyElimAlts) export(valGamElimAlts)
%%]

%%[11 import(EHTyElimEqual) export(valGamElimEqual)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Alts elim
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4_2.valGamElimAlts
valGamElimAlts :: FIOpts -> FIEnv -> TyVarIdL -> UID -> Cnstr -> ValGam -> (ValGam,Cnstr,Gam HsName ErrL)
valGamElimAlts opts env globTvL uniq gCnstr g
  =  let  (g',(c,eg,_))
            =  gamMapThr
                  (\(n,vgi) (c,eg,u)
                  	->  let  (u',u1) = mkNewLevUID u
                  	         (t,ce,e) = tyElimAlts (mkElimAltsWrap env) opts globTvL u1 (c |=> vgiTy vgi)
                  	    in   ((n,vgi {vgiTy = t}),(ce |=> c,gamAdd eg n e,u'))
                  )
                  (emptyCnstr,emptyGam,uniq) (gCnstr |=> g)
          c2 = cnstrDelAlphaRename c
          c3 = cnstrKeys c2 `cnstrDel` cnstrFilterAlphaRename gCnstr
     in   (g',c2 |=> c3,eg)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Equal elim
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[11.valGamElimEqual
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


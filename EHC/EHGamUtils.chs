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

%%[4_3 import(EHTyElimAlts) export(valGamElimAlts)
%%]

%%[11 import(EHTyElimEqual) export(valGamElimEqual)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Alts elim
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4_3.valGamElimAlts
valGamElimAlts :: FIEnv -> UID -> ValGam -> (ValGam,Cnstr,Gam HsName ErrL)
valGamElimAlts env uniq g
  =  let  (g',(c,eg,_))
            =  gamMapThr
                  (\(n,vgi) (c,eg,u)
                  	->  let  (u',u1) = mkNewLevUID u
                  	         (t,ce,e) = tyElimAlts (mkElimAltsWrap env) joinFIOpts u1 (c |=> vgiTy vgi)
                  	    in   ((n,vgi {vgiTy = t}),(ce |=> c,gamAdd n e eg,u'))
                  )
                  (emptyCnstr,emptyGam,uniq) g
     in   (g',c,eg)
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


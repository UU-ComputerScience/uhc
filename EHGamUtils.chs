% $Id: EHC.lag 199 2004-05-12 19:11:13Z andres $

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gamma utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[6_2 import(List,EHCommon,EHTy,EHTyFitsIn,EHError,EHGam,EHCnstr,EHSubstitutable)
%%]

%%[6_2 import(EHTyElimBinds) export(valGamElimBinds)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% App of bind elim
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[6_2.valGamElimBinds
valGamElimBinds :: FIEnv -> UID -> ValGam -> (ValGam,Cnstr,Gam HsName ErrL)
valGamElimBinds env uniq g
  =  let  (g',(c,eg,_))
            =  gamMapThr
                  (\(n,vgi) (c,eg,u)
                  	->  let  (u',u1) = mkNewLevUID u
                  	         (t,c',e) = tyElimBinds (mkElimBindsWrap meetFIOpts env) u1 [] (c |=> vgiTy vgi)
                  	    in   ((n,vgi {vgiTy = t}),(c' |=> c,gamAdd n e eg,u'))
                  )
                  (emptyCnstr,emptyGam,uniq) g
     in   (g',c,eg)
%%]

valGamElimBinds :: FIEnv -> ValGam -> (ValGam,Gam HsName ErrL)
valGamElimBinds env g
  =  let  g' = gamMapElts  (\vgi ->  let  (t,c,e) = tyElimBinds env (vgiTy vgi)
                                     in   (vgi {vgiTy = t},e)
                           ) g
     in   gamUnzip g'


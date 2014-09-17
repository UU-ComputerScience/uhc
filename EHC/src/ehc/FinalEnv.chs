%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Environmental info for codegen, finalized w.r.t. ty inference etc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module itf
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs module {%{EH}FinalEnv}
%%]
%%[(8 codegen) hs import ({%{EH}Gam.Base},{%{EH}Gam.TyKiGam})
%%]
%%[(9 codegen) hs import ({%{EH}Gam.ClGam})
%%]
%%[(8888 codegen) hs import(UHC.Util.Utils,{%{EH}Base.HsName.Builtin},{%{EH}Base.Common},{%{EH}Base.TermLike},{%{EH}Opts.Base})
%%]
%%[(8888 codegen) hs import({%{EH}Ty} as T, qualified {%{EH}Core} as C, {%{EH}AbstractCore})
%%]

%%[(8888 codegen) hs import ({%{EH}Gam.Base},{%{EH}Gam.ClGam},{%{EH}Gam.TyKiGam})
%%]

%%[(8888 codegen) hs import (qualified Data.Set as Set)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% System F generation: environmental info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(ToSysfEnv(..),emptyToSysfEnv)
data ToSysfEnv
  = ToSysfEnv
      { sysfenvTyKiGam 		:: TyKiGam
%%[[9
      , sysfenvClGam 		:: ClGam
%%]]
      }

emptyToSysfEnv :: ToSysfEnv
emptyToSysfEnv
  = ToSysfEnv
      emptyGam
%%[[9
      emptyGam
%%]]
%%]


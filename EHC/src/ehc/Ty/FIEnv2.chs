%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FitsIn Environment, part II
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
A variant of emptyFE, with feFIReqs correctly filled in.
This must be done in a separate module because of the module cycles it solves to avoid.
%%]

%%[(4 hmtyinfer) module {%{EH}Ty.FIEnv2} import({%{EH}Ty.FIEnv})
%%]
%%[(4 hmtyinfer) import({%{EH}Ty.AppSpineGam})
%%]

%%[(8 hmtyinfer) import(qualified {%{EH}TyCore.Full1} as C)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Filling in
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer) export(defaultFIEnv)
defaultFIEnv
  =   emptyFE
        { feAppSpineGam = mkAppSpineGam defaultFIEnv
%%[[8
        , feFIReqs
            = FitsInRequires
                { fireqLRCoeForLamTyAppAsSubst	= C.lrcoeForLamTyAppAsSubst
                , fireqCSubstAppExpr			= C.cSubstApp
                , fireqCSubstAppSubst			= C.cSubstApp
%%[[10
                , fireqCoeEvalOnAsSubst 		= C.coeEvalOnAsSubst
                , fireqLRCoeWipeWeaveAsSubst 	= C.lrcoeWipeWeaveAsSubst
%%]]
                }
%%]]
        }
%%]

%%[(4444 hmtyinfer) export(defaultFIIn)
defaultFIIn
  =   emptyFI
%%[[8
        { fiEnv  = defaultFIEnv
        }
%%]]
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utilities for Ty which cannot be placed elsewhere (e.g. because of module cycles)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[98 module {%{EH}Ty.Utils} import({%{EH}Base.Common}, {%{EH}Base.Builtin}, {%{EH}Base.Opts}, {%{EH}Ty}) 
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Top level main type
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[98 hs export(tyTopLevelMain)
tyTopLevelMain :: EHCOpts -> Ty
tyTopLevelMain opts = mk1ConApp (ehbnIO $ ehcOptBuiltinNames opts) (mkProdApp [])
%%]


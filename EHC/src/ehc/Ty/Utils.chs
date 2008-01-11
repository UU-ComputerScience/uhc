%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utilities for Ty which cannot be placed elsewhere (e.g. because of module cycles)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2 module {%{EH}Ty.Utils} import({%{EH}Base.Common}, {%{EH}Substitutable}, {%{EH}VarMp}, {%{EH}Ty}, {%{EH}Ty.Pretty}) 
%%]

%%[2 import(EH.Util.Pretty) 
%%]

%%[98 import({%{EH}Base.Builtin}, {%{EH}Base.Opts}) 
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Top level main type
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2 hs export(ppTyS)
ppTyS :: VarMp -> Ty -> PP_Doc
ppTyS = ppS ppTy
%%]

%%[98 hs export(tyTopLevelMain)
tyTopLevelMain :: EHCOpts -> Ty
tyTopLevelMain opts = mk1ConApp (ehbnIO $ ehcOptBuiltinNames opts) (mkProdApp [])
%%]


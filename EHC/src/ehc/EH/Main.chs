%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Wrapper around MainAG, to be able to add some stuff which otherwise ends up in wrong files when using uuagc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 hs module {%{EH}EH.Main} import({%{EH}EH.MainAG}) export(module {%{EH}EH.MainAG})
%%]

%%[8 hs import(Data.Typeable)
%%]

%%[1 hs
-- dummy
%%]

%%[8 hs
deriving instance Typeable Syn_AGItf
%%]

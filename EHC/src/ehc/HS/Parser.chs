%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}HS.Parser} import(IO, UU.Parsing, UU.Parsing.Offside, UU.Scanner.GenToken, {%{EH}Base.Common}, {%{EH}Base.ScannerCommon}, {%{EH}HS})
%%]

%%[1 export(pHSAGItf)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
type HSParser         ep    =    (IsParser (OffsideParser i o Token p) Token,InputState i Token p, OutputState o, Position p)
                                    => OffsideParser i o Token p ep

pHSAGItf :: HSParser AGItf
pHSAGItf = pSucceed undefined
%%]

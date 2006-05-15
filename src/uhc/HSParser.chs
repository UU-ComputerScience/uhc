% $Id: EHParser.chs 276 2005-08-31 11:54:35Z atze $

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module HSParser import(IO, UU.Parsing, UU.Parsing.Offside, UU.Scanner.GenToken, EHCommon, EHScannerCommon, HSAbsSyn)
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

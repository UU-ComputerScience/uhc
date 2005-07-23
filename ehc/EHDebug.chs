% $Id$

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tracing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 import(Debug.Trace,UU.Pretty) export(tr,trm,trPP)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Debug functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
showPP :: PP a => a -> String
showPP p = disp (pp p) 1000 ""

trm :: String -> (a -> PP_Doc) -> a -> a
trm msg pp x = trace msg (trace (showPP (msg >|< ":" >|< pp x)) x)

tr :: String -> PP_Doc -> a -> a
tr msg p x = trm msg (const p) x

trPP :: PP a => String -> a -> a
trPP msg x = trace msg (trace (showPP (msg >|< ":" >|< x)) x)
%%]

showPP :: PP a => a -> String
showPP p = disp (pp p) 1000 ""

tr :: String -> PP_Doc -> a -> a
tr msg p x = trace (showPP (msg >|< ":" >|< p)) x

trPP :: PP a => String -> a -> a
trPP msg x = trace (showPP (msg >|< ":" >|< x)) x

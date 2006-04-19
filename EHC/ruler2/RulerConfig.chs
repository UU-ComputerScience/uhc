%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module itf
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(cfgKeywordsOpsEsc,cfgKeywordsOpsExplainEsc,cfgStrSel)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Configuration for Ruler and mentioning in documentation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
cfgKeywordsOpsEsc :: [String]
cfgKeywordsOpsEsc =
%%]
%%[1.cfgKeywordsOpsEsc.val
  [ ",", ":", "[", "]", "*", "<", ":=", "::", "->" ]
%%]

%%[1
cfgKeywordsOpsExplainEsc :: [String]
cfgKeywordsOpsExplainEsc =
%%]
%%[1.cfgKeywordsOpsExplainEsc.val
  [ "=", "-", "---" ]
%%]

%%[1
cfgStrSel :: String
cfgStrSel =
%%]
%%[1.cfgStrSel.val
  "."
%%]
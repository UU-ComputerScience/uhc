%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module itf
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(cfgKeywordsOpsEsc,cfgKeywordsOpsExplainEsc,cfgStrSel)
%%]

%%[1 export(cfgFmFmtCmdAsc)
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Names of commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
cfgFmFmtCmdAsc :: [(String,String)]
cfgFmFmtCmdAsc =
%%]
%%[1.cfgFmFmtCmdAsc.val
  [ ( "rulerChngBegMark",           "rulerChngBegMark"          )   -- lhs2tex for begin of change
  , ( "rulerChngEndMark",           "rulerChngEndMark"          )   -- lhs2tex for end of change
  , ( "rulerSameBegMark",           "rulerSameBegMark"          )   -- lhs2tex for begin of no change
  , ( "rulerSameEndMark",           "rulerSameEndMark"          )   -- lhs2tex for end of no change
  , ( "rulerUndefinedExtern",       "rulerUndefinedExtern"      )   -- lhs2tex for extern hole
  , ( "rulerRulesetFigureEnv",      "rulerRulesetFigure"        )   -- latex env for ruleset figure
  , ( "rulerRuleCmd",               "rulerRule"                 )   -- latex cmd for rule
  , ( "rulerCmdUse",                "rulerCmdUse"               )   -- latex cmd for using generated artefact
  , ( "rulerCmdDef",                "rulerCmdDef"               )   -- latex cmd for defining generated artefact
  ]
%%]

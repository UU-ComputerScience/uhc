module Config
( cfgKeywordsOpsEsc, cfgKeywordsOpsExplainEsc, cfgStrSel
, cfgFmFmtCmdAsc )
where

cfgKeywordsOpsEsc :: [String]
cfgKeywordsOpsEsc =
  [ ",", ":", "[", "]", "*", "<", ":=", "::", "->" ]

cfgKeywordsOpsExplainEsc :: [String]
cfgKeywordsOpsExplainEsc =
  [ "=", "-", "---" ]

cfgStrSel :: String
cfgStrSel =
  "."

cfgFmFmtCmdAsc :: [(String,String)]
cfgFmFmtCmdAsc =
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

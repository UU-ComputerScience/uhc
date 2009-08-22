-------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------

%%[1 hs module (Opts)
%%]

%%[1 hs export (Opts(..), defaultOpts, cmdLineOpts)
%%]

%%[1 hs import (System.Console.GetOpt, EH.Util.ParseUtils, Common, ViewSel.ViewSel, ViewSel.Self, ViewSel.Parser)
%%]

%%[1 hs

-------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------

data Opts 
  = Opts
      { optGenFM        :: FmKind
      , optGenExpl      :: Bool
      , optGenV2		:: Bool
      , optGenAGAttr    :: Bool
      , optGenAGData    :: Bool
      , optDot2Dash     :: Bool
      , optPreamble     :: Bool
      , optAtDir        :: AtDir    -- used internally only
      , optSubstFullNm  :: Bool     -- used internally only
      , optSubstOnce    :: Bool     -- used internally only
      , optMatchROpOpnd :: Bool     -- used internally only
      , optHelp         :: Bool
      , optDebug        :: Bool
      , optVersion      :: Bool
      , optSvnVersion   :: Bool
      , optFragWrap     :: Bool
      , optAGCopyElim   :: Bool
      , optMbMarkChange :: Maybe ViewSels
      , optMbRlSel      :: Maybe RlSel
      , optBaseNm       :: String
      , optSearchPath   :: [String]
      , optDefs   		:: [(String,String)]
      }
      deriving Show

defaultOpts
  = Opts
      { optGenFM        =  FmAll
      , optGenExpl      =  False
      , optGenV2        =  False
      , optGenAGAttr    =  False
      , optGenAGData    =  False
      , optDot2Dash     =  False
      , optPreamble     =  True
      , optAtDir        =  AtInOut
      , optSubstFullNm  =  True
      , optSubstOnce    =  False
      , optMatchROpOpnd =  True
      , optHelp         =  False
      , optDebug        =  False
      , optVersion      =  False
      , optSvnVersion   =  False
      , optFragWrap     =  False
      , optAGCopyElim   =  True
      , optMbMarkChange =  Nothing
      , optMbRlSel      =  Nothing
      , optBaseNm       =  rulesCmdPre
      , optSearchPath   =  []
      , optDefs			=  []
      }

cmdLineOpts  
  =  [ Option "a"  ["ag"]               (NoArg oGenAG)
          "generate code for AG, default=no"
     , Option ""   ["ATTR"]             (NoArg oGenAGAttr)
          "generate ATTR defs (for AG), default=no"
     , Option "b"  ["base"]             (ReqArg oBase "<name>")
          "base name, default = 'rules'"
     , Option "c"  ["markchanges"]      (OptArg oMarkCh "<spec>")
          "mark changes between specified views (with --lhs2tex)"
          -- "mark changes between specified views (in combi with --lhs2tex, <spec>=*|<view name>|<view name>-<view name>)"
     , Option ""   ["copyelim"]         (OptArg oCopyElim "yes|no")
          "perform AG copy elimination, default=yes"
     , Option ""   ["DATA"]             (NoArg oGenAGData)
          "generate DATA defs (for AG, HS), default=no"
     , Option "d"  ["debug"]            (NoArg oDebug)
          "output debugging info"
     , Option "D"  ["def"]              (ReqArg oDef "key[=|:]value")
          "define key/value"
     , Option ""   ["dot2dash"]         (NoArg oDot2Dash)
          "change '.' in rule names to '-', default=no"
     , Option ""   ["explain"]          (NoArg oGenExpl)
          "generate explanation (for scheme's), default=no"
     , Option ""   ["help"]             (NoArg oHelp)
          "output this help"
     , Option "h"  ["hs"]               (NoArg oGenHS)
          "generate code for Haskell, default=no (in development)"
     , Option "l"  ["lhs2tex"]          (NoArg oGenLhs2tex)
          "generate code for lhs2tex, default=no"
     , Option "P"  ["path"]             (ReqArg oPath "<path>")
          "search path, default empty"
     , Option ""   ["preamble"]         (OptArg oPreamble "yes|no")
          "include preamble, default=yes"
     , Option "s"  ["selrule"]          (OptArg oRlSel "<spec>")
          "select rules by specifying view(s), ruleset(s) and rule(s)"
          -- "select rules by specifying view(s), ruleset(s) and rule(s), <spec>=(*|<view name>|<view name>-<view name>).(*|<ruleset names>).(*|<rule names>)"
     , Option ""   ["svn-version"]      (NoArg oSvnVersion)
          "print svn version info"
{-
     , Option ""   ["as2"]              (NoArg oGenAS2)
          "generate code for AS2 (under development, internal restructure), default=no"
-}
     , Option ""   ["v1"]               (NoArg oGenV1)
          "current version of whatever is under development, default=yes"
     , Option ""   ["v2"]               (NoArg oGenV2)
          "next version of whatever is under development, default=no"
     , Option ""   ["version"]          (NoArg oVersion)
          "print version info"
     , Option "w"  ["wrapshuffle"]      (NoArg oFragWrap)
          "wrap (AG|explanation) in fragments for processing by shuffle"
     ]
  where  oGenLhs2tex     o =  o {optGenFM = FmTeX}
         oGenAG          o =  o {optGenFM = FmAG}
         oGenHS          o =  o {optGenFM = FmHS}
{-
         oGenAS2         o =  o {optGenFM = FmAS2 (optGenFM o)}
-}
         oGenV2          o =  o {optGenV2 = True}
         oGenV1          o =  o {optGenV2 = False}
         oGenExpl        o =  o {optGenExpl = True}
         oGenAGAttr      o =  o {optGenAGAttr = True}
         oGenAGData      o =  o {optGenAGData = True}
         oDot2Dash       o =  o {optDot2Dash = True}
         oPreamble   ms  o =  yesno (\f o -> o {optPreamble = f}) ms o
         oCopyElim   ms  o =  yesno (\f o -> o {optAGCopyElim = f}) ms o
         oHelp           o =  o {optHelp = True}
         oFragWrap       o =  o {optFragWrap = True}
         oDebug          o =  o {optDebug = True}
         oVersion        o =  o {optVersion = True}
         oSvnVersion     o =  o {optSvnVersion = True}
         oBase       s   o =  o {optBaseNm = s}
         oPath       s   o =  o {optSearchPath = searchPathFromString s}
         oDef        s   o =  case break (\c -> c == ':' || c == '=') s of
                                (s1,(_:s2)) -> o {optDefs = (s1,s2) : optDefs o}
                                _           -> o
         oMarkCh     ms  o =  o {optMbMarkChange = fmap (viewSelsSelfT . fst . parseToResMsgs pViewSels . mkScan "") ms}
         oRlSel      ms  o =  o {optMbRlSel = fmap (rlSelSelfT . fst . parseToResMsgs pRlSel . mkScan "") ms}
         yesno updO  ms  o =  case ms of
                                Just "yes"  -> updO True o
                                Just "no"   -> updO False o
                                _           -> o

%%]

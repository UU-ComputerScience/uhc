-------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------

module Opts
  ( Opts(..), defaultOpts, cmdLineOpts
  )
  where

import System.Console.GetOpt
import ParseUtils
import Common
import ViewSel
import ViewSelSelf
import ViewSelParser

-------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------

data Opts 
  = Opts
      { optGenFM        :: FmKind
      , optGenExpl      :: Bool
      , optGenV2		:: Bool
      , optGenAGAttr    :: Bool
      , optDot2Dash     :: Bool
      , optPreamble     :: Bool
      , optAtDir        :: AtDir    -- used internally only
      , optSubstFullNm  :: Bool     -- used internally only
      , optSubstOnce    :: Bool     -- used internally only
      , optMatchROpOpnd :: Bool     -- used internally only
      , optHelp         :: Bool
      , optDebug        :: Bool
      , optVersion      :: Bool
      , optFragWrap     :: Bool
      , optAGCopyElim   :: Bool
      , optMbMarkChange :: Maybe ViewSels
      , optMbRlSel      :: Maybe RlSel
      , optBaseNm       :: String
      }
      deriving Show

defaultOpts
  = Opts
      { optGenFM        =  FmAll
      , optGenExpl      =  False
      , optGenV2        =  True
      , optGenAGAttr    =  False
      , optDot2Dash     =  False
      , optPreamble     =  True
      , optAtDir        =  AtInOut
      , optSubstFullNm  =  True
      , optSubstOnce    =  False
      , optMatchROpOpnd =  True
      , optHelp         =  False
      , optDebug        =  False
      , optVersion      =  False
      , optFragWrap     =  False
      , optAGCopyElim   =  True
      , optMbMarkChange =  Nothing
      , optMbRlSel      =  Nothing
      , optBaseNm       =  rulesCmdPre
      }

cmdLineOpts  
  =  [ Option ""   ["version"]          (NoArg oVersion)
          "print version info"
     , Option "l"  ["lhs2tex"]          (NoArg oGenLhs2tex)
          "generate code for lhs2tex, default=no"
     , Option "a"  ["ag"]               (NoArg oGenAG)
          "generate code for AG, default=no"
     , Option ""   ["as2"]              (NoArg oGenAS2)
          "generate code for AS2 (under development, internal restructure), default=no"
     , Option ""   ["v2"]               (NoArg oGenV2)
          "20060221 version (i.e. next version under devel), default=yes"
     , Option ""   ["v1"]               (NoArg oGenV1)
          "20060221 old version (i.e. version soon obsolete), default=no"
     , Option ""   ["explain"]          (NoArg oGenExpl)
          "generate explanation (for scheme's), default=no"
     , Option ""   ["ATTR"]             (NoArg oGenAGAttr)
          "generate ATTR defs (for AG), default=no"
     , Option ""   ["preamble"]         (OptArg oPreamble "yes|no")
          "include preamble, default=yes"
     , Option ""   ["copyelim"]         (OptArg oCopyElim "yes|no")
          "perform AG copy elimination, default=yes"
     , Option ""   ["dot2dash"]         (NoArg oDot2Dash)
          "change '.' in rule names to '-', default=no"
     , Option "c"  ["markchanges"]      (OptArg oMarkCh "<spec>")
          "mark changes between specified views (in combi with --lhs2tex, <spec>=*|<view name>|<view name>-<view name>)"
     , Option "s"  ["selrule"]          (OptArg oRlSel "<spec>")
          "select rules by specifying view(s), ruleset(s) and rule(s), <spec>=(*|<view name>|<view name>-<view name>).(*|<ruleset names>).(*|<rule names>)"
     , Option ""   ["help"]             (NoArg oHelp)
          "output this help"
     , Option "w"  ["wrapshuffle"]      (NoArg oFragWrap)
          "wrap (AG|explanation) in fragments for further processing by shuffle"
     , Option "d"  ["debug"]            (NoArg oDebug)
          "output debugging info"
     , Option "b"  ["base"]             (ReqArg oBase "<name>")
          "base name, default = 'rules'"
     ]
  where  oGenLhs2tex     o =  o {optGenFM = FmTeX}
         oGenAG          o =  o {optGenFM = FmAG}
         oGenAS2         o =  o {optGenFM = FmAS2 (optGenFM o)}
         oGenV2          o =  o {optGenV2 = True}
         oGenV1          o =  o {optGenV2 = False}
         oGenExpl        o =  o {optGenExpl = True}
         oGenAGAttr      o =  o {optGenAGAttr = True}
         oDot2Dash       o =  o {optDot2Dash = True}
         oPreamble   ms  o =  yesno (\f o -> o {optPreamble = f}) ms o
         oCopyElim   ms  o =  yesno (\f o -> o {optAGCopyElim = f}) ms o
         oHelp           o =  o {optHelp = True}
         oFragWrap       o =  o {optFragWrap = True}
         oDebug          o =  o {optDebug = True}
         oVersion        o =  o {optVersion = True}
         oBase       s   o =  o {optBaseNm = s}
         oMarkCh     ms  o =  o {optMbMarkChange = fmap (viewSelsSelfT . fst . parseToResMsgs pViewSels . mkScan "") ms}
         oRlSel      ms  o =  o {optMbRlSel = fmap (rlSelSelfT . fst . parseToResMsgs pRlSel . mkScan "") ms}
         yesno updO  ms  o =  case ms of
                                Just "yes"  -> updO True o
                                Just "no"   -> updO False o
                                _           -> o


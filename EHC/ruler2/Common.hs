-------------------------------------------------------------------------
-- Common stuff
-------------------------------------------------------------------------

module Common
  ( module Data.Maybe
  , module Data.Char
  , module UU.Pretty
  , module Nm
  , module DpdGr
  , module FPath
  , module PPUtils
  , module AttrProps
  , versionInfo, versionDist
  , rulesCmdPre
  , Opts(..), defaultOpts, cmdLineOpts
  , ExprIsRw(..)
  , SPos, emptySPos
  , Err(..), ppErrPPL
  , WrKind(..)
  , strVec, strLhs, strLoc
  , nmVec, nmUnk, nmApp, nmWild, nmNone, nmEql, nmComma, nmOParen, nmCParen, nmLhs, nmAny, nmSp1
  , nmUniq
  , nmCmdBegChng, nmCmdEndChng, nmCmdBegSame, nmCmdEndSame
  , nmFunMkUniq
  , FmKind(..)
  , ScKind(..), ScDeriv(..)
  )
  where

import Data.Maybe
import Data.Char
-- import qualified Data.Map as Map
import IO
-- import System.Directory
import System.Console.GetOpt
import UU.Pretty
import UU.Scanner.Position( noPos, Pos, Position(..) )
import FPath
import PPUtils
import ParseUtils
import ParseErrPrettyPrint
import ScanUtils
import ViewSelParser
import Nm
import DpdGr
import AttrProps
import ViewSel
import ViewSelSelf
import ViewSelParser

-------------------------------------------------------------------------
-- Version of program
-------------------------------------------------------------------------

versionSvn      = "$Id: Ruler.ag 301 2006-01-20 14:59:39Z atze $"
versionMajor    = "0"
versionMinor    = "1"
versionQuality  = "alpha"
versionDist     = versionMajor ++ "." ++ versionMinor ++ versionQuality
versionProg     = "ruler"
versionInfo     = versionProg ++ versionDist ++ ", " ++ versionSvn

-------------------------------------------------------------------------
-- Defaults
-------------------------------------------------------------------------

rulesCmdPre = "rules"

-------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------

data Opts 
  = Opts
      { optGenFM        :: FmKind
      , optGenExpl      :: Bool
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

defaultOpts
  = Opts
      { optGenFM        =  FmAll
      , optGenExpl      =  False
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

-------------------------------------------------------------------------
-- Is Expr a complex (non variable expr)?
-------------------------------------------------------------------------

data ExprIsRw
  = ExprIsRw    Nm
  | ExprIsVar   Nm
  | ExprIsOther
  deriving Show

instance PP ExprIsRw where
  pp = pp . show

-------------------------------------------------------------------------
-- Symbol position
-------------------------------------------------------------------------

type SPos = (String,Pos)

emptySPos = ("",noPos)

-------------------------------------------------------------------------
-- Errors
-------------------------------------------------------------------------

data Err
  = Err_UndefNm SPos String String [Nm]
  | Err_NoJdSc  SPos String [Nm]
  | Err_Match   SPos String PP_Doc PP_Doc
  | Err_RlPost  SPos String Nm
  deriving Show

ppErrPPL :: PP a => [a] -> PP_Doc
ppErrPPL = vlist . map pp

instance PP Err where
  pp (Err_UndefNm pos cx knd nmL)
    = ppErr pos ("In" >#< cx >#< knd >|< "(s) are undefined:" >#< ppCommas nmL)
  pp (Err_NoJdSc pos cx nmL)
    = ppErr pos ("In" >#< cx >#< "no (tex) judgement scheme for:" >#< ppCommas nmL)
  pp (Err_Match pos cx given reqd)
    = ppErr pos ("In" >#< cx >#< "could not match"
                 >-< indent 2
                       (    "scheme judgement expr:" >#< reqd
                        >-< "given view expr      :" >#< given
                       )
                )
  pp (Err_RlPost pos cx nm)
    = ppErr pos ("In" >#< cx >#< "conclusion lacks judgement for ruleset's scheme:" >#< pp nm)

-------------------------------------------------------------------------
-- Kind of Expr wrappers (for influencing latex pretty printing, colors)
-------------------------------------------------------------------------

data WrKind
  = WrIsChanged | WrIsSame | WrTop | WrNone
  deriving (Show,Eq,Ord)

instance PP WrKind where
  pp = text . show

-------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------

strVec = "_"
strLhs = "lhs"
strLoc = "loc"

nmVec, nmUnk, nmApp, nmWild, nmNone, nmEql, nmComma, nmOParen, nmCParen, nmLhs, nmAny, nmSp1 :: Nm
nmVec     = Nm strVec
nmLhs     = Nm strLhs
nmWild    = nmVec
nmUnk     = Nm "??"
nmAny     = Nm "*"
nmSp1     = Nm "^"
nmEql     = Nm "="
nmApp     = Nm "$"
nmNone    = Nm ""
nmComma   = Nm ","
nmOParen  = Nm "("
nmCParen  = Nm ")"

nmUniq :: Int -> Nm
nmUniq u  = Nm ("uniq" ++ (if u > 0 then show u else ""))

nmCmdBegChng, nmCmdEndChng, nmCmdBegSame, nmCmdEndSame :: Nm
nmCmdBegChng = Nm "rulerChngBegMark"
nmCmdEndChng = Nm "rulerChngEndMark"
nmCmdBegSame = Nm "rulerSameBegMark"
nmCmdEndSame = Nm "rulerSameEndMark"

nmFunMkUniq :: Int -> Nm
nmFunMkUniq u = Nm ("rulerMk" ++ show u ++ "Uniq")

-------------------------------------------------------------------------
-- Format kinds
-------------------------------------------------------------------------

data FmKind
  = FmTeX | FmAG | FmSpec | FmAll | FmCnstr
  deriving (Show,Eq,Ord)

instance PP FmKind where
  pp = pp . show

-------------------------------------------------------------------------
-- Kind of scheme
-------------------------------------------------------------------------

data ScKind
  = ScJudge | ScRelation
  deriving (Show,Eq,Ord)

instance PP ScKind where
  pp = text . show

-------------------------------------------------------------------------
-- Derived scheme
-------------------------------------------------------------------------

data ScDeriv
  = ScList Nm
  deriving (Show,Eq,Ord)

instance PP ScDeriv where
  pp (ScList n) = pp_brackets (pp n)


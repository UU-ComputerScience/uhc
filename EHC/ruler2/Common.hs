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
  , module RulerScanner
  
  , rulesCmdPre
  , ExprIsRw(..)
  , Err(..), mkPPErr, ppErrPPL, errLIsFatal
  , WrKind(..)
  , strVec, strLhs, strLoc
  , nmVec, nmUnk, nmApp, nmWild, nmNone, nmEql, nmComma, nmOParen, nmCParen, nmLhs, nmAny, nmSp1
  , nmUniq
  , nmCmdBegChng, nmCmdEndChng, nmCmdBegSame, nmCmdEndSame
  , nmFunMkUniq
  , FmKind(..), fmAS2Fm
  , ScKind(..), ScDeriv(..)
  )
  where

import Data.Maybe
import Data.Char
import IO
import UU.Pretty
-- import UU.Scanner.Position( noPos, Pos, Position(..) )
import RulerScanner( SPos, emptySPos )
import FPath
import PPUtils
import ParseErrPrettyPrint
import ScanUtils
import Nm
import DpdGr
import AttrProps

-------------------------------------------------------------------------
-- Defaults
-------------------------------------------------------------------------

rulesCmdPre = "rules"

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
-- PP instances
-------------------------------------------------------------------------

instance (PP a,PP b) => PP (a,b) where
  pp (a,b) = pp a >#< ":" >#< pp b

-------------------------------------------------------------------------
-- Errors
-------------------------------------------------------------------------

data Err
  = Err_UndefNm      SPos String String [Nm]
  | Err_NoJdSc       SPos String [Nm]
  | Err_Match        SPos String PP_Doc PP_Doc
  | Err_RlPost       SPos String Nm
  | Err_NotAEqnForm  SPos PP_Doc
  | Err_PP                PP_Doc
  deriving Show

ppErrPPL :: PP a => [a] -> PP_Doc
ppErrPPL = vlist . map pp

mkPPErr :: PP a => a -> Err
mkPPErr = Err_PP . pp

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
  pp (Err_NotAEqnForm pos e)
    = ppWarn pos ("expr not of (AG rule) form ... = ...:" >#< e)
  pp (Err_PP e)
    = e

errIsFatal :: Err -> Bool
errIsFatal (Err_NotAEqnForm _ _) = False
errIsFatal _                     = True

errLIsFatal :: [Err] -> Bool
errLIsFatal es = not (null es) && any errIsFatal es

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
  = FmTeX | FmAG | FmSpec | FmAll | FmCnstr | FmAS2 FmKind
  deriving (Show,Eq,Ord)

fmAS2Fm :: FmKind -> FmKind
fmAS2Fm (FmAS2 f) = f
fmAS2Fm f         = f

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


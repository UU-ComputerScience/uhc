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
  -- , Err(..), mkPPErr, ppErrPPL, errLIsFatal
  , WrKind(..)
  , strVec, strUnd, strLhs, strLoc
  , nmVec, nmUnk, nmApp, nmWild, nmNone, nmList
  , nmEql, nmComma, nmOParen, nmCParen, nmLhs, nmAny, nmSp1
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

strUnd = "_"
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
nmList    = Nm "[]"
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
  = FmTeX | FmAG | FmHS | FmSpec | FmAll | FmCnstr | FmAS2 FmKind
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


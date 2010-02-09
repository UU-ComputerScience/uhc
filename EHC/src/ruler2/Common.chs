-------------------------------------------------------------------------
-- Common stuff
-------------------------------------------------------------------------

%%[1 hs module (Common)
%%]

%%[1 hs export (module Data.Maybe)
%%]

%%[1 hs export (module Data.Char)
%%]

%%[1 hs export (module EH.Util.Nm)
%%]

%%[1 hs export (module EH.Util.DependencyGraph)
%%]

%%[1 hs export (module EH.Util.FPath)
%%]

%%[1 hs export (module AttrProps)
%%]

%%[1 hs export (module Scanner)
%%]

%%[1 hs export (rulesCmdPre, ExprIsRw(..), WrKind(..))
%%]

%%[1 hs export (strOverl, strOverVec, strUnd, strLhs, strLoc)
%%]

%%[1 hs export (nmOverl, nmOverVec, nmUnk, nmApp, nmWild, nmNone, nmList)
%%]

%%[1 hs export (nmEql, nmComma, nmOParen, nmCParen, nmLhs, nmAny, nmSp1)
%%]

%%[1 hs export (nmUniq, nmCmdBegChng, nmCmdEndChng, nmCmdBegSame, nmCmdEndSame)
%%]

%%[1 hs export (nmFunMkUniq, nmIsOver, FmKind(..), fmAS2Fm, ScKind(..), ScDeriv(..))
%%]

%%[1 hs import (Data.Maybe, Data.Char, IO, EH.Util.Pretty)
%%]

%%[1 hs import (Scanner( SPos, emptySPos ), EH.Util.FPath)
%%]

%%[1 hs import (EH.Util.ParseErrPrettyPrint, EH.Util.ScanUtils, EH.Util.Nm)
%%]

%%[1 hs import (EH.Util.DependencyGraph, AttrProps)
%%]

%%[1 hs

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

strUnd   	= "_"
strOverl 	= "_"
strOverVec  = ">"
strLhs   	= "lhs"
strLoc   	= "loc"

nmOverl, nmOverVec, nmUnk, nmApp, nmWild, nmNone, nmEql, nmComma, nmOParen, nmCParen, nmLhs, nmAny, nmSp1 :: Nm
nmOverl   = Nm strOverl
nmOverVec = Nm strOverVec
nmLhs     = Nm strLhs
nmWild    = Nm strUnd
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

nmIsOver :: Nm -> Bool
nmIsOver n = nmOverl == n || nmOverVec == n

-------------------------------------------------------------------------
-- Format kinds
-------------------------------------------------------------------------

data FmKind
  = FmTeX | FmFmtCmd | FmAG | FmHS | FmSpec | FmAll | FmCnstr | FmAS2 FmKind
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
  pp (ScList n) = ppBrackets (pp n)

%%]

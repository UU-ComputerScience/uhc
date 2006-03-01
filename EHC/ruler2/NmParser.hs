-------------------------------------------------------------------------
-- Nm parser
-------------------------------------------------------------------------

module NmParser
  ( NmSPos
  , pNmDotSym
  , pNmSym, pNmSymSPos
  , pNmVw, pNmVwSPos
  , pNm, pNmSPos
  )
  where

import UU.Parsing
import RulerScanner
import ParseUtils
import Nm
import SelParser
import KeywParser

-------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------

type NmSPos = (Nm,SPos)

pNmDotSymSPos :: (IsParser p Token) => p String -> p (NmSPos -> NmSPos)
pNmDotSymSPos pSymStr
  = pSel (id,(\(n1,p) n2 -> (n1 `NmSel` n2,p)),Just) (pN,pMb pN)
  where pN = pNmStrI <|> pSymStr

pNmDotSym :: (IsParser p Token) => p String -> p (Nm -> Nm)
pNmDotSym pSymStr
  = pSel (id,NmSel,Just) (pN,pMb pN)
  where pN = pNmStrI <|> pSymStr

pNmSymSPos :: (IsParser p Token) => (p SPos,p String) -> p NmSPos
pNmSymSPos (pN,pS) = ((\sp@(s,p) -> (Nm s,sp)) <$> pN) <**> pNmDotSymSPos pS

pNmSym :: (IsParser p Token) => (p String,p String) -> p Nm
pNmSym (pN,pS) = (Nm <$> pN) <**> pNmDotSym pS

pNmVwSPos :: (IsParser p Token) => p NmSPos
pNmVwSPos = pNmSymSPos (pNmStrISPos,pSymStr)

pNmVw :: (IsParser p Token) => p Nm
pNmVw = fst <$> pNmVwSPos

pNmSPos :: (IsParser p Token) => p NmSPos
pNmSPos = pNmSymSPos (pNmStrSPos,pSymStr)

pNm :: (IsParser p Token) => p Nm
pNm = fst <$> pNmSPos

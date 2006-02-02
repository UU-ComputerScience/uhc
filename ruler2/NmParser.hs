-------------------------------------------------------------------------
-- Nm parser
-------------------------------------------------------------------------

module NmParser
  ( pNmDotSym, pNmSym
  , pNmVw, pNm
  )
  where

import UU.Parsing
import UU.Scanner
import ParseUtils
import Nm
import SelParser
import KeywParser

-------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------

pNmDotSym :: (IsParser p Token) => p String -> p (Nm -> Nm)
pNmDotSym pSymStr
  = pSel (id,NmSel,Just) (pN,pMb pN)
  where pN = pNmStrI <|> pSymStr

pNmSym :: (IsParser p Token) => (p String,p String) -> p Nm
pNmSym (pN,pS) = (Nm <$> pN) <**> pNmDotSym pS

pNmVw :: (IsParser p Token) => p Nm
pNmVw = pNmSym (pNmStrI,pSymStr)

pNm :: (IsParser p Token) => p Nm
pNm = pNmSym (pNmStr,pSymStr)

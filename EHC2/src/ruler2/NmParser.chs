-------------------------------------------------------------------------
-- Nm parser
-------------------------------------------------------------------------

%%[1 hs module (NmParser)
%%]

%%[1 hs export (NmSPos, pNmDotSym, pNmSym, pNmDotted, pNmDottedSPos, pNmVw, pNmVwSPos, pNm, pNmSPos)
%%]

%%[1 hs import (UU.Parsing, Scanner, EH.Util.ParseUtils, EH.Util.Nm, SelParser, KeywParser)
%%]

-------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------

%%[1 hs
type NmSPos = (Nm,SPos)

pNmDotSymSPos :: (IsParser p Token) => p String -> p (NmSPos -> NmSPos)
pNmDotSymSPos pSymStr
  = pSel (id,(\(n1,p) n2 -> (n1 `NmSel` n2,p)),Just) (pN,pMb pN)
  where pN = pNmStrI <|> pSymStr

pNmDotSym :: (IsParser p Token) => p String -> p (Nm -> Nm)
pNmDotSym pSymStr
  = pSel (id,NmSel,Just) (pN,pMb pN)
  where pN = pNmStrI <|> pSymStr

pNmBaseSelSPos :: (IsParser p Token) => (p SPos,p String) -> p NmSPos
pNmBaseSelSPos (pN,pS) = ((\sp@(s,p) -> (Nm s,sp)) <$> pN) <**> pNmDotSymSPos pS

pNmSym :: (IsParser p Token) => (p String,p String) -> p Nm
pNmSym (pN,pS) = (Nm <$> pN) <**> pNmDotSym pS

pNmVwSPos :: (IsParser p Token) => p NmSPos
pNmVwSPos = pNmBaseSelSPos (pNmStrISPos,pSymStr)

pNmVw :: (IsParser p Token) => p Nm
pNmVw = fst <$> pNmVwSPos

pNmDottedSPos :: (IsParser p Token) => p String -> p NmSPos
pNmDottedSPos pSymStr = pNmBaseSelSPos (pNmStrSPos,pSymStr)

pNmDotted :: (IsParser p Token) => p String -> p Nm
pNmDotted pSymStr = fst <$> pNmDottedSPos pSymStr

pNmSPos :: (IsParser p Token) => p NmSPos
pNmSPos = pNmBaseSelSPos (pNmStrSPos,pSymStr)

pNm :: (IsParser p Token) => p Nm
pNm = fst <$> pNmSPos
%%]

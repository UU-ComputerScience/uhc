%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Basic/shared parsers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{EH}Base.Parser} import(UU.Parsing, EH.Util.ParseUtils(PlainParser), EH.Util.ScanUtils, {%{EH}Base.Common}, {%{EH}Scanner.Common}, {%{EH}Scanner.Scanner})
%%]

%%[8 export(pDollNm,pUID,pInt,pCTag)
%%]

%%[12 export(pPredOccId)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parsers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
type P p = PlainParser Token p

pDollNm :: P HsName
pDollNm = tokMkQName <$> pVaridTk

-- counterpart of ppUID'
pUID :: P UID
pUID = UID <$ pOCURLY <*> pList1Sep pCOMMA pInt <* pCCURLY

pInt :: P Int
pInt = tokMkInt <$> pInteger10Tk

%%]


-- counterpart of ppCTag'
%%[8
pCTag :: P CTag
pCTag
  = pCurly (   CTag <$> pDollNm <* pCOMMA <*> pDollNm <* pCOMMA <*> pInt <* pCOMMA <*> pInt
           <|> CTagRec <$ pKeyTk "Rec"
           )
%%]

-- counterpart of ppPredOccId'
%%[12
pPredOccId :: P PredOccId
pPredOccId
  = PredOccId <$ pOCURLY <*> pUID <* pCOMMA <*> pUID <* pCCURLY
%%]

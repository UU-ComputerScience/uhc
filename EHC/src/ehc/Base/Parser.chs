%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Basic/shared parsers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{EH}Base.Parser} import(UU.Parsing, UHC.Util.ParseUtils, UHC.Util.ScanUtils, {%{EH}Base.Builtin},{%{EH}Base.Common}, {%{EH}Scanner.Common}, {%{EH}Scanner.Scanner}, {%{EH}Base.ParseUtils})
%%]

%%[8 import({%{EH}Base.ParseUtils}) export(module {%{EH}Base.ParseUtils})
%%]

%%[50 import(qualified Data.Set as Set,qualified UHC.Util.Rel as Rel)
%%]

%%[(5020 hmtyinfer) import(qualified {%{EH}Pred} as Pr)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parsers for concrete structures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(pDollNm,pUID,pInt)

pDollNm :: P HsName
pDollNm
  = tokMkQName
    <$> (   pVaridTk  <|> pConidTk
%%[[50
        <|> pQVaridTk <|> pQConidTk
%%]]
        )

-- counterpart of ppUIDParseable
pUID :: P UID
-- pUID = mkUID <$ pOCURLY <*> pList1Sep pCOMMA pInt <* pCCURLY
pUID = mkUID <$ pPERCENT <*> pList1Sep pCOMMA pInt <* pPERCENT

pInt :: P Int
pInt = tokMkInt <$> pInteger10Tk

%%]

%%[50 export(pUIDHI)
pUIDHI :: P UID
pUIDHI = pKeyTk "uid" *> pUID
%%]

%%[8 export(pCTag, pCTagExtensive)
-- | counterpart of ppCTag'
pCTag :: P CTag
pCTag
  = pCurly (   (\tyNm conNm tg -> CTag tyNm conNm tg (-1) (-1))
               <$> pDollNm <* pCOMMA <*> pDollNm <* pCOMMA <*> pInt
           <|> CTagRec <$ pKeyTk "Rec"
           )
-- | counterpart of pCTagExtensive'
pCTagExtensive :: P CTag
pCTagExtensive
  = pCurly (   CTag <$> pDollNm <* pCOMMA <*> pDollNm <* pCOMMA <*> pInt <* pCOMMA <*> pInt <* pCOMMA <*> pInt
           <|> CTagRec <$ pKeyTk "Rec"
           )
%%]

%%[8 export(pBool)
pBool :: P Bool
pBool = True <$ pKeyTk "True" <|> False <$ pKeyTk "False"
%%]

-- counterpart of ppPredOccId'
%%[50 export(pPredOccId)
pPredOccId :: P PredOccId
pPredOccId
  = mkPrId <$> pUIDHI
%%]

%%[50 export(pIdOcc,pIdOccKind)
-- counterpart of PP IdOccKind instance
pIdOccKind :: P IdOccKind
pIdOccKind
  =   IdOcc_Val     <$ pKeyTk "Value"
  <|> IdOcc_Pat     <$ pKeyTk "Pat"
  <|> IdOcc_Type    <$ pKeyTk "Type"
  <|> IdOcc_Kind    <$ pKeyTk "Kind"
  <|> IdOcc_Class   <$ pKeyTk "Class"
  <|> IdOcc_Inst    <$ pKeyTk "Instance"
  <|> IdOcc_Dflt    <$ pKeyTk "Default"
  <|> IdOcc_Data    <$ pKeyTk "Data"
  <|> IdOcc_Any     <$ pKeyTk "Any"

pIdOcc :: P IdOcc
pIdOcc = IdOcc <$ pOCURLY <*> pDollNm <* pCOMMA <*> pIdOccKind <* pCCURLY
%%]

%%[50 export(pAssocL)
pAssocL :: P a -> P b -> P (AssocL a b)
pAssocL pA pB = pOCURLY *> pListSep pCOMMA ((,) <$> pA <* pEQUAL <*> pB) <* pCCURLY
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser abstractions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(pCurlySemiBlock,pCurlys,pSemiBlock,pCurlyCommaBlock)
pSemiBlock :: P p -> P [p]
pSemiBlock p = pListSep pSEMI p

pCurlys :: P p -> P p
pCurlys p = pOCURLY *> p <* pCCURLY

pCurlySemiBlock :: P p -> P [p]
pCurlySemiBlock p = pCurlys (pListSep pSEMI p)

pCurlyCommaBlock :: P p -> P [p]
pCurlyCommaBlock p = pCurlys (pListSep pCOMMA p)
%%]


%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Basic/shared parsers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{EH}Base.Parser} import(UU.Parsing, EH.Util.ParseUtils, EH.Util.ScanUtils, {%{EH}Base.Builtin},{%{EH}Base.Common}, {%{EH}Scanner.Common}, {%{EH}Scanner.Scanner}, {%{EH}Base.ParseUtils})
%%]

%%[8 import({%{EH}Base.ParseUtils}) export(module {%{EH}Base.ParseUtils})
%%]

%%[20 import({%{EH}Module},qualified Data.Set as Set,qualified EH.Util.Rel as Rel)
%%]

%%[(20 hmtyinfer) import(qualified {%{EH}Pred} as Pr)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parsers for concrete structures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(pDollNm,pUID,pInt)

pDollNm :: P HsName
pDollNm = tokMkQName <$> pVaridTk

-- counterpart of ppUID'
pUID :: P UID
pUID = mkUID <$ pOCURLY <*> pList1Sep pCOMMA pInt <* pCCURLY

pInt :: P Int
pInt = tokMkInt <$> pInteger10Tk

%%]

%%[20 export(pUIDHI)
pUIDHI :: P UID
pUIDHI = pKeyTk "uid" *> pUID
%%]

-- counterpart of ppCTag'
%%[8 export(pCTag)
pCTag :: P CTag
pCTag
  = pCurly (   CTag <$> pDollNm <* pCOMMA <*> pDollNm <* pCOMMA <*> pInt <* pCOMMA <*> pInt <* pCOMMA <*> pInt
           <|> CTagRec <$ pKeyTk "Rec"
           )
%%]

%%[8 export(pBool)
pBool :: P Bool
pBool = True <$ pKeyTk "True" <|> False <$ pKeyTk "False"
%%]

-- counterpart of ppPredOccId'
%%[20 export(pPredOccId)
pPredOccId :: P PredOccId
pPredOccId
  = mkPrId <$> pUIDHI
%%]

%%[20 export(pIdOcc)
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

%%[20
pAssocL :: P a -> P b -> P (AssocL a b)
pAssocL pA pB = pOCURLY *> pListSep pCOMMA ((,) <$> pA <* pEQUAL <*> pB) <* pCCURLY
%%]

%%[20 export(pModEntRel)
pModEnt :: P ModEnt
pModEnt
  = (\kind occ owns -> ModEnt kind occ owns emptyRange)
    <$  pOCURLY <*> pIdOccKind <* pCOMMA <*> pIdOcc
    <*> pMaybe Set.empty id (Set.fromList <$ pCOMMA <* pOCURLY <*> pListSep pCOMMA pModEnt <* pCCURLY)
    <*  pCCURLY

pModEntRel :: P ModEntRel
pModEntRel
  = Rel.fromList <$> pAssocL pDollNm pModEnt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser abstractions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 export(pCurlySemiBlock,pCurlys,pSemiBlock,pCurlyCommaBlock)
pSemiBlock :: P p -> P [p]
pSemiBlock p = pListSep pSEMI p

pCurlys :: P p -> P p
pCurlys p = pOCURLY *> p <* pCCURLY

pCurlySemiBlock :: P p -> P [p]
pCurlySemiBlock p = pCurlys (pListSep pSEMI p)

pCurlyCommaBlock :: P p -> P [p]
pCurlyCommaBlock p = pCurlys (pListSep pCOMMA p)
%%]


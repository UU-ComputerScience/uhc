%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Basic/shared parsers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{EH}Base.Parser} import(UU.Parsing, UHC.Util.Utils, UHC.Util.ParseUtils, UHC.Util.ScanUtils, {%{EH}Base.HsName.Builtin},{%{EH}Base.Common}, {%{EH}Scanner.Common}, {%{EH}Scanner.Scanner}, {%{EH}Base.ParseUtils})
%%]

%%[8 import({%{EH}Base.ParseUtils}) export(module {%{EH}Base.ParseUtils})
%%]

%%[8 import({%{EH}Error}, {%{EH}Error.Pretty})
%%]

%%[8 import(qualified Data.Map as Map,qualified Data.Set as Set,qualified UHC.Util.Rel as Rel, Data.List)
%%]

%%[(5020 hmtyinfer) import(qualified {%{EH}Pred} as Pr)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parsers for concrete structures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(pDollNm,pUID,pInt)
pDollNm :: P HsName
pDollNm
  = (parseHsName . tokenVals) -- tokMkQName
    <$> (   pVaridTk  <|> pConidTk
%%[[50
        <|> pQVaridTk <|> pQConidTk
%%]]
        )

-- counterpart of ppUIDParseable/showUIDParseable
pUID :: P UID
-- pUID = mkUID <$ pOCURLY <*> pList1Sep pCOMMA pInt <* pCCURLY
pUID = mkUID <$ pBACKQUOTE <* pOCURLY <*> pList1Sep pCOMMA pInt <* pCCURLY
-- pUID = mkUID <$ pPERCENT <* pOBRACK <*> pList1Sep pSLASH pInt <* pCBRACK

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
%%% Parsers for HsName, in particular to its internal structure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
pHsNameUnique :: P HsNameUnique
pHsNameUnique
  =   HsNameUnique_Int    <$> pInt
  <|> HsNameUnique_UID    <$> pUID
  <|> HsNameUnique_String <$> pStr

pHsNameUniqifier :: P HsNameUniqifier
pHsNameUniqifier
  =   HsNameUniqifier_New                  <$ pKeyTk "NEW"
  <|> HsNameUniqifier_Error                <$ pKeyTk "ERR"
  <|> HsNameUniqifier_GloballyUnique       <$ pKeyTk "UNQ"
  <|> HsNameUniqifier_Evaluated            <$ pKeyTk "EVL"
  <|> HsNameUniqifier_Field                <$ pKeyTk "FLD"
  <|> HsNameUniqifier_Class                <$ pKeyTk "CLS"
  <|> HsNameUniqifier_ClassDict            <$ pKeyTk "DCT"
  <|> HsNameUniqifier_SelfDict             <$ pKeyTk "SDC"
  <|> HsNameUniqifier_ResultDict           <$ pKeyTk "RDC"
  <|> HsNameUniqifier_SuperClass           <$ pKeyTk "SUP"
  <|> HsNameUniqifier_DictField            <$ pKeyTk "DFL"
  <|> HsNameUniqifier_Inline               <$ pKeyTk "INL"
  <|> HsNameUniqifier_GloballyUniqueDict   <$ pKeyTk "UND"
  <|> HsNameUniqifier_FieldOffset          <$ pKeyTk "OFF"
  <|> HsNameUniqifier_CaseContinuation     <$ pKeyTk "CCN"
  <|> HsNameUniqifier_GrinUpdated          <$ pKeyTk "UPD"
  <|> HsNameUniqifier_FFIArg               <$ pKeyTk "FFI"
  <|> HsNameUniqifier_LacksLabel           <$ pKeyTk "LBL"
  <|> HsNameUniqifier_BindAspect           <$ pKeyTk "ASP"
  <|> HsNameUniqifier_Strict               <$ pKeyTk "STR"
%%[[91 
  <|> HsNameUniqifier_GenericClass         <$ pKeyTk "GEN"
%%]] 
%%[[(8 javascript) 
  <|> HsNameUniqifier_JSSwitchResult       <$ pKeyTk "JSW"
%%]] 
%%[[(8 grin) 
  <|> HsNameUniqifier_GRINTmpVar       	   <$ pKeyTk "GRN"
%%]] 
%%[[(8 cmm) 
  <|> HsNameUniqifier_CMMTmpVar       	   <$ pKeyTk "CMM"
%%]] 
%%[[90
  <|> HsNameUniqifier_FFE                  <$ pKeyTk "FFE"
  <|> HsNameUniqifier_FFECoerced           <$ pKeyTk "FFC"
%%]]

pHsNameUniqifierMp :: P HsNameUniqifierMp
pHsNameUniqifierMp
  = Map.fromList <$> pList ((pSep *> pHsNameUniqifier) <+> (pOCURLY *> pList (pSep *> pHsNameUnique)) <* pCCURLY)
  where pSep = tokConcat <$> pBACKQUOTE <*> pBACKQUOTE -- pAT

pHsName :: P HsName
pHsName
  = (\qs (b,u) -> hsnMkModf qs b u) <$> pList_ng (pHsName_Qual <* pDOT) <*> pHsName_Base

pHsName_Qual :: P String
pHsName_Qual
  = tokMkStr  <$> (pVaridTk <|> pConidTk <|> pVarsymTk <|> pConsymTk <|> pK)
  where pK =   pAnyKey pKeyTk $ Set.toList $ scoKeywordsTxt hsnScanOpts

pHsName_Base :: P (HsName, HsNameUniqifierMp)
pHsName_Base
  = pB <+> pHsNameUniqifierMp
  where pB =   mkHNmBase . concat <$> pList1 pHsName_Qual
           <|> mkHNm              <$> pUID
           <|> tokMkQName         <$> pDOT
%%]

%%[8 export(parseHsName)
parseHsName :: [String] -> HsName
parseHsName ss
  = p $ concat $ intersperse "." ss
  where p s = case parseToResMsgs pHsName $ scan hsnScanOpts (initPos s) s of
          (res,[]) -> res
          (res,ms) -> hsnUniqifyStr HsNameUniqifier_Error (show ms) res  
%%]
parseHsName ss
  = case initlast ss of
      Just ([],b) -> case initlast $ splitForQualified b of
                       Just (qs,b) -> mk qs b
                       _           -> hsnUnknown
      Just (qs,b) -> mk qs b
      _           -> hsnUnknown
  where prs p s = case parseToResMsgs p $ scan hsnScanOpts (initPos s) s of
          (res,[]) -> (res, id)
          (res,ms) -> (res, hsnUniqifyStr HsNameUniqifier_Error (show ms))
        mk qs b = foldr (.) be qse $ hsnMkModf qs' b' u'
          where (qs'    ,qse) = unzip $ map (prs pHsName_Qual) qs
                ((b',u'),be ) = prs pHsName_Base b
          

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser abstractions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(pStr)
pStr :: P String
pStr = tokMkStr <$> pStringTk
%%]

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


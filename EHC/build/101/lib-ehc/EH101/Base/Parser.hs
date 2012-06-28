module EH101.Base.Parser
( module EH101.Base.ParseUtils
, pDollNm, pUID, pInt
, pCTag
, pBool
, pUIDHI
, pPredOccId
, pIdOcc, pIdOccKind
, pAssocL
, pCurlySemiBlock, pCurlys, pSemiBlock, pCurlyCommaBlock )
where
import UU.Parsing
import EH.Util.ParseUtils
import EH.Util.ScanUtils
import EH101.Base.Builtin
import EH101.Base.Common
import EH101.Scanner.Common
import EH101.Scanner.Scanner
import EH101.Base.ParseUtils
import EH101.Base.ParseUtils
import qualified Data.Set as Set
import qualified EH.Util.Rel as Rel



{-# LINE 27 "src/ehc/Base/Parser.chs" #-}

pDollNm :: P HsName
pDollNm = tokMkQName <$> pVaridTk

-- counterpart of ppUID'
pUID :: P UID
pUID = mkUID <$ pOCURLY <*> pList1Sep pCOMMA pInt <* pCCURLY

pInt :: P Int
pInt = tokMkInt <$> pInteger10Tk


{-# LINE 41 "src/ehc/Base/Parser.chs" #-}
pUIDHI :: P UID
pUIDHI = pKeyTk "uid" *> pUID

{-# LINE 47 "src/ehc/Base/Parser.chs" #-}
pCTag :: P CTag
pCTag
  = pCurly (   CTag <$> pDollNm <* pCOMMA <*> pDollNm <* pCOMMA <*> pInt <* pCOMMA <*> pInt <* pCOMMA <*> pInt
           <|> CTagRec <$ pKeyTk "Rec"
           )

{-# LINE 55 "src/ehc/Base/Parser.chs" #-}
pBool :: P Bool
pBool = True <$ pKeyTk "True" <|> False <$ pKeyTk "False"

{-# LINE 61 "src/ehc/Base/Parser.chs" #-}
pPredOccId :: P PredOccId
pPredOccId
  = mkPrId <$> pUIDHI

{-# LINE 67 "src/ehc/Base/Parser.chs" #-}
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

{-# LINE 85 "src/ehc/Base/Parser.chs" #-}
pAssocL :: P a -> P b -> P (AssocL a b)
pAssocL pA pB = pOCURLY *> pListSep pCOMMA ((,) <$> pA <* pEQUAL <*> pB) <* pCCURLY

{-# LINE 94 "src/ehc/Base/Parser.chs" #-}
pSemiBlock :: P p -> P [p]
pSemiBlock p = pListSep pSEMI p

pCurlys :: P p -> P p
pCurlys p = pOCURLY *> p <* pCCURLY

pCurlySemiBlock :: P p -> P [p]
pCurlySemiBlock p = pCurlys (pListSep pSEMI p)

pCurlyCommaBlock :: P p -> P [p]
pCurlyCommaBlock p = pCurlys (pListSep pCOMMA p)


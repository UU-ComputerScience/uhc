%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% HI parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12 module {%{EH}HI.Parser} import(UU.Parsing, EH.Util.ParseUtils(PlainParser), EH.Util.ScanUtils, {%{EH}Base.Common}, qualified {%{EH}Pred} as Pr, {%{EH}Scanner.Common}, {%{EH}Scanner.Scanner}, {%{EH}Base.Parser}, {%{EH}Ty}, {%{EH}HI}) export(pAGItf)
%%]

%%[12 import({%{EH}HS.Parser}(pFixity),{%{EH}Core.Parser}(pCExpr),{%{EH}Ty.Parser})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12
type HIParser       hp     =    PlainParser Token hp

pNmIs :: String -> HIParser HsName
pNmIs k = pKeyTk k *> pDollNm <* pEQUAL

pAGItf :: HIParser AGItf
pAGItf
  = AGItf_AGItf <$> pModule

pModule :: HIParser Module
pModule
  = Module_Module <$ pMODULE <*> pDollNm <* pEQUAL <* pOCURLY <*> pListSep pSEMI pBinding <* pCCURLY

pBinding :: HIParser Binding
pBinding
  =   Binding_Fixity    <$> pNmIs "fixity" <* pOCURLY <*> pInt    <* pSEMI <*> pFixity  <* pCCURLY
  <|> Binding_Ids       <$  pNmIs "iddef"  <* pOCURLY
                                           <*> pListSep pSEMI ((,) <$ pOCURLY <*> pIdOcc <* pSEMI <*> pIdOcc <* pCCURLY)
                                           <* pCCURLY
  <|> Binding_Val       <$> pNmIs "value"  <* pOCURLY <*> pTy                           <* pCCURLY
  <|> Binding_Stamp     <$  pNmIs "stamp"  <* pOCURLY <*> pString <* pSEMI <*> pString
                                           <* pSEMI   <*> pString <* pSEMI <*> pString
                                           <* pSEMI   <*> pString <* pSEMI <*> pString
                                           <* pSEMI   <*> (read <$> pInteger)
                                           <* pCCURLY
  <|> Binding_Export    <$  pNmIs "export"            <*> pModEntRel
  <|> Binding_Ty        <$> pNmIs "type"   <* pOCURLY <*> pTy      <* pSEMI <*> pTy     <* pCCURLY
  <|> (\tn cs -> Binding_DataCon tn (map (\f -> f tn) cs))
      <$> pNmIs "data" <*  pOCURLY
                       <*> pCurlySemiBlock
                             ((\n t a fm tn -> (n,(CTag tn n t a,fm)))
                              <$> pDollNm <*  pEQUAL
                              <*  pOCURLY <*> pInt <* pCOMMA <*> pInt
                                          <*> pList ((,) <$ pSEMI <*> pDollNm <* pEQUAL <*> pInt)
                              <*  pCCURLY
                             )
                       <*  pSEMI <*> pBool
                       <*  pCCURLY
  <|> Binding_Class     <$> pNmIs "class"       <* pOCURLY <*> pTy <* pSEMI <*> pTy <* pSEMI <*> pRule <* pCCURLY
  <|> Binding_Instance  <$> pNmIs "instance"    <* pOCURLY <*> pListSep pSEMI pRule <* pCCURLY

pRule :: HIParser Rule
pRule
  = (\n t ev id c -> Rule_Rule n t ev id c [])
    <$> pNmIs "rule"
    <*  pOCURLY
    <*> pTy <* pSEMI
    <*> (   Pr.MkEvidVar  <$ pKeyTk "var"  <*> pDollNm
        <|> Pr.MkEvidCtxt <$ pKeyTk "ctxt" <*> pDollNm
        <|> Pr.MkEvidSup  <$ pKeyTk "sup"  <*> pDollNm <*> pInt <*> pCTag
        ) <* pSEMI
    <*> pPredOccId <* pSEMI
    <*> pProofCost
    <*  pCCURLY
%%]


%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% HI parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 module {%{EH}HI.Parser} import(UU.Parsing, EH.Util.ParseUtils(PlainParser), EH.Util.ScanUtils, {%{EH}Base.Common})
%%]

%%[20 import(qualified {%{EH}Pred} as Pr, {%{EH}Scanner.Common}, {%{EH}Scanner.Scanner}, {%{EH}Base.Parser}, {%{EH}Ty}, {%{EH}HI})
%%]

%%[20 import({%{EH}HS.Parser}(pFixity),{%{EH}Core.Parser}(pCExpr),{%{EH}GrinCode.Parser}(pExprSeq),{%{EH}Ty.Parser})
%%]

%%[20 import({%{EH}CHR},{%{EH}CHR.Constraint},{%{EH}Pred.CHR},{%{EH}Pred.CommonCHR})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 export(pAGItf)
type HIParser       hp     =    PlainParser Token hp

pNmIs :: String -> HIParser HsName
pNmIs k = pKeyTk k *> pDollNm <* pEQUAL

pAGItf :: HIParser AGItf
pAGItf
  = AGItf_AGItf <$> pModule

pModule :: HIParser Module
pModule
  = Module_Module <$ pMODULE <*> pDollNm <* pEQUAL <* pOCURLY <*> pListSep pSEMI pBinding <* pCCURLY

pConstraint :: HIParser (Constraint CHRPredOcc RedHowAnnotation)
pConstraint
  =   Prove     <$ pKeyTk "Prove"     <* pOCURLY <*> pCHRPredOcc <* pCCURLY
  <|> Assume    <$ pKeyTk "Assume"    <* pOCURLY <*> pCHRPredOcc <* pCCURLY
  <|> Reduction <$ pKeyTk "Reduction" <* pOCURLY <*> pCHRPredOcc
                                      <* pCOMMA  <*> pRedHowAnnotation
                                      <* pCOMMA  <*> pCurlySemiBlock pCHRPredOcc
                                      <* pCCURLY

pLabelOffset :: HIParser LabelOffset
pLabelOffset = pKeyTk "offset" *> (LabelOffset_Off <$> pInt <|> LabelOffset_Var <$> pUIDHI)

pLabel :: HIParser Label
pLabel = pKeyTk "label" *> (Label_Lab <$> pDollNm <|> Label_Var <$> pUIDHI)

pPredScope :: HIParser PredScope
pPredScope = pKeyTk "scope" *> (PredScope_Lev <$> pBracks_pCommas pInt <|> PredScope_Var <$> pUIDHI)

pCHRPredOcc :: HIParser CHRPredOcc
pCHRPredOcc = CHRPredOcc <$ pOCURLY <*> pPred <* pCOMMA <*> pPredScope <* pCCURLY

pGuard :: HIParser Guard
pGuard
  =   (\[sc1,sc2,sc3] -> HasStrictCommonScope   sc1 sc2 sc3) <$ pKeyTk "HasStrictCommonScope"  <*> pCurlyCommaBlock pPredScope
  <|> (\[sc1,sc2,sc3] -> IsStrictParentScope    sc1 sc2 sc3) <$ pKeyTk "IsStrictParentScope"   <*> pCurlyCommaBlock pPredScope
  <|> (\[sc1,sc2]     -> IsVisibleInScope       sc1 sc2    ) <$ pKeyTk "IsVisibleInScope"      <*> pCurlyCommaBlock pPredScope
  <|> (\[sc1,sc2]     -> NotEqualScope          sc1 sc2    ) <$ pKeyTk "NotEqualScope"         <*> pCurlyCommaBlock pPredScope
  <|>                    NonEmptyRowLacksLabel               <$ pKeyTk "NonEmptyRowLacksLabel" <* pOCURLY <*> pTy
                                                                                               <* pCOMMA  <*> pLabelOffset
                                                                                               <* pCOMMA  <*> pTy
                                                                                               <* pCOMMA  <*> pLabel
                                                                                               <* pCCURLY

pRedHowAnnotation :: HIParser RedHowAnnotation
pRedHowAnnotation
  =   RedHow_ByInstance   <$ pKeyTk "redhowinst"   <* pOCURLY <*> pDollNm
                                                   <* pCOMMA  <*> pPred
                                                   <* pCOMMA  <*> pPredScope
                                                   <* pCCURLY
  <|> RedHow_BySuperClass <$ pKeyTk "redhowsuper"  <* pOCURLY <*> pDollNm
                                                   <* pCOMMA  <*> pInt
                                                   <* pCOMMA  <*> pCTag
                                                   <* pCCURLY
  <|> RedHow_ProveObl     <$ pKeyTk "redhowprove"  <* pOCURLY <*> pUIDHI
                                                   <* pCOMMA  <*> pPredScope
                                                   <* pCCURLY
  <|> RedHow_Assumption   <$ pKeyTk "redhowassume" <* pOCURLY <*> pUIDHI
                                                   <* pCOMMA  <*> pDollNm
                                                   <* pCOMMA  <*> pPredScope
                                                   <* pCCURLY
  <|> RedHow_ByScope      <$ pKeyTk "redhowscope"
  <|> RedHow_ByLabel      <$ pKeyTk "redhowlabel"  <* pOCURLY <*> pLabel
                                                   <* pCOMMA  <*> pLabelOffset
                                                   <* pCOMMA  <*> pPredScope
                                                   <* pCCURLY

  <|> RedHow_Lambda       <$ pKeyTk "redhowlambda" <* pOCURLY <*> pUIDHI
                                                   <* pCOMMA  <*> pPredScope
                                                   <* pCCURLY

pBinding :: HIParser Binding
pBinding
  =   Binding_Fixity    <$> pNmIs "fixity"   <* pOCURLY <*> pInt    <* pSEMI <*> pFixity  <* pCCURLY
  <|> Binding_Ids       <$  pNmIs "iddef"    <* pOCURLY
                                             <*> pListSep pSEMI ((,) <$ pOCURLY <*> pIdOcc <* pSEMI <*> pIdOcc <* pCCURLY)
                                             <* pCCURLY
  <|> Binding_Arities   <$  pNmIs "arity"    <*> pCurlySemiBlock ((,) <$ pOCURLY <*> pDollNm <* pSEMI <*> pInt <* pCCURLY)
  <|> Binding_GrInlines <$  pNmIs "grInline" <*> pCurlySemiBlock ((\n a g -> (n,(a,g))) <$ pOCURLY <*> pDollNm <* pSEMI <*> pCurlySemiBlock pDollNm <* pSEMI <*> pCurlys pExprSeq <* pCCURLY)
  <|> Binding_Val       <$> pNmIs "value"    <* pOCURLY <*> pTy <* pCCURLY
  <|> Binding_Stamp     <$  pNmIs "stamp"    <* pOCURLY <*> pString <* pSEMI <*> pString
                                             <* pSEMI   <*> pString <* pSEMI <*> pString
                                             <* pSEMI   <*> pString <* pSEMI <*> pString
                                             <* pSEMI   <*> pString
                                             <* pSEMI   <*> (read <$> pInteger)
                                             <* pCCURLY
  <|> Binding_Export    <$  pNmIs "export"              <*> pModEntRel
  <|> Binding_Ty        <$> pNmIs "type"     <* pOCURLY <*> pTy      <* pSEMI <*> pTy     <* pCCURLY
  <|> (\tn cs -> Binding_DataCon tn (map (\f -> f tn) cs))
      <$> pNmIs "data" <*  pOCURLY
                       <*> pCurlySemiBlock
                             ((\n t a ma fm tn -> (n,(CTag tn n t a ma,fm)))
                              <$> pDollNm   <*  pEQUAL
                              <*  pOCURLY   <*> pInt <* pCOMMA <*> pInt <* pCOMMA <*> pInt
                                            <*> pList ((,) <$ pSEMI <*> pDollNm <* pEQUAL <*> pInt)
                              <*  pCCURLY
                             )
                       <*  pSEMI <*> pBool
                       <*  pCCURLY
  <|> Binding_Class     <$> pNmIs "class"       <* pOCURLY <*> pTy <* pSEMI <*> pTy <* pSEMI <*> pRule <* pCCURLY
  <|> Binding_Instance  <$> pNmIs "instance"    <* pOCURLY <*> pListSep pSEMI pRule <* pCCURLY
  <|> Binding_CHRStore  <$  pNmIs "chrstore"    <*> pCurlySemiBlock
                                                      (CHR <$ pOCURLY <*> pCurlySemiBlock pConstraint
                                                           <* pSEMI   <*> pInt
                                                           <* pSEMI   <*> pCurlySemiBlock pGuard
                                                           <* pSEMI   <*> pCurlySemiBlock pConstraint
                                                           <* pCCURLY
                                                      )

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


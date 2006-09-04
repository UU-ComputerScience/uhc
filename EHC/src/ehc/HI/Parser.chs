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
%%% Scanner
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12
%%]
hiScanOpts :: ScanOpts
hiScanOpts
  =  hsScanOpts
        {   scoKeywordsTxt      =   [ "value", "fixity", "stamp", "uid", "rule"
                                    ]
                                    ++ scoKeywordsTxt hsScanOpts
        ,   scoOpChars          =   scoOpChars coreScanOpts
        ,   scoDollarIdent      =   True
        ,   scoSpecChars        =   scoSpecChars coreScanOpts
        }

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
  =   Binding_Fixity    <$> pNmIs "fixity" <* pOCURLY <*> pInt     <* pSEMI <*> pFixity                <* pCCURLY
  <|> Binding_Val       <$> pNmIs "value"  <* pOCURLY <*> pTy                                          <* pCCURLY
  <|> Binding_Stamp     <$  pNmIs "stamp"  <* pOCURLY <*> pString  <* pSEMI <*> (read <$> pInteger)    <* pCCURLY
  <|> Binding_Ty        <$> pNmIs "type"   <* pOCURLY <*> pTy      <* pSEMI <*> pTy                    <* pCCURLY
  <|> (\tn cs -> Binding_DataCon tn (map (\f -> f tn) cs))
      <$> pNmIs "data" <*  pOCURLY
                       <*> pListSep pSEMI ((\n t a tn -> (n,CTag tn n t a)) <$> pDollNm <* pEQUAL <*> pInt <* pCOMMA <*> pInt)
                       <*  pCCURLY
  <|> Binding_Class     <$> pNmIs "class"       <* pOCURLY <*> pTy <* pSEMI <*> pTy <* pSEMI <*> pRule <* pCCURLY
  <|> Binding_Instance  <$> pNmIs "instance"    <* pOCURLY <*> pListSep pSEMI pRule <* pCCURLY

pRule :: HIParser Rule
pRule
  = (\n t ev id c -> Rule_Rule n t ev id c [])
    <$> pNmIs "rule"
    <*  pOCURLY
    <*> pTy <* pSEMI
    <*> pCExpr <* pSEMI
    <*> pPredOccId <* pSEMI
    <*> (Pr.Cost <$ pOCURLY <*> pInt <* pCOMMA <*> pInt <* pCCURLY)
    <*  pCCURLY
%%]

DATA Rule
    | Rule
        nm                      : HsName
        ty                      : Ty
        mkEvid                  : {([CExpr],CExpr)}
        uid                     : PredOccId
        cost                    : {Pr.ProofCost}
        fundeps                 : {[([HsName],[HsName])]}

SEM Rule
    | Rule
        lhs         .   pp          =   hiNV "rule" @nm [ppTyWithCfg cfgPPTyHI @ty,pp @uid,pp @mkEvid,pp @cost]


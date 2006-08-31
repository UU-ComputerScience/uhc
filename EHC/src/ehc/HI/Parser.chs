%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% HI parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12 module {%{EH}HI.Parser} import(UU.Parsing, EH.Util.ParseUtils(PlainParser), EH.Util.ScanUtils, {%{EH}Base.Common}, {%{EH}Scanner.Common}, {%{EH}Scanner.Scanner}, {%{EH}Ty}, {%{EH}HI}, {%{EH}HS}(mkQName)) export(pModule,hiScanOpts)
%%]

%%[12 import({%{EH}HS.Parser}(pFixity))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scanner
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12
hiScanOpts :: ScanOpts
hiScanOpts
  =  ehScanOpts
        {   scoKeywordsTxt      =   [ "value", "fixity", "stamp"
                                    ]
                                    ++ scoKeywordsTxt ehScanOpts
        ,   scoDollarIdent      =   True
        }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12
type HIParser       hp     =    PlainParser Token hp

pModule :: HIParser Module
pModule
  = Module_Module <$ pMODULE <*> pNm <* pOCURLY <*> pListSep pSEMI pBinding <* pCCURLY

pNm :: HIParser HsName
pNm = mkQName <$> pVaridTk

pUID :: HIParser UID
pUID = (UID . reverse) <$> pList1Sep pUNDERSCORE pInt

pInt :: HIParser Int
pInt = read <$> pInteger

pBinding :: HIParser Binding
pBinding
  =   Binding_Fixity    <$> pNm <* pOCURLY <*> pInt     <* pSEMI <*> pFixity                <* pCCURLY
  <|> Binding_Val       <$> pNm <* pOCURLY <*> pTy                                          <* pCCURLY
  <|> Binding_Stamp     <$  pNm <* pOCURLY <*> pString  <* pSEMI <*> (read <$> pInteger)    <* pCCURLY
  <|> Binding_Ty        <$> pNm <* pOCURLY <*> pTy      <* pSEMI <*> pTy                    <* pCCURLY
  <|> (\tn cs -> Binding_DataCon tn (map (\f -> f tn) cs))
      <$> pNm <*  pOCURLY
              <*> pListSep pSEMI ((\n t a tn -> (n,CTag tn n t a)) <$> pNm <* pEQUAL <*> pInt <* pCOMMA <*> pInt)
              <*  pCCURLY

pTyBase :: HIParser Ty
pTyBase
  =   mkTyVar <$> pUID
  <|> Ty_Con  <$> pNm
  <|> pParens pTy
  <|> Ty_Pred
      <$ pOIMPL
         <*> (pTy
              <**> (   (flip Pred_Lacks) <$ pLAM <*> pNm
                   <|> pSucceed Pred_Class
             )     )
         <*  pCIMPL

pTyApp :: HIParser Ty
pTyApp
  =   pTyBase
      <**> (   pSucceed id
           <|> flip Ty_App <$> pTyBase
           )

pTy :: HIParser Ty
pTy
  =   pTyApp
  <|> Ty_Quant TyQu_Forall <$ pFORALL <*> pUID <*> pTyApp
  <|> Ty_Quant TyQu_Exists <$ pEXISTS <*> pUID <*> pTyApp
%%]

  | CTag {ctagTyNm :: HsName, ctagNm :: HsName, ctagTag' :: Int, ctagArity :: Int}



%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Ty parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12 module {%{EH}Ty.Parser} import(UU.Parsing, EH.Util.ParseUtils(PlainParser), {%{EH}Base.Parser}, EH.Util.ScanUtils, {%{EH}Base.Common}, {%{EH}Scanner.Common}, {%{EH}Scanner.Scanner}, {%{EH}Ty})
%%]

%%[12 export(pUIDHI,pTy)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parsers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12
type P p = PlainParser Token p
%%]

%%[12
pUIDHI :: P UID
pUIDHI = pKeyTk "uid" *> pUID

%%]

%%[12
pTyBase :: P Ty
pTyBase
  =   mkTyVar <$> pUIDHI
  <|> Ty_Con  <$> pDollNm
  <|> pParens pTy
  <|> Ty_Pred
      <$ pOIMPL
         <*> (pTy
              <**> (   (flip Pred_Lacks) <$ pLAM <*> pDollNm
                   <|> pSucceed Pred_Class
             )     )
         <*  pCIMPL
  <|> pRow
  where pRow :: P Ty
        pRow
          = pOROWROW
             *> (   foldl (\r (l,e) -> Ty_Ext r l e) <$> pRow <* pVBAR <*> pList1Sep pCOMMA ((,) <$> pDollNm <* pDCOLON <*> pTy)
                <|> pSucceed (Ty_Con hsnRowEmpty)
                )
            <*  pCROWROW

pTyApp :: P Ty
pTyApp
  =   foldl1 Ty_App <$> pList1 pTyBase

pTy :: P Ty
pTy
  =   pTyApp
  <|> Ty_Quant TyQu_Forall <$ pFORALL <*> pUIDHI <* pDOT <*> pTyApp
  <|> Ty_Quant TyQu_Exists <$ pEXISTS <*> pUIDHI <* pDOT <*> pTyApp
%%]

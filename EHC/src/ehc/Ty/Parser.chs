%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Ty parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(20 hmtyinfer || hmtyast) module {%{EH}Ty.Parser} import(UU.Parsing, EH.Util.ParseUtils, {%{EH}Base.Parser}, EH.Util.ScanUtils, {%{EH}Base.Common}, {%{EH}Base.Builtin},{%{EH}Scanner.Common}, {%{EH}Scanner.Scanner}, {%{EH}Ty})
%%]

%%[(20 hmtyinfer || hmtyast) export(pTy,pPred)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parsers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(20 hmtyinfer || hmtyast)
-- type P p = PlainParser Token p
%%]

%%[(20 hmtyinfer || hmtyast)
pPred :: P Pred
pPred
  = pOIMPL
     *> (pTy
         <**> (   (flip Pred_Lacks) <$ pLAM <*> (Label_Lab <$> pDollNm <|> Label_Var <$> pUIDHI)
              <|> pSucceed Pred_Class
        )     )
    <*  pCIMPL

pTyBase :: P Ty
pTyBase
  =   mkTyVar <$> pUIDHI
  <|> Ty_Any  <$  pQUESTQUEST
  <|> Ty_Con  <$> pDollNm
  <|> pParens pTy
  <|> Ty_Pred <$> pPred
  <|> pRow
  where pRow :: P Ty
        pRow
          = pOROWROW
             *> (   foldl (\r (l,e) -> Ty_Ext r l e)
                    <$> pRow <* pVBAR
                    <*> pList1Sep pCOMMA ((,) <$> (pDollNm <|> mkHNmPos <$> pInt) <* pDCOLON <*> pTy)
                <|> pSucceed (Ty_Con hsnRowEmpty)
                )
            <*  pCROWROW

pTyApp :: P Ty
pTyApp
  =   foldl1 Ty_App <$> pList1 pTyBase

pTy :: P Ty
pTy
  =   pTyApp
  <|> Ty_Quant
      <$> ((TyQu_Forall <$ pFORALL <|> TyQu_Exists <$ pEXISTS) <*> pMaybe 0 id (pSTAR *> pMaybe 1 id pInt))
      <*> pUIDHI
      <*> pMaybe kiStar id (pParens pTy)
      <*  pDOT
      <*> pTy
  <|> Ty_Lam <$ pLAM <*> pUIDHI <* pRARROW <*> pTy
%%]

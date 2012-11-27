%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Ty parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 hmtyinfer || hmtyast) module {%{EH}Ty.Parser} import(UU.Parsing, UHC.Util.ParseUtils, {%{EH}Base.Parser}, UHC.Util.ScanUtils, {%{EH}Base.Common},{%{EH}Base.TermLike}, {%{EH}Base.Builtin},{%{EH}Scanner.Common}, {%{EH}Scanner.Scanner}, {%{EH}Ty})
%%]

%%[(50 hmtyinfer || hmtyast) export(pTy,pPred)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parsers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 hmtyinfer || hmtyast)
-- type P p = PlainParser Token p
%%]

%%[(50 hmtyinfer || hmtyast)
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
  <|> appCon  <$> pDollNm
  <|> pParens pTy
  <|> mkTyPr  <$> pPred
  <|> pRow
  where pRow :: P Ty
        pRow
          = pOROWROW
             *> (   foldl (\r (l,e) -> Ty_Ext r l e)
                    <$> pRow <* pVBAR
                    <*> pList1Sep pCOMMA ((,) <$> (pDollNm <|> mkHNmPos <$> pInt) <* pDCOLON <*> pTy)
                <|> pSucceed (appCon hsnRowEmpty)
                )
            <*  pCROWROW

pTyApp :: P Ty
pTyApp
  =   foldl1 Ty_App <$> pList1 pTyBase

pTy :: P Ty
pTy
  =   pTyApp
  <|> Ty_TBind
      <$> ((TyQu_Forall <$ pFORALL <|> TyQu_Exists <$ pEXISTS) <*> pMaybe 0 id (pSTAR *> pMaybe 1 id pInt))
      <*> pUIDHI
      <*> pMaybe kiStar id (pParens pTy)
      <*  pDOT
      <*> pTy
  <|> Ty_Lam <$ pLAM <*> pUIDHI <* pRARROW <*> pTy
%%]

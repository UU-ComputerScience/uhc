%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Ty parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 hmtyinfer || hmtyast) module {%{EH}Ty.Parser} import(UU.Parsing, UHC.Util.ParseUtils, {%{EH}Base.Parser}, UHC.Util.ScanUtils, {%{EH}Base.Common},{%{EH}Base.TermLike}, {%{EH}Base.HsName.Builtin},{%{EH}Scanner.Common}, {%{EH}Scanner.Scanner}, {%{EH}Ty})
%%]

%%[(8 hmtyinfer || hmtyast)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parsers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 hmtyinfer || hmtyast)
-- type P p = PlainParser Token p
%%]

%%[(8 hmtyinfer || hmtyast) export(pTy', pTy)
pTy :: P Ty
pTy = pTy' pDollNm

pTy' :: P HsName -> P Ty
pTy' pNm = pT
  where 
{-
    pPred :: P Pred
    pPred
      = pOIMPL
         *> (pT
             <**> (   (flip Pred_Lacks) <$ pLAM <*> (Label_Lab <$> pNm <|> Label_Var <$> pUIDHI)
                  <|> pSucceed Pred_Class
            )     )
        <*  pCIMPL
-}

    pTyBase :: P Ty
    pTyBase
      =   appCon  <$> pNm
      <|> pParens pT
      <|> pBracks pT
      <|> pRow
      <|> mkTyVar <$> pUID
{-
      <|> Ty_Any  <$  pQUESTQUEST
      <|> mkTyPr  <$> pPred
-}
      where pRow :: P Ty
            pRow
              = pOROWROW
                 *> (   foldl (\r (l,e) -> Ty_Ext r l e)
                        <$> pRow <* pVBAR
                        <*> pList1Sep pCOMMA ((,) <$> (pNm <|> mkHNmPos <$> pInt) <* pDCOLON <*> pT)
                    <|> pSucceed (appCon hsnRowEmpty)
                    )
                <*  pCROWROW

    pTyApp :: P Ty
    pTyApp
      =   appTopApp <$> pList1 pTyBase

    pT :: P Ty
    pT
      =   pTyApp <**>
            (   flip app1Arr <$ pRARROW <*> pT
            <|> pSucceed id
            )
{-
      <|> Ty_TBind
          <$> ((TyQu_Forall <$ pFORALL <|> TyQu_Exists <$ pEXISTS) <*> pMaybe 0 id (pSTAR *> pMaybe 1 id pInt))
          <*> pUIDHI
          <*> pMaybe kiStar id (pParens pT)
          <*  pDOT
          <*> pT
      <|> Ty_Lam <$ pLAM <*> pUIDHI <* pRARROW <*> pT
-}
%%]

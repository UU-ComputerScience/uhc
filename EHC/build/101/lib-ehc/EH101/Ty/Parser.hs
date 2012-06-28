module EH101.Ty.Parser
( pTy, pPred )
where
import UU.Parsing
import EH.Util.ParseUtils
import EH101.Base.Parser
import EH.Util.ScanUtils
import EH101.Base.Common
import EH101.Base.Builtin
import EH101.Scanner.Common
import EH101.Scanner.Scanner
import EH101.Ty

{-# LINE 21 "src/ehc/Ty/Parser.chs" #-}
-- type P p = PlainParser Token p

{-# LINE 25 "src/ehc/Ty/Parser.chs" #-}
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
  <|> mkTyPr  <$> pPred
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
  <|> Ty_TBind
      <$> ((TyQu_Forall <$ pFORALL <|> TyQu_Exists <$ pEXISTS) <*> pMaybe 0 id (pSTAR *> pMaybe 1 id pInt))
      <*> pUIDHI
      <*> pMaybe kiStar id (pParens pTy)
      <*  pDOT
      <*> pTy
  <|> Ty_Lam <$ pLAM <*> pUIDHI <* pRARROW <*> pTy

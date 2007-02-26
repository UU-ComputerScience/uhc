%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Core parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{EH}Core.Parser} import(UU.Parsing, EH.Util.ParseUtils, EH.Util.ScanUtils, {%{EH}Base.Common}, {%{EH}Scanner.Common}, {%{EH}Scanner.Scanner}, {%{EH}Base.Parser}, {%{EH}Ty.Parser(pTy)}, {%{EH}Core})
%%]

%%[20 export(pCModule,pCExpr)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scanner
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20
type CParser       hp     =    PlainParser Token hp

pCModule :: CParser CModule
pCModule
  = (\m e tm -> CModule_Mod m e tm) <$ pMODULE <*> pDollNm <* pEQUAL <*> pCExpr <*> pA (pA pCTag)
  where pA pE = pOCURLY *> pListSep pSEMI ((,) <$> pDollNm <* pEQUAL <*> pE) <* pCCURLY

pCTagOnly :: CParser CTag
pCTagOnly = pNUMBER *> pKeyTk "Tag" *> pCTag

pCExprBase :: CParser CExpr
pCExprBase
  =   CExpr_Var <$> pDollNm
  <|> pNUMBER
       *> (   (   (CExpr_Int     . read) <$ pKeyTk "Int"
              <|> (CExpr_Char    . head) <$ pKeyTk "Char"
              <|> (CExpr_String        ) <$ pKeyTk "String"
%%[[97
              <|> (CExpr_Integer . read) <$ pKeyTk "Integer"
%%]
              )
              <*> (tokMkStr <$> pStringTk)
          <|> CExpr_Tup <$ pKeyTk "Tag" <*> pCTag
          )
  <|> pOPAREN *> pCExpr <* pCPAREN

pCExprSelSuffix :: CParser (CExpr -> CExpr)
pCExprSelSuffix
  =   (\(t,o,l)    e -> CExpr_TupDel e t l o   ) <$ pKeyTk "-=" <*> pS
  <|> (\(t,o,l) e' e -> CExpr_TupIns e t l o e') <$ pKeyTk "+=" <*> pS <*> pCExprBase
  <|> (\(t,o,l) e' e -> CExpr_TupUpd e t l o e') <$ pKeyTk ":=" <*> pS <*> pCExprBase
  where pS = (,,) <$ pOCURLY <*> pCTagOnly <* pCOMMA <*> pCExpr <* pCOMMA <*> pDollNm <* pCCURLY

pCExprSel :: CParser CExpr
pCExprSel = pCExprBase <??> pCExprSelSuffix

pCExpr :: CParser CExpr
pCExpr
  =   foldl1 CExpr_App <$> pList1 pCExprSel
  <|> (\as b -> foldr CExpr_Lam b as) <$ pLAM <*> pList1 pDollNm <* pRARROW <*> pCExpr
  <|> CExpr_Let <$ pLET <*> pMaybe CBindPlain id pCBindCateg <* pOCURLY <*> pListSep pSEMI pCBind <* pCCURLY <* pIN <*> pCExpr
  <|> CExpr_Case <$ pCASE <*> pCExpr <* pOF
      <* pOCURLY <*> pListSep pSEMI pCAlt <* pCCURLY
      <* pOCURLY <* pDEFAULT <*> pCExpr <* pCCURLY
  where pCBindCateg
          =   CBindRec    <$ pKeyTk "rec"
          <|> CBindFFI    <$ pFOREIGN
          <|> CBindStrict <$ pBANG

pCBind :: CParser CBind
pCBind
  = (pDollNm <* pEQUAL)
    <**> (   (\e n -> CBind_Bind n e) <$> pCExpr
         <|> (\c s i t n -> CBind_FFI c s i n t) <$ pFOREIGN <* pOCURLY <*> pS <* pCOMMA <*> pS <* pCOMMA <*> pS <* pCOMMA <*> pTy <* pCCURLY
         )
  where pS = tokMkStr <$> pStringTk

pCAlt :: CParser CAlt
pCAlt
  =   (\p e -> CAlt_Alt p e) <$> pCPat <* pRARROW <*> pCExpr

pCPat :: CParser CPat
pCPat
  =   pDollNm
      <**> (   pNUMBER
				*> (   (   (\s n -> CPat_Int  n (read s)) <$ pKeyTk "Int"
					   <|> (\s n -> CPat_Char n (head s)) <$ pKeyTk "Char"
					   )
				       <*> (tokMkStr <$> pStringTk)
				   <|> (\t r bs n -> CPat_Con n t r bs)
				       <$  pKeyTk "Tag" <*> pCTag
				       <*  pOCURLY <*> pCPatRest <* pVBAR <*> pListSep pCOMMA pCPatBind <* pCCURLY
				   )
           <|> pSucceed CPat_Var
           )
  where -- pRPatNm = RPatNmOrig <$> pDollNm <|> RPatNmUniq <$ pKeyTk "uniq" <*> pDollNm
        pCPatRest = pMaybe CPatRest_Empty CPatRest_Var pDollNm

pCPatBind :: CParser CPatBind
pCPatBind
  = CPatBind_Bind <$ pOCURLY <*> pDollNm <* pCOMMA <*> pCExpr <* pCOMMA <*> pDollNm <* pCCURLY <* pEQUAL <*> pCPat
%%]

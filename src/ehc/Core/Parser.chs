%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Core parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) module {%{EH}Core.Parser} import(UU.Parsing as P, EH.Util.ParseUtils, EH.Util.ScanUtils, {%{EH}Base.Common}, {%{EH}Scanner.Common}, {%{EH}Scanner.Scanner}, {%{EH}Base.Parser}, {%{EH}Ty.Parser(pTy)}, {%{EH}Core})
%%]

%%[(20 codegen) export(pCModule,pCExpr)
%%]

%%[(94 codegen) import({%{EH}Foreign.Parser})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(20 codegen)
type CParser       hp     =    PlainParser Token hp

pCModule :: CParser CModule
pCModule
  = (\m e tm -> CModule_Mod m e tm) <$ pMODULE <*> pDollNm <* pEQUAL <*> pCExpr <*> pA (pA pCTag)
  where pA pE = pOCURLY *> pListSep pSEMI ((,) <$> pDollNm <* pEQUAL <*> pE) <* pCCURLY

pCTagOnly :: CParser CTag
pCTagOnly = pNUMBER *> pKeyTk "Tag" *> pCTag

pCNumber :: CParser CExpr
pCNumber
  =    pNUMBER
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

pCExprBase :: CParser CExpr
pCExprBase
  =   CExpr_Var <$> pDollNm
  <|> pCNumber
  <|> pOPAREN *> pCExpr <* pCPAREN

pCExprBaseMeta :: CParser (CExpr,CMeta)
pCExprBaseMeta
  =   (\v m -> (CExpr_Var v, m))<$> pDollNm <*> pCMetaOpt
  <|> (\n   -> (n, CMeta_Val)  ) <$> pCNumber
  <|> pOPAREN *> pCExpr P.<+> pCMetaOpt <* pCPAREN

pCExprSelSuffix :: CParser (CExpr -> CExpr)
pCExprSelSuffix
  =   (\(t,o,l)    e -> CExpr_TupDel e t l o   ) <$ pKeyTk "-=" <*> pS
  <|> (\(t,o,l) e' e -> CExpr_TupIns e t l o e') <$ pKeyTk "+=" <*> pS <*> pCExprBase
  <|> (\(t,o,l) e' e -> CExpr_TupUpd e t l o e') <$ pKeyTk ":=" <*> pS <*> pCExprBase
  where pS = (,,) <$ pOCURLY <*> pCTagOnly <* pCOMMA <*> pCExpr <* pCOMMA <*> pDollNm <* pCCURLY

pCExprSelSuffixMeta :: CParser ((CExpr,CMeta) -> (CExpr,CMeta))
pCExprSelSuffixMeta
  = (\f (e,m) -> (f e,m)) <$> pCExprSelSuffix

pCExprSelMeta :: CParser (CExpr,CMeta)
pCExprSelMeta = pCExprBaseMeta <??> pCExprSelSuffixMeta

pCExprSel :: CParser CExpr
pCExprSel = pCExprBase <??> pCExprSelSuffix

pCExpr :: CParser CExpr
pCExpr
  =   mkCExprAppMeta <$> pCExprSel <*> pList pCExprSelMeta
  <|> mkCExprLamMeta <$  pLAM <*> pList1 (pDollNm P.<+> pCMetaOpt) <* pRARROW <*> pCExpr
  <|> CExpr_Let      <$  pLET <*> pMaybe CBindPlain id pCBindCateg <* pOCURLY <*> pListSep pSEMI pCBind <* pCCURLY <* pIN <*> pCExpr
  <|> CExpr_Case <$ pCASE <*> pCExpr <* pOF
      <* pOCURLY <*> pListSep pSEMI pCAlt <* pCCURLY
      <* pOCURLY <*  pDEFAULT <*> pCExpr <* pCCURLY
  where pCBindCateg
          =   CBindRec    <$ pKeyTk "rec"
          <|> CBindFFI    <$ pFOREIGN
%%[[94
          <|> CBindFFE    <$ pKeyTk "foreignexport"
%%]]
          <|> CBindStrict <$ pBANG


pMbDollNm :: CParser (Maybe HsName)
pMbDollNm
  =  f <$> pDollNm
    where f (HNm "_") = Nothing
          f x         = Just x

pCMeta :: CParser CMeta
pCMeta
  =   CMeta_Val          <$ pKeyTk "VAL"
  <|> CMeta_Dict         <$ pKeyTk "DICT"  <*> ( Just <$ pOCURLY <*> (pInt <|> ((\_ n -> 0-n) <$> pMINUS <*> pInt)) <* pCCURLY
                                               <|> pSucceed Nothing
                                               )
  <|> CMeta_DictClass    <$ pKeyTk "DICTCLASS"    <* pOCURLY <*> pListSep pCOMMA pMbDollNm <* pCCURLY
  <|> CMeta_DictInstance <$ pKeyTk "DICTINSTANCE" <* pOCURLY <*> pListSep pCOMMA pMbDollNm <* pCCURLY

pCMetaOpt :: CParser CMeta
pCMetaOpt
  =   pMaybe CMeta_Val id (pCOLON *> pCMeta)

pCBind :: CParser CBind
pCBind
  = (  (pDollNm P.<+> pCMetaOpt) <* pEQUAL)
    <**> (   (\e (n,m)        -> mkCBind1Meta n m e) <$> pCExpr
         <|> (\c s i t (n,m)  -> CBind_FFI c s i n t) <$ pFOREIGN <* pOCURLY <*> pS <* pCOMMA <*> pS <* pCOMMA <*> pS <* pCOMMA <*> pTy <* pCCURLY
%%[[94
         <|> (\c e en t (n,m) -> CBind_FFE n c (fst $ parseForeignEnt e) en t) <$ pKeyTk "foreignexport" <* pOCURLY <*> pS <* pCOMMA <*> pS <* pCOMMA <*> pDollNm <* pCOMMA <*> pTy <* pCCURLY
%%]]
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

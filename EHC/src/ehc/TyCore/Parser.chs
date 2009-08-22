%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Core parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) module {%{EH}Core.Parser} import(UU.Parsing as P, EH.Util.ParseUtils, EH.Util.ScanUtils, {%{EH}Base.Common}, {%{EH}Scanner.Common}, {%{EH}Scanner.Scanner}, {%{EH}Base.Parser}, {%{EH}Ty.Parser(pTy)}, {%{EH}Core})
%%]
%%[(8 codegen) import(Data.Maybe)
%%]

%%[(20 codegen) export(pCModule,pExpr)
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
  = (\m e tm -> CModule_Mod m e tm) <$ pMODULE <*> pDollNm <* pEQUAL <*> pExpr <*> pA (pA pCTag)
  where pA pE = pOCURLY *> pListSep pSEMI ((,) <$> pDollNm <* pEQUAL <*> pE) <* pCCURLY

pCTagOnly :: CParser CTag
pCTagOnly = pNUMBER *> pKeyTk "Tag" *> pCTag

pCNumber :: CParser Expr
pCNumber
  =    pNUMBER
       *> (   (   (Expr_Int     . read) <$ pKeyTk "Int"
              <|> (Expr_Char    . head) <$ pKeyTk "Char"
              <|> (Expr_String        ) <$ pKeyTk "String"
%%[[97
              <|> (Expr_Integer . read) <$ pKeyTk "Integer"
%%]
              )
              <*> (tokMkStr <$> pStringTk)
          <|> Expr_Tup <$ pKeyTk "Tag" <*> pCTag
          )

pExprBase :: CParser Expr
pExprBase
  =   Expr_Var <$> pDollNm
  <|> pCNumber
  <|> pOPAREN *> pExpr <* pCPAREN

pExprBaseMeta :: CParser (Expr,MetaVal)
pExprBaseMeta
  =   (\v m -> (Expr_Var v, m))<$> pDollNm <*> pMetaValOpt
  <|> (\n   -> (n, MetaVal_Val)  ) <$> pCNumber
  <|> pOPAREN *> pExpr P.<+> pMetaValOpt <* pCPAREN

pExprSelSuffix :: CParser (Expr -> Expr)
pExprSelSuffix
  =   (\(t,o,l)    e -> Expr_TupDel e t l o   ) <$ pKeyTk "-=" <*> pS
  <|> (\(t,o,l) e' e -> Expr_TupIns e t l o e') <$ pKeyTk "+=" <*> pS <*> pExprBase
  <|> (\(t,o,l) e' e -> Expr_TupUpd e t l o e') <$ pKeyTk ":=" <*> pS <*> pExprBase
  where pS = (,,) <$ pOCURLY <*> pCTagOnly <* pCOMMA <*> pExpr <* pCOMMA <*> pDollNm <* pCCURLY

pExprSelSuffixMeta :: CParser ((Expr,MetaVal) -> (Expr,MetaVal))
pExprSelSuffixMeta
  = (\f (e,m) -> (f e,m)) <$> pExprSelSuffix

pExprSelMeta :: CParser (Expr,MetaVal)
pExprSelMeta = pExprBaseMeta <??> pExprSelSuffixMeta

pExprSel :: CParser Expr
pExprSel = pExprBase <??> pExprSelSuffix

pExpr :: CParser Expr
pExpr
  =   mkExprAppMeta <$> pExprSel <*> pList pExprSelMeta
  <|> mkExprLamMeta <$  pLAM <*> pList1 (pDollNm P.<+> pMetaValOpt) <* pRARROW <*> pExpr
  <|> Expr_Let      <$  pLET <*> pMaybe ValBindCateg_Plain id pValBindCateg <* pOCURLY <*> pListSep pSEMI pCBind <* pCCURLY <* pIN <*> pExpr
  <|> Expr_Case <$ pCASE <*> pExpr <* pOF
      <* pOCURLY <*> pListSep pSEMI pAlt <* pCCURLY
      <* pOCURLY <*  pDEFAULT <*> pExpr <* pCCURLY
  where pValBindCateg
          =   ValBindCateg_Rec    <$ pKeyTk "rec"
          <|> ValBindCateg_FFI    <$ pFOREIGN
%%[[94
          <|> ValBindCateg_FFE    <$ pKeyTk "foreignexport"
%%]]
          <|> ValBindCateg_Strict <$ pBANG


pMbDollNm :: CParser (Maybe HsName)
pMbDollNm
  =  f <$> pDollNm
    where f n | isJust ms && fromJust ms == "_"
                      = Nothing
                      where ms = mbHNm n
          f x         = Just x

pCMetas :: CParser CMetas
pCMetas
  =   (,) <$ pOCURLY <*> pCMetaBind <* pCOMMA <*> pMetaVal <* pCCURLY

pCMetasOpt :: CParser CMetas
pCMetasOpt
  =   pMaybe metasDefault id pCMetas

pCMetaBind :: CParser CMetaBind
pCMetaBind
  =   CMetaBind_Plain       <$ pKeyTk "BINDPLAIN"
  <|> CMetaBind_Function0   <$ pKeyTk "BINDFUNCTION0"
  <|> CMetaBind_Function1   <$ pKeyTk "BINDFUNCTION1"
  <|> CMetaBind_Apply0      <$ pKeyTk "BINDAPPLY0"

pMetaVal :: CParser MetaVal
pMetaVal
  =   MetaVal_Val          <$ pKeyTk "VAL"
  <|> MetaVal_Dict         <$ pKeyTk "DICT"  <*> ( Just <$ pOCURLY <*> (pInt <|> ((\_ n -> 0-n) <$> pMINUS <*> pInt)) <* pCCURLY
                                                  <|> pSucceed Nothing
                                                  )
  <|> MetaVal_DictClass    <$ pKeyTk "DICTCLASS"    <* pOCURLY <*> pListSep pCOMMA pMbDollNm <* pCCURLY
  <|> MetaVal_DictInstance <$ pKeyTk "DICTINSTANCE" <* pOCURLY <*> pListSep pCOMMA pMbDollNm <* pCCURLY

pMetaValOpt :: CParser MetaVal
pMetaValOpt
  =   pMaybe MetaVal_Val id (pCOLON *> pMetaVal)

pCBind :: CParser CBind
pCBind
  = (  (pDollNm P.<+> pCMetasOpt) <* pEQUAL)
    <**> (   (\e (n,m)        -> CBind_Bind n m e) <$> pExpr
         <|> (\(c,_) s i t (n,m)  -> CBind_FFI c s (mkEnt c i) n t)
             <$ pFOREIGN <* pOCURLY <*> pFFIWay <* pCOMMA <*> pS <* pCOMMA <*> pS <* pCOMMA <*> pTy <* pCCURLY
%%[[94
         <|> (\(c,_) e en t (n,m) -> CBind_FFE n c (mkEnt c e) en t)
             <$ pKeyTk "foreignexport" <* pOCURLY <*> pFFIWay <* pCOMMA <*> pS <* pCOMMA <*> pDollNm <* pCOMMA <*> pTy <* pCCURLY
%%]]
         )
  where pS = tokMkStr <$> pStringTk
%%[[8
        mkEnt _ e = e
%%][94
        mkEnt c e = fst $ parseForeignEnt c Nothing e
%%]]

pAlt :: CParser Alt
pAlt
  =   (\p e -> Alt_Alt p e) <$> pPat <* pRARROW <*> pExpr

pPat :: CParser Pat
pPat
  =   pDollNm
      <**> (   pNUMBER
				*> (   (   (\s n -> Pat_Int  n (read s)) <$ pKeyTk "Int"
					   <|> (\s n -> Pat_Char n (head s)) <$ pKeyTk "Char"
					   )
				       <*> (tokMkStr <$> pStringTk)
				   <|> (\t r bs n -> Pat_Con n t r bs)
				       <$  pKeyTk "Tag" <*> pCTag
				       <*  pOCURLY <*> pPatRest <* pVBAR <*> pListSep pCOMMA pFldBind <* pCCURLY
				   )
           <|> pSucceed Pat_Var
           )
  where -- pRPatNm = RPatNmOrig <$> pDollNm <|> RPatNmUniq <$ pKeyTk "uniq" <*> pDollNm
        pPatRest = pMaybe PatRest_Empty PatRest_Var pDollNm

pFldBind :: CParser FldBind
pFldBind
  = FldBind_Fld <$ pOCURLY <*> pDollNm <* pCOMMA <*> pExpr <* pCOMMA <*> pDollNm <* pCCURLY <* pEQUAL <*> pPat
%%]

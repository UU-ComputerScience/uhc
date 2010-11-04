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

%%[(8 codegen) hs import({%{EH}AbstractCore})
%%]

%%[(20 codegen) export(pCModule,pCExpr)
%%]

%%[(90 codegen) import({%{EH}Foreign.Parser})
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
       *> (   (   (acoreInt2     . read) <$ pKeyTk "Int"
              <|> (CExpr_Char    . head) <$ pKeyTk "Char"
              <|> (CExpr_String        ) <$ pKeyTk "String"
%%[[97
              <|> (CExpr_Integer . read) <$ pKeyTk "Integer"
%%]
              )
              <*> (tokMkStr <$> pStringTk)
          <|> CExpr_Tup <$ pKeyTk "Tag" <*> pCTag
          )

pCExprAnn :: CParser (CExpr -> CExpr)
pCExprAnn
  =   CExpr_Ann
      <$> (pDCOLON *> (CExprAnn_Ty <$> pTy)
          )
  <|> pSucceed id

pCExprBase :: CParser CExpr
pCExprBase
  =   acoreVar <$> pDollNm
  <|> pCNumber
  <|> pOPAREN *> (pCExpr <**> pCExprAnn) <* pCPAREN

pCExprBaseMeta :: CParser (CExpr,CMetaVal)
pCExprBaseMeta
  =   (\v m -> (acoreVar v, m))<$> pDollNm <*> pCMetaValOpt
  <|> (\n   -> (n, CMetaVal_Val)  ) <$> pCNumber
  <|> pOPAREN *> pCExpr P.<+> pCMetaValOpt <* pCPAREN

pCExprSelSuffix :: CParser (CExpr -> CExpr)
pCExprSelSuffix
  =   (\(t,o,l)    e -> CExpr_TupDel e t l o   ) <$ pKeyTk "-=" <*> pS
  <|> (\(t,o,l) e' e -> CExpr_TupIns e t l o e') <$ pKeyTk "+=" <*> pS <*> pCExprBase
  <|> (\(t,o,l) e' e -> CExpr_TupUpd e t l o e') <$ pKeyTk ":=" <*> pS <*> pCExprBase
  where pS = (,,) <$ pOCURLY <*> pCTagOnly <* pCOMMA <*> pCExpr <* pCOMMA <*> pDollNm <* pCCURLY

pCExprSelSuffixMeta :: CParser ((CExpr,CMetaVal) -> (CExpr,CMetaVal))
pCExprSelSuffixMeta
  = (\f (e,m) -> (f e,m)) <$> pCExprSelSuffix

pCExprSelMeta :: CParser (CExpr,CMetaVal)
pCExprSelMeta = pCExprBaseMeta <??> pCExprSelSuffixMeta

pCExprSel :: CParser CExpr
pCExprSel = pCExprBase <??> pCExprSelSuffix

pCExpr :: CParser CExpr
pCExpr
  =   (\f as -> acoreApp f (map fst as))
                     <$> pCExprSel <*> pList pCExprSelMeta
  <|> acoreLam       <$  pLAM <*> pList1 (pDollNm) <* pRARROW <*> pCExpr
  <|> CExpr_Let      <$  pLET <*> pMaybe CBindings_Plain id pCBindingsCateg <* pOCURLY <*> pListSep pSEMI pCBind <* pCCURLY <* pIN <*> pCExpr
  <|> CExpr_Case <$ pCASE <*> pCExpr <* pOF
      <* pOCURLY <*> pListSep pSEMI pCAlt <* pCCURLY
      <* pOCURLY <*  pDEFAULT <*> {- pMb -} pCExpr <* pCCURLY
  where pCBindingsCateg
          =   CBindings_Rec    <$ pKeyTk "rec"
          <|> CBindings_FFI    <$ pFOREIGN
%%[[90
          <|> CBindings_FFE    <$ pKeyTk "foreignexport"
%%]]
          <|> CBindings_Strict <$ pBANG


pTrack          ::   CParser Track
pTrack          =    (\x -> TrackVarApply x [])  <$> pDollNm     -- TODO: this is just a mockup, should do real track parsing

pMbDollNm :: CParser (Maybe HsName)
pMbDollNm
  =  f <$> pDollNm
    where f n | isJust ms && fromJust ms == "_"
                      = Nothing
                      where ms = mbHNm n
          f x         = Just x

pManyDollNm :: CParser [HsName]
pManyDollNm
  =  f <$> pList pDollNm
    where -- for backward compatibility with libraries created before 20090917
          f [n] | isJust ms && fromJust ms == "_"
                      = []
                      where ms = mbHNm n
          f ns        = ns


pCMetas :: CParser CMetas
pCMetas
  =   (,) <$ pOCURLY <*> pCMetaBind <* pCOMMA <*> pCMetaVal <* pCCURLY

pCMetasOpt :: CParser CMetas
pCMetasOpt
  =   pMaybe cmetasDefault id pCMetas

pCMetaBind :: CParser CMetaBind
pCMetaBind
  =   CMetaBind_Plain       <$ pKeyTk "BINDPLAIN"
  <|> CMetaBind_Function0   <$ pKeyTk "BINDFUNCTION0"
  <|> CMetaBind_Function1   <$ pKeyTk "BINDFUNCTION1"
  <|> CMetaBind_Apply0      <$ pKeyTk "BINDAPPLY0"

pCMetaVal :: CParser CMetaVal
pCMetaVal
  =   CMetaVal_Val          <$ pKeyTk "VAL"
  <|> CMetaVal_Dict         <$ pKeyTk "DICT"
  <|> CMetaVal_DictClass    <$ pKeyTk "DICTCLASS"    <* pOCURLY <*> pListSep pCOMMA pTrack <* pCCURLY
  <|> CMetaVal_DictInstance <$ pKeyTk "DICTINSTANCE" <* pOCURLY <*> pList1Sep pCOMMA pTrack <* pCCURLY
  -- TODO: parse Track

pCMetaValOpt :: CParser CMetaVal
pCMetaValOpt
  =   pMaybe CMetaVal_Val id (pCOLON *> pCMetaVal)

-- 20100806 AD: due to intro of CBindAspect not consistent with pretty printing anymore, just patched it to have it compiled
pCBind :: CParser CBind
pCBind
  = (  (pDollNm P.<+> pCMetasOpt) <* pEQUAL)
    <**> (   (\e (n,m)        -> CBind_Bind n [CBindAspect_Bind m e]) <$> pCExpr
         <|> (\(c,_) s i t (n,m)  -> CBind_Bind n [CBindAspect_Bind m $ CExpr_FFI c s (mkImpEnt c i) t])
             <$ pFOREIGN <* pOCURLY <*> pFFIWay <* pCOMMA <*> pS <* pCOMMA <*> pS <* pCOMMA <*> pTy <* pCCURLY
%%[[90
         <|> (\(c,_) e en t (n,m) -> CBind_Bind n [CBindAspect_FFE c (mkEnt ForeignDirection_Export c e) en t])
             <$ pKeyTk "foreignexport" <* pOCURLY <*> pFFIWay <* pCOMMA <*> pS <* pCOMMA <*> pCExpr {- pDollNm -} <* pCOMMA <*> pTy <* pCCURLY
%%]]
         )
  where pS = tokMkStr <$> pStringTk
%%[[8
        mkImpEnt c e = e
%%][90
        mkEnt d c e = fst $ parseForeignEnt d c Nothing e
        mkImpEnt c e = mkEnt ForeignDirection_Import c e
%%]]

pCAlt :: CParser CAlt
pCAlt
  =   (\p e -> CAlt_Alt p e) <$> pCPat <* pRARROW <*> pCExpr

pCPat :: CParser CPat
pCPat
  =   pNUMBER
       *> (   (   (CPat_Int  . read) <$ pKeyTk "Int"
              <|> (CPat_Char . head) <$ pKeyTk "Char"
              )
              <*> (tokMkStr <$> pStringTk)
          <|> CPat_Con
              <$  pKeyTk "Tag" <*> pCTag
              <*  pOCURLY <*> pCPatRest <* pVBAR <*> pListSep pCOMMA pCPatFld <* pCCURLY
          )
  <|> CPat_Var <$> pDollNm
  where -- pRPatNm = RPatNmOrig <$> pDollNm <|> RPatNmUniq <$ pKeyTk "uniq" <*> pDollNm
        pCPatRest = pMaybe CPatRest_Empty CPatRest_Var pDollNm

pCPatFld :: CParser CPatFld
pCPatFld
  = (\l o n -> CPatFld_Fld l o n []) <$ pOCURLY <*> pDollNm <* pCOMMA <*> pCExpr <* pCCURLY <* pEQUAL <*> pDollNm -- pCPat
%%]

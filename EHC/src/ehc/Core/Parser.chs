%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Core parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corein) module {%{EH}Core.Parser} import({%{EH}Base.Common}, {%{EH}Base.HsName.Builtin})
%%]

%%[(8 corein) import(UHC.Util.ScanUtils, {%{EH}Scanner.Common}, {%{EH}Scanner.Scanner})
%%]
%%[(8 corein) import(UU.Parsing as P, UHC.Util.ParseUtils, {%{EH}Base.Parser}, {%{EH}Ty.Parser})
%%]
%%[(90 corein) import({%{EH}Foreign.Parser})
%%]

%%[(8 corein) import(Data.Maybe)
%%]

%%[(8 corein) hs import({%{EH}AbstractCore}, {%{EH}Core}, {%{EH}Ty})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corein)
type CParser       hp     =    PlainParser Token hp

pS :: CParser String
pS = pStr
%%]

%%[(8 corein)
pINT		,
%%[[97
  pINTEGER  ,
%%]]
  pCHAR
    :: CParser HsName
pINT = tokMkQName <$> pKeyTk "Int" -- pKeywHsNname hsnInt
pCHAR = tokMkQName <$> pKeyTk "Char" -- pKeywHsNname hsnChar
%%[[97
pINTEGER = tokMkQName <$> pKeyTk "Integer" -- pKeywHsNname hsnInteger
%%]]

pCTy :: CParser Ty
pCTy
  = pTy' (   pDollNm <|> pINT <|> pCHAR
%%[[97
         <|> pINTEGER
%%]]
         )
%%]

%%[(8 corein)
-- | Parse something which is semicolon terminated
pSemiTerminated :: CParser x -> CParser x
pSemiTerminated p = p <* pSEMI

-- | Parse list of something which is semicolon terminated
pListSemiTerminated :: CParser x -> CParser [x]
pListSemiTerminated p = pList (pSemiTerminated p)
%%]

%%[(8 corein) export(pCModule,pCExpr)
pCModule :: CParser CModule
pCModule
  = CModule_Mod
    <$  pMODULE <*> pDollNm <* pSEMI
    <*> pE
    <*> pI
    <*> pM
    <*> pCExpr -- <*> pA (pA pCTag)
  where pM    = pList pCDeclMeta -- pMaybe [] id $ pOCURLY *> pListSep pSEMI pCDeclMeta <* pCCURLY
        pI    = pList pCImport
        pE    = pList pCExport

pCExport :: CParser CExport
pCExport
  =   CExport_Export <$ pEXPORT <*> pDollNm <* pSEMI

pCImport :: CParser CImport
pCImport
  =   CImport_Import <$ pIMPORT <*> pDollNm <* pSEMI

pCDeclMeta :: CParser CDeclMeta
pCDeclMeta
  =   CDeclMeta_Data <$ pDATA <*> pDollNm <* pEQUAL <*> pListSep pCOMMA pCDataCon <* pSEMI

pCDataCon :: CParser CDataCon
pCDataCon = CDataCon_Con <$> pDollNm <* pEQUAL <* pOCURLY <*> pInt <* pCOMMA <*> pInt <* pCCURLY

pCTagTag :: CParser CTag
pCTagTag = pKeyTk "Tag" *> pCTag

pCTagOnly :: CParser CTag
pCTagOnly = pHASH *> pCTagTag

pCNumber :: CParser CExpr
pCNumber
  =    pHASH
       *> (   (   (CExpr_Int     . read) <$ pINT
              <|> (CExpr_Char    . head) <$ pCHAR
              <|> (CExpr_String        ) <$ pKeyTk "String"
%%[[97
              <|> (CExpr_Integer . read) <$ pINTEGER
%%]]
              )
              <*> (tokMkStr <$> pStringTk)
          <|> CExpr_Tup <$ pKeyTk "Tag" <*> pCTag
          )

{-
pCExprAnn :: CParser (CExpr -> CExpr)
pCExprAnn
  =   CExpr_Ann
      <$> (pDCOLON *> (CExprAnn_Ty <$> pTy)
          )
  <|> pSucceed id
-}

pCExprBase :: CParser CExpr
pCExprBase
  =   acoreVar <$> pDollNm
  <|> pCNumber
  <|> pOPAREN *> (pCExpr {- <**> pCExprAnn -}) <* pCPAREN

{-
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
-}

pCExprSel :: CParser CExpr
pCExprSel = pCExprBase -- <??> pCExprSelSuffix

pCExpr :: CParser CExpr
pCExpr
{-
  =   (\f as -> acoreApp f (map fst as))
                     <$> pCExprSel <*> pList pCExprSelMeta
-}
  =   (\f as -> acoreApp f as)
                     <$> pCExprSel <*> pList pCExprSel -- pCExprSelMeta
  <|> mkLam          <$  pLAM <*> pList1 (pDollNm) <* pRARROW <*> pCExpr
  <|> CExpr_Let      <$  pLET <*> pMaybe CBindCateg_Plain id pCBindCateg <*> pListSemiTerminated pCBind <* pIN <*> pCExpr
  <|> (\(c,_) s i t -> CExpr_FFI c s (mkImpEnt c i) t)
                     <$  pFOREIGN <* pOCURLY <*> pFFIWay <* pCOMMA <*> pS <* pCOMMA <*> pS <* pCOMMA <*> pTy <* pCCURLY
  <|> CExpr_Case <$ pCASE <*> pCExpr <* pOF
      <*> pListSemiTerminated pCAlt
      <* pDEFAULT <*> {- pMb -} pSemiTerminated pCExpr
  where pCBindCateg
          =   CBindCateg_Rec    <$ pKeyTk "rec"
          <|> CBindCateg_FFI    <$ pFOREIGN
%%[[90
          <|> CBindCateg_FFE    <$ pKeyTk "foreignexport"
%%]]
          <|> CBindCateg_Strict <$ pBANG
        -- mkLam = acoreLam -- not used to avoid spurious intro of error type info
        mkLam as e = foldr (\n e -> CExpr_Lam (CBind_Bind n []) e) e as
%%[[8
        mkImpEnt c e = e
%%][90
        mkEnt d c e = fst $ parseForeignEnt d c Nothing e
        mkImpEnt c e = mkEnt ForeignDirection_Import c e
%%]]


{-
pTrack          ::   CParser Track
pTrack          =    (\x -> TrackVarApply x [])  <$> pDollNm     -- TODO: this is just a mockup, should do real track parsing
-}

{-
pMbDollNm :: CParser (Maybe HsName)
pMbDollNm
  =  f <$> pDollNm
    where f n | isJust ms && m == "_"
                      = Nothing
                      where ms@(~(Just m)) = hsnMbBaseString n
          f x         = Just x

pManyDollNm :: CParser [HsName]
pManyDollNm
  =  f <$> pList pDollNm
    where -- for backward compatibility with libraries created before 20090917
          f [n] | isJust ms && m == "_"
                      = []
                      where ms@(~(Just m)) = hsnMbBaseString n
          f ns        = ns
-}

{-
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
-}

pCBound :: CParser CBound
pCBound
  = CBound_Bind cmetasDefault <$> pCExpr

pCBind :: CParser CBind
pCBind
  = (\n b -> CBind_Bind n [b]) <$> pDollNm <* pEQUAL <*> pCBound

{-
-- 20100806 AD: due to intro of CBound not consistent with pretty printing anymore, just patched it to have it compiled
pCBind :: CParser CBind
pCBind
  = (  (pDollNm P.<+> pCMetasOpt) <* pEQUAL)
    <**> (   (\e (n,m)        -> CBind_Bind n [CBound_Bind m e]) <$> pCExpr
         <|> (\(c,_) s i t (n,m)  -> CBind_Bind n [CBound_Bind m $ CExpr_FFI c s (mkImpEnt c i) t])
             <$ pFOREIGN <* pOCURLY <*> pFFIWay <* pCOMMA <*> pS <* pCOMMA <*> pS <* pCOMMA <*> pTy <* pCCURLY
%%[[90
         <|> (\(c,_) e en t (n,m) -> CBind_Bind n [CBound_FFE c (mkEnt ForeignDirection_Export c e) en t])
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
-}

pCAlt :: CParser CAlt
pCAlt
  =   (\p e -> CAlt_Alt p e) <$> pCPat <* pRARROW <*> pCExpr

pCPat :: CParser CPat
pCPat
  =   pHASH
       *> (   (   (CPat_Int  . read) <$ pINT
              <|> (CPat_Char . head) <$ pCHAR
              )
              <*> (tokMkStr <$> pStringTk)
          -- <|> (\t r fs -> CPat_Con t r $ zipWith (\o (mf,n) -> acorePatFldTy (acoreTyErr "pCPatFld") (maybe (n, CExpr_Int o) id mf) n) [0..] fs)		-- TODO, use refGen instead of baked in 0.. ...
          <|> 
              -- TODO, use refGen instead of baked in 0.. ...
              (\t r fs -> CPat_Con t r $ zipWith (\o (mf,n) -> let (lbl',o') = fromMaybe (n, CExpr_Int o) mf
                                                                in CPatFld_Fld lbl' o' (CBind_Bind n []) []) [0..] fs)
              <$> pCTagTag
              <*  pOCURLY <*> pCPatRest <*> pListSep pCOMMA pCPatFld <* pCCURLY
          )
  <|> CPat_Var <$> pDollNm
  where -- pRPatNm = RPatNmOrig <$> pDollNm <|> RPatNmUniq <$ pKeyTk "uniq" <*> pDollNm
        pCPatRest = pMaybe CPatRest_Empty CPatRest_Var (pDollNm <* pVBAR)

-- pCPatFld :: CParser CPatFld
pCPatFld :: CParser (Maybe (HsName,CExpr),HsName)
pCPatFld
  -- = (\l o n -> CPatFld_Fld l o n []) <$ pOCURLY <*> pDollNm <* pCOMMA <*> pCExpr <* pCCURLY <* pEQUAL <*> pCBind -- pCPat
  -- = (\l o n -> acorePatFldTy (acoreTyErr "pCPatFld") (l, o) n) <$ pOCURLY <*> pDollNm <* pCOMMA <*> pCExpr <* pCCURLY <* pEQUAL <*> pDollNm -- pCPat
  = pLblOff <+> pDollNm -- pCPat
  where pLblOff = pMb $ (,) <$ pOCURLY <*> pDollNm <* pCOMMA <*> pCExpr <* pCCURLY <* pEQUAL
%%]

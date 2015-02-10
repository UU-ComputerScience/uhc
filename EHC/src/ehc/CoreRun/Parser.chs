%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CoreRun parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) module {%{EH}CoreRun.Parser} import({%{EH}Base.Common}, {%{EH}Base.HsName.Builtin})
%%]

%%[(8 corerun) import(UHC.Util.ScanUtils, {%{EH}Scanner.Common}, {%{EH}Scanner.Scanner})
%%]
%%[(8 corerun) import(UU.Parsing as P, UHC.Util.ParseUtils, {%{EH}Base.Parser})
%%]

%%[(8 corerun) import(Data.Maybe)
%%]

%%[(8 corerun) hs import({%{EH}CoreRun})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) export(parseModFromString)
-- | Parses a module. TBD: integration with other parser utils from EHC driver...
parseModFromString :: String -> Either [String] Mod
parseModFromString str = case parseToResMsgs pMod $ scan corerunScanOpts (initPos $ take 80 str) str of
    (res, []) -> Right res
    (_, errs) -> Left $ map show errs
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun)
pDifficultNm :: CRParser HsName
pDifficultNm = (\s -> {- parseHsName [s] -} mkHNm s) <$> pStr
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun)
type CRParser hp = PlainParser Token hp
%%]

%%[(8 corerun)
-- | Parse module 'Mod'
pMod :: CRParser Mod
pMod
  = (\nm nr sz main is ms bs -> mkModWithImportsMetas nm nr sz is ms (crarrayFromList bs) main)
    <$  pMODULE <*> pMaybe (mkHNm "Main") id pDollNm <*> pInt <* pCOMMA <*> pInt <*> pMb (pRARROW *> pExp) <* pSEMI
    <*> pList (pImport <* pSEMI)
    <*> pList (pMeta <* pSEMI)
    <*> pList (pExp <* pSEMI)

-- | Parse 'Import'
pImport :: CRParser Import
pImport
  = Import_Import <$ pIMPORT <*> pDollNm

-- | Parse 'Meta'
pMeta :: CRParser Meta
pMeta
  = Meta_Data <$ pDATA <*> pDifficultNm <* pEQUAL <*> pListSep pCOMMA pDataCon

-- | Parse 'DataCon'
pDataCon :: CRParser DataCon
pDataCon = DataCon_Con <$> pDifficultNm <* pRARROW <*> pInt

-- | Parse simple expression 'SExp'
pSExp :: CRParser SExp
pSExp
  =    mkInt' <$> pInt
  <|> (mkChar' . head) <$> pChar
  <|> mkString' <$> pString
  <|> mkVar' <$> pRRef

-- | Parse expression 'Exp'
pExp :: CRParser Exp
pExp = pE
  where pB =   mkExp <$> pSExp
           <|> pParens pE
           <|> mkEval <$ pKeyTk "eval" <*> pB
           <|> mkTail <$ pKeyTk "tail" <*> pB
        pE =   pB
           <|> (   mkApp <$ pKeyTk "app"   <*> pB
               <|> mkTup <$ pKeyTk "alloc" <*> pInt 
               <|> mkFFI <$ pKeyTk "ffi"   <*> pString 
               ) <*> pParens (pListSep pCOMMA pSExp)
           <|> dbg <$ pKeyTk "dbg" <*> pString
           <|> mkCase <$ pCASE <*> pSExp <* pOF <*> pList1 (pRARROW *> pE <* pSEMI)
           <|> mkLet  <$ pLET  <*> pInt  <* pRARROW <*> pList1 (pE <* pSEMI) <* pIN <*> pE
           <|> mkLam  <$ pLAM  <*> pInt  <* pCOMMA <*> pInt <* pRARROW <*> pE

-- | Parse reference RRef to something
pRRef :: CRParser RRef
pRRef
  = (\b sufs -> foldl (flip ($)) b sufs) <$> pB <*> pList_ng pS
  where pB =   ( (   mkLocDifRef <$ pKeyTk "d"
                 <|> mkGlobRef   <$ pKeyTk "g"
                 <|> mkImpRef    <$ pKeyTk "i"
                 <|> mkLocLevRef <$ pKeyTk "l"
                 ) <* pDOT <*> pInt <* pDOT <*> pInt
               )
           <|> ( mkModRef <$ pKeyTk "m" <* pDOT <*> pInt
               )
        pS = pDOT
              *> (   RRef_Tag <$ pKeyTk "tag"
                 <|> flip RRef_Fld <$> pInt
                 )

%%]

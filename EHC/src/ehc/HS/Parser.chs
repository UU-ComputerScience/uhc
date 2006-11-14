%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}HS.Parser} import(IO, UU.Parsing, UU.Parsing.Offside, EH.Util.ParseUtils, UU.Scanner.GenToken, EH.Util.ScanUtils, {%{EH}Base.Common}, {%{EH}Base.Builtin}, {%{EH}Scanner.Common}, {%{EH}HS})
%%]

%%[1 export(pAGItf, HSParser)
%%]

%%[12 export(pFixity, pAGItfImport, HSParser')
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scanner related
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
tokConcat :: Token -> Token -> Token
tokConcat t1 t2 = Reserved (genTokVal t1 ++ genTokVal t2) (position t1)

tokEmpty :: Token
tokEmpty = Reserved "" noPos
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
type HSParser         ep    =    LayoutParser Token ep
type HSParser'        ep    =    PlainParser Token ep
%%]

%%[1
pAGItf :: HSParser AGItf
pAGItf
  =   AGItf_AGItf <$> pModule pBody
%%]

%%[12
pAGItfImport :: HSParser AGItf
pAGItfImport
  =   AGItf_AGItf <$> pModule pBodyImport
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Abstractions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
pPacked' :: HSParser Token -> HSParser Token -> HSParser (Range -> v) -> HSParser v
pPacked' pO pC pMk = (\o mk c -> mk (mkRange2 o c)) <$> pO <*> pMk <*> pC

pParens' :: HSParser (Range -> v) -> HSParser v
pParens' = pPacked' pOPAREN pCPAREN

pBracks' :: HSParser (Range -> v) -> HSParser v
pBracks' = pPacked' pOBRACK pCBRACK

pCurlys' :: HSParser (Range -> v) -> HSParser v
pCurlys' = pPacked' pOCURLY pCCURLY
%%]

%%[9
pImpls' :: HSParser (Range -> v) -> HSParser v
pImpls' = pPacked' pOIMPL pCIMPL

pImpls :: IsParser p Token => p v -> p v
pImpls = pPacked pOIMPL pCIMPL
%%]

%%[1.pApp
pApp            ::   SemApp ep => HSParser ep -> HSParser ep
pApp p          =    mkApp <$> pList1 p
%%]

%%[1
%%]
block items
  =    pOCURLY   *>  items <* pCCURLY
  <|>  pVOCURLY  *>  items <* close

close :: HSParser Token
close = pVCCURLY

close :: HParser () 
close = pWrap f g (pVCCURLY)
  where g state steps1 k = (state,ar,k)
          where ar = if not (hasSuccess steps1) 
                       then case unP popContext state of
                             POk state' _   -> let steps2 = k state'
                                               in  if  hasSuccess steps2 then steps2 else steps1                      
                             _              -> steps1  
                       else steps1                             
        f acc state steps k = let (stl,ar,str2rr) = g state (val snd steps)  k
                              in (stl ,val (acc (return ())) ar , str2rr )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
pModule :: HSParser Body -> HSParser Module
%%]
%%[1.pModule
pModule pBody
  =   (\b -> Module_Module emptyRange Nothing b) <$> pBody
  <|> (\t m b -> Module_Module (mkRange1 t) (Just $ tokMkQName $ m) b) <$> pMODULE <*> modid <* pWHERE <*> pBody
%%]
%%[12.pModule -1.pModule
pModule pBody
  =   (\b -> Module_Module emptyRange Nothing Nothing b) <$> pBody
  <|> (\t m e b -> Module_Module (mkRange1 t) (Just $ tokMkQName $ m) e b) <$> pMODULE <*> modid <*> pMaybeExports <* pWHERE <*> pBody
%%]

%%[1.pBody
pBody :: HSParser Body
pBody
  =   Body_Body emptyRange <$> pDeclarations1' pTopDeclaration
  <|> pSucceed (Body_Body emptyRange [])
%%]
%%[12 -1.pBody
pBody :: HSParser Body
pBody
  =   (\ids -> let (i,d) = foldr cmbid ([],[]) ids in Body_Body emptyRange i d)
      <$> pDeclarations1' ((\d -> ([],[d])) <$> pTopDeclaration <|> (\i -> ([i],[])) <$> pImportDeclaration)
  <|> pSucceed (Body_Body emptyRange [] [])
  where cmbid ([i],_) (is,ds) = (i:is,ds)
        cmbid (_,[d]) (_ ,ds) = ([],d:ds)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module header + import only
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12
pBodyImport :: HSParser Body
pBodyImport
  =   (\d -> Body_Body emptyRange d []) <$> pDeclarations' pImportDeclaration
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Export, import
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12
pImportExport :: (Range -> Name -> ie,Range -> Name -> MaybeNames -> ie,Range -> Name -> ie) -> HSParser ie
pImportExport (sem_Var,sem_tOrC,sem_tOrC_complete)
  =   mkRngNm sem_Var <$> qvar
  <|> oqtycon
      <**> (   pParens
                 (   (\c n -> mkRngNm sem_tOrC n (Just (tokMkQNames c))) <$> qcnames
                 <|> mkRngNm sem_tOrC_complete <$ pDOTDOT
                 )
           <|> pSucceed (\n -> mkRngNm sem_tOrC n Nothing)
           )
%%]

%%[12
pExport :: HSParser Export
pExport
  =   (\t m -> Export_Module (mkRange1 t) (tokMkQName m)) <$> pMODULE <*> modid
  <|> pImportExport (Export_Variable,Export_TypeOrClass,Export_TypeOrClassComplete)

pMaybeExports :: HSParser MaybeExports
pMaybeExports = Just <$> pParens (pListSep pCOMMA pExport) <|> pSucceed Nothing         
%%]

%%[12
pImport :: HSParser Import
pImport
  =   pImportExport (Import_Variable,Import_TypeOrClass,Import_TypeOrClassComplete)

pImportDeclaration :: HSParser ImportDeclaration
pImportDeclaration
  = (\q m a i -> ImportDeclaration_Import (mkRange1 m) q (tokMkQName m) (fmap tokMkQName a) i)
    <$  pIMPORT <*> (True <$ pQUALIFIED <|> pSucceed False)
    <*> modid
    <*> (Just <$ pAS <*> modid <|> pSucceed Nothing)
    <*> (Just <$> pImportSpecification <|> pSucceed Nothing)
  where pImportSpecification :: HSParser ImportSpecification
        pImportSpecification
          = (True <$ pHIDING <|> pSucceed False)
            <**> pParens'
                   ((\i r h -> ImportSpecification_Import r h i) <$> pListSep pCOMMA pImport)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Declarations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
pDeclaration :: HSParser Declaration
pDeclaration
  =   pDeclarationValue
  <|> pDeclarationSignature
  <|> pDeclarationFixity
%%]
%%[5
  <|> pDeclarationData
%%]
%%[9
  <|> pDeclarationClass
  <|> pDeclarationInstance
%%]
%%[11
  <|> pDeclarationType
%%]

%%[1
pTopDeclaration :: HSParser Declaration
pTopDeclaration
  =   pDeclaration
%%]
%%[8
  <|> pDeclarationForeign
%%]
%%[9
  <|> pDeclarationDefault
%%]

%%[1
pDeclarations' :: HSParser d -> HSParser [d]
pDeclarations' pD
  =   pBlock pOCURLY pSEMI pCCURLY pD

pDeclarations1' :: HSParser d -> HSParser [d]
pDeclarations1' pD
  =   pBlock1 pOCURLY pSEMI pCCURLY pD

pDeclarations :: HSParser Declarations
pDeclarations
  =   pDeclarations' pDeclaration

pDeclarations1 :: HSParser Declarations
pDeclarations1
  =   pDeclarations1' pDeclaration
%%]

%%[1
pWhere' :: HSParser Declaration -> HSParser MaybeDeclarations
pWhere' pD = Just <$ pWHERE <*> pDeclarations' pD <|> pSucceed Nothing

pWhere :: HSParser MaybeDeclarations
pWhere = pWhere' pDeclaration
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fixity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
pDeclarationFixity :: HSParser Declaration
pDeclarationFixity
  = (\f p o -> Declaration_Fixity emptyRange f p (tokMkQNames o))
    <$> pFixity
    <*> ((Just . tokMkInt) <$> pInteger10Tk <|> pSucceed Nothing)
    <*> pListSep pCOMMA op

pFixity :: HSParser' Fixity
pFixity = Fixity_Infixl <$ pINFIXL <|> Fixity_Infixr <$ pINFIXR <|> Fixity_Infix <$ pINFIX
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Value definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
pDeclarationSignature :: HSParser Declaration
pDeclarationSignature
  =   (\(v:vs) t -> Declaration_TypeSignature (mkRange1 v) (tokMkQNames (v:vs)) t)
      <$> pList1Sep pCOMMA var <* pDCOLON <*> pType

pDeclarationValue :: HSParser Declaration
pDeclarationValue
  =   (\l r -> Declaration_FunctionBindings emptyRange [FunctionBinding_FunctionBinding emptyRange l r]) <$> pLhs <*> rhs
  <|> Declaration_PatternBinding emptyRange <$> pPatternOp <*> rhs
  where rhs = pRhs pEQUAL
%%]

%%[8
pDeclarationSimpleValue :: HSParser Declaration
pDeclarationSimpleValue
  =   Declaration_PatternBinding emptyRange <$> lhs <*> rhs
  where lhs = mkRngNm Pattern_Variable <$> var
        rhs = (\t e -> RightHandSide_Expression (mkRange1 t) e Nothing) <$> pEQUAL <*> pExpression
%%]

%%[1
pRhs :: HSParser Token -> HSParser RightHandSide
pRhs pSep
  =   (RightHandSide_Expression . mkRange1) <$> pSep <*> pExpression <*> pWhere
%%]
%%[5
  <|> RightHandSide_Guarded emptyRange
      <$> pList1 ((GuardedExpression_GuardedExpression . mkRange1) <$> pVBAR <*> pExpression <* pSep <*> pExpression)
      <*> pWhere
%%]

%%[1
pLhs :: HSParser LeftHandSide
pLhs
  =   mkRngNm LeftHandSide_Function <$> qvar <*> pLhsTail
  <|> pParens' ((\l r t -> LeftHandSide_Parenthesized r l t) <$> pLhs) <*> pLhsTail
  <|> (\l o r -> LeftHandSide_Infix (mkRange1 o) l (tokMkQName o) r) <$> pPatternOp <*> varop <*> pPatternOp
  where pLhsTail = pList1 pPatternBase
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Data definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[5
pDeclarationData :: HSParser Declaration
pDeclarationData
  =   pD pDATA    (Declaration_Data    . mkRange1) (pEQUAL *> pListSep pVBAR pConstructor <|> pSucceed [])
  <|> pD pNEWTYPE (Declaration_Newtype . mkRange1) (pEQUAL *> pConstructor)
  where pD pK sem pC
          = sem <$> pK
%%[[9
            <*> pContextItemsPrefixOpt
%%]]
            <*> pSimpleType <*> pC
%%[[9
            <*> (pDERIVING *> ((:[]) <$> pDeriving <|> pParens (pList1Sep pCOMMA pDeriving)) <|> pSucceed [])
%%]]
%%]

%%[9
pDeriving :: HSParser Deriving
pDeriving
  = mkRngNm Deriving_Deriving <$> qconid
%%]

%%[5.pConstructor
pConstructor :: HSParser Constructor
pConstructor
  =   con
      <**> (   (\ts c -> mkRngNm Constructor_Constructor c ts) <$> pList pTB
%%]
%%[7
           <|> pCurlys' ((\fs r c -> mkRngNm Constructor_Record c fs) <$> pList1Sep pCOMMA pFieldDeclaration)
%%]
%%[5
           )
  <|> (\l o r -> Constructor_Infix (mkRange1 o) l (tokMkQName o) r) <$> pT <*> conop <*> pT
  where pT  = pAnnotatedType pType
        pTB = pAnnotatedType pTypeBase
%%]

%%[7
pFieldDeclaration :: HSParser FieldDeclaration
pFieldDeclaration
  = (\vs@(v:_) -> FieldDeclaration_FieldDeclaration (mkRange1 v) (tokMkQNames vs))
    <$> pList1Sep pCOMMA var <* pDCOLON <*> pAnnotatedType pType
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Foreign
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
pDeclarationForeign :: HSParser Declaration
pDeclarationForeign
  = pFOREIGN
    <**> (   (\c s (i,n,t) r -> Declaration_ForeignImport (mkRange1 r) (tokMkStr c) s i (tokMkQName n) t)
             <$ pIMPORT <*> callconv <*> pSafety <*> pFSpec
         <|> (\c (i,n,t) r -> Declaration_ForeignExport (mkRange1 r) (tokMkStr c) i (tokMkQName n) t)
             <$ pEXPORT <*> callconv <*> pFSpec
         )
  where pSafety =  (Just . tokMkStr) <$> safety <|> pSucceed Nothing
        pFSpec = (,,) <$> ((Just . tokMkStr) <$> pStringTk <|> pSucceed Nothing) <*> var{-id_no_foreign-} <* pDCOLON <*> pType
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Class & Instance
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
pDeclarationClass :: HSParser Declaration
pDeclarationClass
  = (\t -> Declaration_Class (mkRange1 t))
    <$> pCLASS
    <*> pContextItemsPrefixOpt <*> pSimpleType
    <*> (pVBAR *> pListSep pCOMMA pFunctionalDependency
        `opt` []
        )
    <*> pWhere' (pDeclarationValue <|> pDeclarationSignature)
  where pFunctionalDependency :: HSParser FunctionalDependency
        pFunctionalDependency
          = (\vs1@(v:_) vs2 -> FunctionalDependency_Dependency (mkRange1 v) (tokMkQNames vs1) (tokMkQNames vs2))
            <$> pList1 tyvar <* pRARROW <*> pList1 tyvar
%%]

%%[9
pDeclarationInstance :: HSParser Declaration
pDeclarationInstance
  = pINSTANCE
    <**> (   (\(n,u) c cl ts d t -> Declaration_Instance (mkRange1 t) n u c (tokMkQName cl) ts d)
             <$> ((\n e -> (Just (tokMkQName n),e)) <$> varid <*> (True <$ pLTCOLON <|> False <$ pDCOLON) <|> pSucceed (Nothing,True))
             <*> pContextItemsPrefixOpt <*> qconid <*> pList1 pType
             <*> pWhere' pDeclarationValue
         <|> (\e cl ts t -> Declaration_InstanceUseImplicitly (mkRange1 t) e (tokMkQName cl) ts)
             <$> pExpression <* pLTCOLON <*> qconid <*> pList1 pType
         )
%%]

%%[9
pDeclarationDefault :: HSParser Declaration
pDeclarationDefault
  = (Declaration_Default . mkRange1) <$> pDEFAULT <*> pParens (pListSep pCOMMA pType)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Type synomym
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[11
pDeclarationType :: HSParser Declaration
pDeclarationType
  =   (Declaration_Type . mkRange1) <$> pTYPE <*> pSimpleType <* pEQUAL <*> pType
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Type
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
pTypeBase :: HSParser Type
pTypeBase
  =   mkRngNm Type_Constructor <$> gtycon_no_delims
%%]
%%[2.pTypeBase
  <|> (Type_Wildcard . mkRange1) <$> pTDOT
%%]
%%[3.pTypeBase
  <|> mkRngNm Type_Variable <$> tyvar
  <|> mkRngNm Type_NamedWildcard <$ pPERCENT <*> tyvar
%%]
%%[5
  <|> pBracks'
        (   (\t r -> Type_Application r False (Type_Constructor r hsnList) [t])
            <$> pType
        <|> pSucceed (\r -> Type_Constructor r hsnList)
        )
%%]
%%[1.pTypeBase.prod
  <|> pParens' pInParens
  where pInParens :: HSParser (Range -> Type)
        pInParens
          =   (pType
               <**> (   (\es e r -> Type_Application r False (Type_Constructor r $ hsnProd $ length es + 1) (e:es))
                        <$>  pList1 (pComma *> pType)
                    <|> pSucceed (flip Type_Parenthesized)
              )     )
          <|> pSucceed (\r -> Type_Constructor r (hsnProd 0))
%%]
%%[7 -1.pTypeBase.prod
  <|> pParenRow True pOROWREC pCROWREC pDCOLON False undefined undefined
         (Type_RowRecEmpty,Type_Variable,RowTypeUpdate_Extends,Type_RowRecUpdate,Type_Parenthesized)
         qvarid pType
  <|> pParenRow False pOROWROW pCROWROW pDCOLON False undefined undefined
         (Type_RowEmpty,Type_Variable,RowTypeUpdate_Extends,Type_RowUpdate,const id)
         qvarid pType
  <|> pParenRow False pOROWSUM pCROWSUM pDCOLON False undefined undefined
         (Type_RowSumEmpty,Type_Variable,RowTypeUpdate_Extends,Type_RowSumUpdate,const id)
         qvarid pType
%%]

%%[1.pType
pType :: HSParser Type
pType =  pChainr (mk1Arrow <$ pRARROW) pTypeBase
%%]
%%[4.pType -1.pType
pType ::  HSParser Type
pType =   pTypePrefix <*> pType
      <|> pTypeApp <??> (flip mk1Arrow <$ pRARROW <*> pType)
%%]

%%[4.pTypePrefix
pTypePrefix :: HSParser (Type -> Type)
pTypePrefix
  =  ((Type_Forall . mkRange1) <$> pFORALL <|> (Type_Exists . mkRange1) <$> pEXISTS)
     <*> (tokMkQNames <$> pTyVarBinds) <* pDOT
%%]
%%[9.pTypePrefix
  <|>  pTyPrefixContextItems
%%]

%%[4.pTypeApp
pTypeApp :: HSParser Type
pTypeApp =  pTypeBase
%%]
%%[5.pTypeApp -4.pTypeApp
pTypeApp :: HSParser Type
pTypeApp =  pApp pTypeBase
%%]

%%[4
pTyVarBind :: HSParser Token
pTyVarBind =  tyvar

pTyVarBinds :: HSParser [Token]
pTyVarBinds =  pList1 pTyVarBind
%%]

%%[5
pSimpleType :: HSParser SimpleType
pSimpleType
  = mkRngNm SimpleType_SimpleType <$> gtycon <*> (tokMkQNames <$> pList tyvar)
%%]

%%[5
pAnnotatedType :: HSParser Type -> HSParser AnnotatedType
pAnnotatedType pT
  = AnnotatedType_Type emptyRange False <$> pT
%%]

%%[9.pTyPrefixContextItems
pContextItemsPrefix :: HSParser ContextItems
pContextItemsPrefix
  =   mkL
      <$> pImpls'
            (    const <$> (pContextItem <|> pIm)
            <|>  pSucceed ContextItem_NoImplicits
            )
      <*  pRARROW
  <|> (   mkL <$> (pContextItemBase <|> pIm)
      <|> pParens ((:) <$> pContextItem
                       <*> (   pImO
                           <|> (++) <$> pList1 (pCOMMA *> pContextItem) <*> pImO
                  )        )
      )
      <*  pDARROW
  where pIm   =   (ContextItem_Implicits . mkRange1) <$> pTDOT
        pImO  =   mkL <$ pCOMMA <*> pIm `opt` []
        mkL   =   (:[])
        pIm  :: HSParser ContextItem
        pImO :: HSParser ContextItems

pContextItemsPrefixOpt :: HSParser ContextItems
pContextItemsPrefixOpt = pContextItemsPrefix <|> pSucceed []

pTyPrefixContextItems :: HSParser (Type -> Type)
pTyPrefixContextItems
  = Type_Qualified emptyRange <$> pContextItemsPrefix
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for Predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
pContextItemClass :: HSParser ContextItem
pContextItemClass
  =    mkRngNm ContextItem_Class <$> qconid <*> pList1 pType
%%]

%%[9
pContextItemPrefix :: HSParser (ContextItem -> ContextItem)
pContextItemPrefix
  =   ContextItem_Arrow emptyRange <$> pContextItemBase <* pDARROW
  <|> (ContextItem_Forall . mkRange1) <$> pFORALL <*> (tokMkQNames <$> pTyVarBinds) <* pDOT
%%]

%%[9
pContextItem :: HSParser ContextItem
pContextItem
  =   pContextItemPrefix <*> pContextItem
  <|> pContextItemBase
%%]

%%[9
%%]
pTyContextItem :: HSParser Type
pTyContextItem
  =   pTyPrefixContextItems <*> pTyContextItem
  <|> (\c -> Type_Qualified emptyRange [c]) <$> pContextItemBase

%%[9
pContextItemBase ::   HSParser ContextItem
pContextItemBase
  =   pContextItemClass
  <|> pParens pContextItem
%%]
%%[1010
  <|>  ContextItem_DynVar <$> pDynVar <* pKey "::" <*> pType
%%]
%%[10
  <|> tyvar <**>  (    (\s v -> mkRngNm ContextItem_RowLacksLabel v (tokMkQName s))
                       <$ pLAM <*> pSelector
%%]
%%[50
                  <|>  (flip ContextItem_Equal)
                       <$ pKey "=" <*> pType
%%]
%%[10
                  )
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Literal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
pLiteralNumber :: HSParser Literal
pLiteralNumber
  =   mkRngStr Literal_Int  <$> pIntegerTk

pLiteral :: HSParser Literal
pLiteral
  =   pLiteralNumber
  <|> mkRngStr Literal_Char <$> pCharTk
%%]
%%[5
  <|> mkRngStr Literal_String <$> pStringTk
%%]
%%[8
  <|> mkRngStr Literal_Float  <$> pFractionTk
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Expression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
pExpressionBase :: HSParser Expression
pExpressionBase
  =   Expression_Literal emptyRange  <$> pLiteral
  <|> mkRngNm Expression_Variable    <$> qvarid
%%[[5
  <|> pExpressionList
%%]]
%%]
%%[1
  <|> pParens' pInParens
  where pInParens :: HSParser (Range -> Expression)
        pInParens
          =   (pExpression
               <**> (   (\(o,_) e r -> Expression_SectionApplication r (Just e) o Nothing) <$> pOp
%%]
%%[1.parenExprAsProdA
                    <|> pSucceed (flip Expression_Parenthesized)
                    <|> (\es e r -> Expression_Tuple r (e:es))
                        <$>  pList1 (pComma *> pExpression)
%%]
%%[1
              )     )
          <|> (pOpm
               <**> (   (\e (o,_) r -> Expression_SectionApplication r Nothing o (Just e)) <$> pExpression
                    <|> pSucceed (\(o,_) r -> Expression_SectionApplication r Nothing o Nothing)
              )     )
%%]
%%[1.parenExprAsProdB
          <|> pSucceed (\r -> Expression_Constructor r (hsnProd 0))
%%]
%%[7 -(1.parenExprAsProdA 1.parenExprAsProdB)
          <|> pParenRow' True pOROWREC pCROWREC pEQUAL True pCOLEQUAL RowRecordExpressionUpdate_Update
                 (Expression_RowRecordEmpty,Expression_Variable,RowRecordExpressionUpdate_Extends,Expression_RowRecordUpdate,Expression_Parenthesized)
                 qvarid pExpression
%%]

%%[5
pExpressionList :: HSParser Expression
pExpressionList
  = pBracks'
      (pExpression
       <**> (   pDOTDOT
                *> (     (\e3 e1 r -> Expression_Enum r e1 Nothing (Just e3)) <$> pExpression
                   `opt` (\   e1 r -> Expression_Enum r e1 Nothing  Nothing )
                   )
            <|> pCOMMA
                *> (pExpression
                    <**> (   pDOTDOT
                             *> (     (\e3 e2 e1 r -> Expression_Enum r e1 (Just e2) (Just e3)) <$> pExpression
                                `opt` (\   e2 e1 r -> Expression_Enum r e1 (Just e2)  Nothing )
                                )
                         <|> (\es e2 e1 r -> Expression_List r (e1:e2:es)) <$> pList (pComma *> pExpression)
                   )     )
            <|> pVBAR
                *> ((\c e r -> Expression_Comprehension r e (c ++ [Qualifier_Empty emptyRange])) <$> pListSep pCOMMA pQualifier) 
            `opt` flip one
            )
      `opt` zero
      )
  where zero r   = Expression_List r []
        one  r h = Expression_List r [h]
        pQualifier :: HSParser Qualifier
        pQualifier
          =   Qualifier_Guard emptyRange <$> pExpressionNoLet
          <|> (Qualifier_Let . mkRange1) <$> pLET <*> pDeclarations
          <|> Qualifier_Generator emptyRange <$> pPattern <* pLARROW <*> pExpression
%%]

%%[88.pExprBase
                <|>  Expr_Undefined  <$   pKey "..."
%%]
%%[1010.pExprBase
                <|>  Expr_DynVar     <$>  pDynVar
%%]

%%[9
pExpressionDo :: HSParser Expression
pExpressionDo
  = (Expression_Do . mkRange1) <$> pDO <*> pBlock1 pOCURLY pSEMI pCCURLY pStatement
  where pStatement :: HSParser Statement
        pStatement
          =   Statement_Expression emptyRange <$> pExpressionNoLet
          <|> (\p t e -> Statement_Generator (mkRange1 t) p e) <$> pPattern <*> pLARROW <*> pExpression
          <|> (Statement_Let . mkRange1) <$> pLET <*> pDeclarations
%%]

%%[1
pExpressionConUpd :: HSParser Expression
pExpressionConUpd
  =   gcon
      <**> (   pSucceed (mkRngNm Expression_Constructor)
%%[[7
           <|> pCurlys' ((\bs _ c -> mkRngNm Expression_RecordConstruction c bs) <$> pListSep pCOMMA pRecordExpressionBinding)
%%]]
           )
  <|> pExpressionBase
%%[[7
      <**> ((\u e -> foldr ($) e u) <$> pList pU)
  where pU =   pCurlys' ((\bs r e -> Expression_RecordUpdate r e bs) <$> pList1Sep pCOMMA pRecordExpressionBinding)
           <|> pRowRecordSelectionSuffix
%%]]
%%]

%%[7
pRecordExpressionBinding :: HSParser RecordExpressionBinding
pRecordExpressionBinding
  = mkRngNm RecordExpressionBinding_Binding <$> qvar <* pEQUAL <*> pExpression
%%]

%%[1.pExpressionApp
pExpressionApp :: HSParser Expression
pExpressionApp
  = pE <**> ((\as e -> foldl (flip ($)) e as) <$> pList pA)
  where pE =   pExpressionConUpd
        pA =   (\es e -> Expression_NormalApplication emptyRange e es) <$> pList1 pE
%%[[4
           <|> (\es e -> Expression_ImpredicativeApplication emptyRange e es) <$> pList1 (pTILDE *> pE)
%%]]
%%[[9
           <|> (\es e -> Expression_ImplicitApplication emptyRange e es) <$> pList1 (pImpls' pContextedExpression)
           where pContextedExpression = (\e c r -> ContextedExpression_Contexted r e c) <$> pExpression <* pLTCOLON <*> pContextItem
                 pContextedExpression :: HSParser (Range -> ContextedExpression)
%%]]
%%]

%%[1
pExpressionLayout :: HSParser Expression
pExpressionLayout
  =   pExpressionApp
%%[[5
  <|> (Expression_Case . mkRange1) <$> pCASE <*> pExpression <* pOF <*> pAlternatives
%%]]
%%[[9
  <|> pExpressionDo
%%]]
%%]

%%[1
pOp, pOpm :: HSParser (Expression,Range)
pOp  = mkRngNm' Expression_Variable <$> qvarop  <|> mkRngNm' Expression_Constructor <$> qconop
pOpm = mkRngNm' Expression_Variable <$> qvaropm <|> mkRngNm' Expression_Constructor <$> qconop
%%]

%%[1
pExpressionOpPrefix :: HSParser Expression -> HSParser (Expression -> Expression)
pExpressionOpPrefix pLeftOpndE
  = (\l e -> Expression_InfixApplicationChainTop emptyRange
             $ foldr (\(l,(op,rng)) r -> Expression_InfixApplication rng l op r) e l
    )
    <$> pList1_ng ((,) <$> pLeftOpndE <*> pOp)
%%]

%%[1
pExpression' :: HSParser (Expression -> Expression) -> HSParser Expression
pExpression' pPreE
  = pE1 <??> ((\c t e -> Expression_Typed (mkRange1 c) e t) <$> pDCOLON <*> pType)
  where pE1, pE2 :: HSParser Expression
        pE1  = pExpressionOpPrefix pExpressionLayout <*> pE2 <|> pE2
        pE2  = pPre1 <*> pExpressionLayout <|> pExpressionLayout
        pPre1 :: HSParser (Expression -> Expression)
        pPre1 = foldr (.) id <$> pList1 pPreE
%%]
pExpression' :: HSParser (Expression -> Expression) -> HSParser Expression
pExpression' pPreE
  = (pE1 <|> pE2) <??> ((\c t e -> Expression_Typed (mkRange1 c) e t) <$> pDCOLON <*> pType)
  where pE1, pE2 :: HSParser Expression
        pE1  = pExpressionOpPrefix pExpressionLayout <*> (pPre1 <*> pE1 <|> pExpressionLayout)
        pE2  = pPre1 <*> (pExpressionOpPrefix pExpressionLayout <*> pE2 <|> pExpressionLayout)
        pPre1 :: HSParser (Expression -> Expression)
        pPre1 = foldr (.) id <$> pList1 pPreE

%%[1
pExpression :: HSParser Expression
pExpression
  = pExpression' pExpressionPrefix
%%]

%%[1
pExpressionNoLet :: HSParser Expression
pExpressionNoLet
  = pExpression' pExpressionNoLetPrefix
%%]

%%[1.pExpressionLetPrefix
pExpressionLetPrefix :: HSParser (Expression -> Expression)
pExpressionLetPrefix
%%[[1
  =   (Expression_Let . mkRange1)
      <$> pLET
      <*> pDeclarations <* pIN
%%][8
  =   (\(s,t,d) -> Expression_Let (mkRange1 t) s d)
      <$> (   (,,) False <$> pLET       <*> pDeclarations                          <* pIN
          <|> (,,) True  <$> pLETSTRICT <*> pDeclarations' pDeclarationSimpleValue <* pIN
          )
%%]]
%%]

%%[1.pExpressionNoLetPrefix
pExpressionNoLetPrefix :: HSParser (Expression -> Expression)
pExpressionNoLetPrefix
  =   (Expression_Negate . mkRange1) <$> pMINUS
%%[[5
  <|> (Expression_If . mkRange1) <$> pIF <*> pExpression <* pTHEN <*> pExpression <* pELSE
%%]]
  <|> pLAM <**> pLamArgs
  where pLamArgs
          =   (\a1 a2 t e -> a1 t (a2 t e))
              <$> (   (\ps t e -> Expression_Lambda (mkRange1 t) ps e) <$> pList1 pPatternBase
%%[[9
                  <|> (\ps t e -> Expression_ImplicitLambda (mkRange1 t) ps e) <$> pList1 (pImpls' pContextedPattern)
%%]]
                  )
              <*> pLamArgs
          <|> (\_ e -> e) <$ pRARROW
%%[[9
        pContextedPattern = (\p c r -> ContextedPattern_Contexted r p c) <$> pPattern <* pLTCOLON <*> pContextItem
        pContextedPattern :: HSParser (Range -> ContextedPattern)
%%]]
%%]

%%[1.pExpressionPrefix
pExpressionPrefix :: HSParser (Expression -> Expression)
pExpressionPrefix
  =   pExpressionLetPrefix
  <|> pExpressionNoLetPrefix
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Alternatives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[5
pAlternative :: HSParser Alternative
pAlternative
  = Alternative_Alternative emptyRange <$> pPattern <*> pRhs pRARROW

pAlternatives :: HSParser Alternatives
pAlternatives
  = pBlock1 pOCURLY pSEMI pCCURLY pAlternative
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pattern
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.pPatternBase
pPatternBase :: HSParser Pattern
pPatternBase
  =   qvar <**> (   (\a p v -> Pattern_As (mkRange1 a) (tokMkQName v) p) <$> pAT <*> pPatternBase
                <|> pSucceed (mkRngNm Pattern_Variable)
                )
  <|> Pattern_Literal emptyRange <$> pLiteral
  <|> (Pattern_Negate . mkRange1) <$> pMINUS <*> pLiteralNumber
  <|> qconid
      <**> (   pSucceed (\c -> mkRngNm Pattern_Constructor c [])
%%[[7
           <|> pCurlys' ((\bs _ c -> mkRngNm Pattern_Record c bs) <$> pListSep pCOMMA pRecordPatternBinding)
%%]]
           )
%%[[5
  <|> pBracks' (flip Pattern_List <$> pListSep pCOMMA pPattern)
%%]]
%%[[8
  <|> (Pattern_Irrefutable . mkRange1) <$> pTILDE <*> pPatternBase
%%]]
%%[[1
  <|> pParens' pInParens
  where pInParens :: HSParser (Range -> Pattern)
        pInParens
          =   (pPattern
               <**> (   (\es e r -> Pattern_Tuple r (e:es))
                        <$>  pList1 (pComma *> pPattern)
                    <|> pSucceed (flip Pattern_Parenthesized)
              )     )
          <|> pSucceed (\r -> Pattern_Constructor r (hsnProd 0) [])
%%][7
  <|>   pParenRow True pOROWREC pCROWREC pEQUAL False undefined undefined
          (Pattern_RowRecordEmpty,Pattern_Variable,RowRecordPatternBinding_Binding,Pattern_RowRecordBinding,Pattern_Parenthesized)
          pSelector pPattern
%%]]
%%]

%%[7
pRecordPatternBinding :: HSParser RecordPatternBinding
pRecordPatternBinding
  = qvar
    <**> (   pSucceed (\v -> mkRngNm RecordPatternBinding_Pun v)
         <|> (\p v -> mkRngNm RecordPatternBinding_Binding v p) <$ pEQUAL <*> pPattern
         )
%%]

%%[1
pPatternApp :: HSParser Pattern
pPatternApp
  =   mkRngNm Pattern_Constructor <$> qconid <*> pList1 pPatternBase
  <|> pPatternBase
%%]

%%[1
pPatternOp :: HSParser Pattern
pPatternOp
  =   pChainr_ng
        ((\o l r -> mkRngNm Pattern_Constructor o [l,r]) <$> qconop)
        pPatternApp
%%]

%%[1.pPattern
pPattern :: HSParser Pattern
pPattern
  =   pPatternOp
%%]
%%[4.patExpr
      <??> ((\c t p -> Pattern_Typed (mkRange1 c) p t) <$> pDCOLON <*> pType)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Row
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[7
data RowFld a = FldSel Token a | FldNoSel a | FldUpd Token a
%%]

%%[7
pParenRow
  ::   Bool -> HSParser Token -> HSParser Token -> HSParser Token -> Bool -> HSParser Token -> (Range -> HsName -> e -> r)
       -> (Range -> e,Range -> HsName -> e,Range -> Maybe HsName -> e -> r,Range -> e -> [r] -> e,Range -> e -> e)
       -> HSParser Token -> HSParser e -> HSParser e
pParenRow singleAsIs pO pC pS allowUpd pSUpd' semUpd' sems pSel pE
  = pPacked' pO pC (pParenRow' singleAsIs pO pC pS allowUpd pSUpd' semUpd' sems pSel pE)
%%]

%%[7
pParenRow'
  ::   Bool -> HSParser Token -> HSParser Token -> HSParser Token -> Bool -> HSParser Token -> (Range -> HsName -> e -> r)
       -> (Range -> e,Range -> HsName -> e,Range -> Maybe HsName -> e -> r,Range -> e -> [r] -> e,Range -> e -> e)
       -> HSParser Token -> HSParser e -> HSParser (Range -> e)
pParenRow' singleAsIs pO pC pS allowUpd pSUpd' (semUpd' :: Range -> HsName -> e -> r) (semEmpty,semVar,semExt,semRow,semParens) pSel (pE :: HSParser e)
  = pRowFlds
  where pFld          = (pSel <**> pSep <|> pSucceed FldNoSel) <*> pE
        pFlds         = pListSep pComma pFld
        pExtFlds      = (\e fs rng -> mkR rng e fs)
                        <$> (   pPacked' pO pC pExtFlds
                            <|> mkRngNm semVar <$> qvarid
                            )
                        <* pVBAR <*> pFlds
        pRowFlds      =   pFld <**> mkS ((\fs f rng -> mkR rng (semEmpty rng) (f:fs)) <$ pComma <*> pFlds)
                      <|> pExtFlds <|> pSucceed semEmpty
                      where mkS = if singleAsIs
                                  then (\p -> p <|> pSucceed (\le rng -> case le of {FldNoSel e -> semParens rng e; _ -> mkR rng (semEmpty rng) [le]}))
                                  else id
        mkR rng e fs  = semRow rng e (mkE fs)
        mkE           = map (\f -> case f of 
                                     FldSel   l e -> semExt (mkRange1 l) (Just (tokMkQName l)) e
                                     FldNoSel   e -> semExt emptyRange Nothing e
                                     FldUpd   l e -> mkRngNm semUpd l e
                            )
        pSep          = if allowUpd then FldSel <$ pS <|> FldUpd <$ pSUpd' else FldSel <$ pS
        semUpd        = if allowUpd then semUpd'                           else \_ _ _ -> undefined
        pFld :: HSParser (RowFld e)
        pFlds :: HSParser [RowFld e]
        pSep :: HSParser (Token -> a -> RowFld a)
        pRowFlds, pExtFlds :: HSParser (Range -> e)
        semUpd :: Range -> HsName -> e -> r
%%]

%%[7
pRowRecordSelectionSuffix :: HSParser (Expression -> Expression)
pRowRecordSelectionSuffix
  = (\lbls e -> foldl (\e l -> Expression_RowRecordSelect (mkRange1 l) e (tokMkQName l)) e lbls)
    <$> pList1 (pHASH *> pSelector)
%%]

%%[7
pSelector :: HSParser Token
pSelector
  =   qvarid <|> qconid <|> pIntegerTk
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Names/Symbols of all sorts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
commas :: HSParser Token 
commas =  genTokMap (\s -> strProd (length s + 1)) <$> pFoldr (tokConcat,tokEmpty) pCOMMA
%%]

%%[1
modid :: HSParser Token
modid =   pCONID
%%]
%%[12
      <|> pQCONID
%%]

%%[12
qcnames :: HSParser [Token] 
qcnames =  pListSep pCOMMA qcname

qcname  :: HSParser Token   -- Variable or data constructor
qcname  =  qvar <|> gcon                    
%%]

%%[1
identifier :: HSParser Token 
identifier =  qvar      
          <|> gcon  
          <|> qop       

depreclist :: HSParser [Token] 
depreclist = pList1Sep pCOMMA deprec_var

deprec_var :: HSParser Token
deprec_var = var <|> tycon          

gcon    :: HSParser Token   -- Data constructor namespace
gcon    =  sysdcon      
       <|> qcon         
-- the case of '[:' ':]' is part of the production `parr'

sysdcon :: HSParser Token   -- Wired in data constructors
sysdcon =  pParens commas 
       <|> tokConcat <$> pOBRACK <*> pCBRACK

var     :: HSParser Token
var =  varid            
   <|> pParens varsym 

qvar    :: HSParser Token
qvar =  qvarid      
    <|> pParens qvarsym_forpar
%%]

%%[1
qvarsym_forpar :: HSParser Token
qvarsym_forpar
  =   varsym
%%]
%%[12
  <|> qvarsym1
%%]

{-
ipvar   :: HParser (IPName RdrName)
ipvar =  liftM (Dupable . mkUnqual varName) <$> pDUPIPVARID
     <|> liftM (Linear . mkUnqual varName)  <$> pSPLITIPVARID
-}

%%[1
qcon    :: HSParser Token
qcon    = qconid <|> pParens qconsym


varop   :: HSParser Token
varop   =  varsym 
       <|> pBACKQUOTE *> varid <* pBACKQUOTE
       
qvarop :: HSParser Token
qvarop = qvarsym    
       <|> pBACKQUOTE *> qvarid <* pBACKQUOTE

qvaropm :: HSParser Token
qvaropm =  qvarsym_no_minus 
       <|> pBACKQUOTE *> qvarid <* pBACKQUOTE

conop :: HSParser Token
conop =  consym     
       <|> pBACKQUOTE *> conid <* pBACKQUOTE

qconop :: HSParser Token
qconop =  qconsym       
      <|> pBACKQUOTE *> qconid <* pBACKQUOTE

%%]

%%[1
-----------------------------------------------------------------------------
-- Variables 

qvarsym :: HSParser Token 
qvarsym =  varsym
%%]
%%[12
       <|> qvarsym1
%%]

%%[1
qvarsym_no_minus :: HSParser Token
qvarsym_no_minus =  varsym_no_minus
%%]
%%[12
                <|> qvarsym1
%%]

%%[12
qvarsym1 :: HSParser Token
qvarsym1 = pQVARSYM 
%%]

%%[1
varsym :: HSParser Token 
varsym  =  varsym_no_minus  
       <|> pMINUS       


varsym_no_minus :: HSParser Token  -- varsym not including '-'
varsym_no_minus = pVARSYM <|> special_sym

-- See comments with special_id
special_sym :: HSParser Token 
special_sym 
        =  pBANG    
       <|> pDOT     
       <|> pSTAR
       <|> pPERCENT

-----------------------------------------------------------------------------
-- Data constructors

qconid :: HSParser Token    -- Qualified or unqualifiedb
qconid =  conid
%%]
%%[12
      <|> pQCONID
%%]

%%[1
conid   :: HSParser Token
conid   =  pCONID           

qconsym :: HSParser Token   -- Qualified or unqualified
qconsym = consym
%%]
%%[12
       <|> pQCONSYM
%%]

%%[1
consym :: HSParser Token
consym =  pCONSYM       
      <|> pCOLON
    -- ':' means only list cons
    -- NB: SrcName because we are reading source
%%]

%%[1
con :: HSParser Token
con = conid <|> pParens consym
%%]

%%[1
-----------------------------------------------------------------------------
-- Any operator

op  :: HSParser Token   -- used in infix decls
op  = varop <|> conop

qop :: HSParser Token   -- used in sections
qop = qvarop <|> qconop

qopm    :: HSParser  Token    -- used in sections
qopm    = qvaropm <|> qconop

-----------------------------------------------------------------------------
-- VarIds

qvarid :: HSParser Token
qvarid = varid
%%]
%%[12
     <|> pQVARID
%%]

%%[8
safety :: HSParser Token
safety
  =   pUNSAFE        
  <|> pSAFE      
  <|> pTHREADSAFE 

callconv :: HSParser Token
callconv
  =   pSTDCALL 
  <|> pCCALL    
  <|> pJAZY 
%%]

%%[1
varid :: HSParser Token
varid = varid_no_unsafe
%%]
%%[8
     <|> safety
%%]

%%[1
varid_no_unsafe :: HSParser Token
varid_no_unsafe
  =   varid_no_foreign
%%]
%%[8
  <|> callconv
%%]

%%[1
varid_no_foreign :: HSParser Token
varid_no_foreign
  =   pVARID
%%]
%%[4
  <|> pFORALL
%%]
%%[8
  <|> special_id_no_callconv
%%]

%%[1
tyvar   :: HSParser Token
tyvar   =  pVARID
%%]
%%[8
       <|> special_id       
       <|> safety   
%%]

%%[8
-- These special_ids are treated as keywords in various places, 
-- but as ordinary ids elsewhere.   'special_id' collects all these
-- except 'unsafe' and 'forall' whose treatment differs depending on context
special_id_no_callconv :: HSParser Token 
special_id_no_callconv
  =   pLABEL   
  <|> pDYNAMIC
  <|> pEXPORT
%%[[12
  <|> pAS      
  <|> pQUALIFIED   
  <|> pHIDING
%%]

special_id :: HSParser Token 
special_id
  =   special_id_no_callconv
  <|> callconv
%%]

%%[1
gtycon_no_delims :: HSParser Token   -- A "general" qualified tycon
gtycon_no_delims
  =   oqtycon
  <|> pParens
        (   commas
        <|> pRARROW
        )

gtycon :: HSParser Token   -- A "general" qualified tycon
gtycon
  =   gtycon_no_delims
%%]
%%[5
  <|> tokConcat <$> pOBRACK <*> pCBRACK
%%]
%%[90
  <|> tokConcat <$> pOPABRACK <*> pCPABRACK
%%]

%%[1
tycon   :: HSParser Token   -- Unqualified
tycon   = pCONID                

oqtycon :: HSParser Token   -- An "ordinary" qualified tycon
oqtycon =  qtycon
       <|> pParens qtyconsym  

qtycon :: HSParser Token    -- Qualified or unqualified
qtycon =  tycon
%%]
%%[12
      <|> pQCONID
%%]

%%[1      
qtyconsym :: HSParser Token
qtyconsym =  tyconsym
%%]
%%[12
         <|> pQCONSYM
%%]

%%[1
tyconsym :: HSParser Token
tyconsym = pCONSYM          
%%]

%%[12
qtyconop :: HSParser Token  -- Qualified or unqualified
qtyconop = qtyconsym
       <|> pBACKQUOTE *> qtycon <* pBACKQUOTE
%%]

%%[8
tyconop :: HSParser Token   -- Unqualified
tyconop = tyconsym  
       <|> pBACKQUOTE *> tycon <* pBACKQUOTE
%%]


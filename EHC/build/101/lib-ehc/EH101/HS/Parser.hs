module EH101.HS.Parser
( HSParser, HSParser'
, pAGItf
, pAGItfImport )
where
import UU.Parsing
import UU.Parsing.Offside
import EH.Util.ParseUtils
import UU.Scanner.GenToken
import EH.Util.ScanUtils
import EH101.Base.Common
import EH101.Base.Builtin
import EH101.Scanner.Common
import EH101.Opts
import EH101.HS
import System.IO
import EH.Util.Utils
import EH.Util.Pretty
import Data.Maybe
import EH101.Base.Target
import qualified EH.Util.FastSeq as Seq





{-# LINE 37 "src/ehc/HS/Parser.chs" #-}
tokConcat :: Token -> Token -> Token
tokConcat t1 t2 = Reserved (tokenVal t1 ++ tokenVal t2) (position t1)

tokEmpty :: Token
tokEmpty = Reserved "" noPos

{-# LINE 49 "src/ehc/HS/Parser.chs" #-}
pPacked' :: HSParser Token -> HSParser Token -> HSParser (Range -> v) -> HSParser v
pPacked' pO pC pMk = (\o mk c -> mk (mkRange2 o c)) <$> pO <*> pMk <*> pC

pParens' :: HSParser (Range -> v) -> HSParser v
pParens' = pPacked' pOPAREN pCPAREN

pBracks' :: HSParser (Range -> v) -> HSParser v
pBracks' = pPacked' pOBRACK pCBRACK

pCurlys' :: HSParser (Range -> v) -> HSParser v
pCurlys' = pPacked' pOCURLY pCCURLY

{-# LINE 63 "src/ehc/HS/Parser.chs" #-}
pImpls' :: HSParser (Range -> v) -> HSParser v
pImpls' = pPacked' pOIMPL pCIMPL

pImpls :: IsParser p Token => p v -> p v
pImpls = pPacked pOIMPL pCIMPL

{-# LINE 71 "src/ehc/HS/Parser.chs" #-}
pApp            ::   SemApp ep => HSParser ep -> HSParser ep
pApp p          =    mkApp <$> pList1 p

{-# LINE 80 "src/ehc/HS/Parser.chs" #-}
type HSParser         ep    =    LayoutParser Token ep
type HSParser2        ep    =    LayoutParser2 Token ep
type HSParser'        ep    =    PlainParser Token ep

{-# LINE 86 "src/ehc/HS/Parser.chs" #-}
pAGItf :: EHCOpts -> HSParser AGItf
pAGItf opts
  =   AGItf_AGItf <$> pModule opts pBody'

{-# LINE 96 "src/ehc/HS/Parser.chs" #-}
pAGItfImport :: EHCOpts -> HSParser AGItf
pAGItfImport opts
  =   AGItf_AGItf <$> pModule opts (\opts _ -> pBodyImport opts)

{-# LINE 110 "src/ehc/HS/Parser.chs" #-}
data Expression4Result
  = Expression4Result_Op                (Expression,Range)
  | Expression4Result_CommaList         [Expression]
  | Expression4Result_Typed
  | Expression4Result_NotOpPre
  -- deriving Eq

type Expression4 = (Expression,Int,[Expression4Result])


{-# LINE 126 "src/ehc/HS/Parser.chs" #-}
pPragma' :: (Range -> Pragma -> x) -> HSParser x
pPragma' mk
  = pPacked' pOPRAGMA pCPRAGMA
      (   (\t ps r -> mk r $ Pragma_Language   (mkRange1 t) ps)
          <$> pLANGUAGE_prag   <*> pCommas (tokMkQName <$>           conid)
      <|> (\t cl fld val r -> mk r $ Pragma_Derivable (mkRange1 t) (tokMkQName cl) (tokMkQName fld) (tokMkQName val))
          <$> pDERIVABLE_prag  <*> gtycon' tyconsym <*> var <*> qvar
      <|> (\t targ r -> mk r $ Pragma_ExcludeIfTarget (mkRange1 t) (map tokMkStr $ concat targ))
          <$> pEXCLUDEIFTARGET_prag  <*> pList1Sep pCOMMA (pList1 (conid <|> varid))
      )

pPragma :: HSParser Pragma
pPragma = pPragma' (flip const)

pDeclarationPragma :: HSParser Declaration
pDeclarationPragma
  = pPragma' Declaration_Pragma


{-# LINE 151 "src/ehc/HS/Parser.chs" #-}
pModule :: EHCOpts -> (EHCOpts -> (HSParser2 Declaration) -> HSParser Body) -> HSParser Module
pModule opts pBody
  = pList_gr pPragma
    <**> (   (\      b p -> Module_Module emptyRange   Nothing                 p Nothing b) <$> pBody opts id
         <|> (\t m e b p -> Module_Module (mkRange1 t) (Just $ tokMkQName $ m) p e       b) <$> pMODULE <*> modid <*> pMaybeExports <* pWHERE <*> pBody opts (\d -> d <|> pDeclarationPragma)
         )
  <?> "pModule"

{-# LINE 177 "src/ehc/HS/Parser.chs" #-}
pImportExport :: (Range -> Name -> ie,Range -> Name -> MaybeNames -> ie,Range -> Name -> ie) -> HSParser ie
pImportExport (sem_Var,sem_tOrC,sem_tOrC_complete)
  =   mkRngNm sem_Var <$> qvar
  <|> qtycon
      <**> (   pParens
                 (   (\c n -> mkRngNm sem_tOrC n (Just (tokMkQNames c))) <$> qcnames
                 <|> mkRngNm sem_tOrC_complete <$ pDOTDOT
                 )
           <|> pSucceed (\n -> mkRngNm sem_tOrC n Nothing)
           )
  <?> "pImportExport"

{-# LINE 191 "src/ehc/HS/Parser.chs" #-}
pExport :: HSParser Export
pExport
  =   (\t m -> Export_Module (mkRange1 t) (tokMkQName m)) <$> pMODULE <*> modid
  <|> pImportExport (Export_Variable,Export_TypeOrClass,Export_TypeOrClassComplete)
  <?> "pExport"

pMaybeExports :: HSParser MaybeExports
pMaybeExports
  =   Just <$> pParens (pListSep_ng pCOMMA pExport <* pMb pCOMMA)
  <|> pSucceed Nothing
  <?> "pMaybeExports"

{-# LINE 205 "src/ehc/HS/Parser.chs" #-}
pImport :: HSParser Import
pImport
  =   pImportExport (Import_Variable,Import_TypeOrClass,Import_TypeOrClassComplete)
  <?> "pImport"

pImportDeclaration :: HSParser ImportDeclaration
pImportDeclaration
  = (\q m a i -> ImportDeclaration_Import (mkRange1 m) q (tokMkQName m) (fmap tokMkQName a) i)
    <$  pIMPORT <*> (True <$ pQUALIFIED <|> pSucceed False)
    <*> modid
    <*> (Just <$ pAS <*> modid <|> pSucceed Nothing)
    <*> (Just <$> pImportSpecification <|> pSucceed Nothing)
  <?> "pImportDeclaration"
  where pImportSpecification :: HSParser ImportSpecification
        pImportSpecification
          = (True <$ pHIDING <|> pSucceed False)
            <**> pParens'
                   ((\i r h -> ImportSpecification_Import r h i) <$> pListSep_ng pCOMMA pImport <* pMb pCOMMA)

{-# LINE 230 "src/ehc/HS/Parser.chs" #-}
pLayoutList :: HSParser d -> HSParser [d]
pLayoutList pD
  =   pBlock pOCURLY pSEMI pCCURLY pD

pLayoutList1 :: HSParser d -> HSParser [d]
pLayoutList1 pD
  =   pBlock1 pOCURLY pSEMI pCCURLY pD


{-# LINE 245 "src/ehc/HS/Parser.chs" #-}
pBodyImport :: EHCOpts -> HSParser Body
pBodyImport opts
  =   (\d -> Body_Body emptyRange d []) <$> pLayoutList pImportDeclaration
  <?> "pBodyImport"

{-# LINE 252 "src/ehc/HS/Parser.chs" #-}
pBody :: EHCOpts -> HSParser Body
pBody opts = pBody' opts id

{-# LINE 257 "src/ehc/HS/Parser.chs" #-}
pBody' :: EHCOpts -> (HSParser2 Declaration) -> HSParser Body
pBody' opts addDecl
  =   (\ids -> let (i,d) = foldr cmbid ([],[]) ids in Body_Body emptyRange i d)
      <$> pLayoutList (   (\d -> ([],[d])) <$> (addDecl pTopDeclaration)
                      <|> (\i -> ([i],[])) <$> pImportDeclaration
                      )
  <?> "pBody"
  where
        cmbid ([i],_) (is,ds) = (i:is,ds)
        cmbid (_,[d]) (_ ,ds) = ([],d:ds)

{-# LINE 281 "src/ehc/HS/Parser.chs" #-}
        pDeclaration :: HSParser Declaration
        pDeclaration
          =   pDeclarationValue
          <|> pDeclarationTypeSignature
          <|> pDeclarationData
          <|> pDeclarationKindSignature
          <|> pDeclarationInstance
          <|> pDeclarationType
          <|> pDeclarationFusion
          <|> pDeclarationConversion
          <?> "pDeclaration"

{-# LINE 305 "src/ehc/HS/Parser.chs" #-}
        pTopDeclaration :: HSParser Declaration
        pTopDeclaration
          =   pDeclaration
          <|> pDeclarationFixity
          <|> pDeclarationForeign
          <|> pDeclarationClass
          <|> pDeclarationDefault
          <?> "pTopDeclaration"

{-# LINE 320 "src/ehc/HS/Parser.chs" #-}
        pDeclarations :: HSParser Declarations
        pDeclarations
          =   pLayoutList pDeclaration

        pDeclarations1 :: HSParser Declarations
        pDeclarations1
          =   pLayoutList1 pDeclaration

{-# LINE 330 "src/ehc/HS/Parser.chs" #-}
        pWhere'' :: HSParser d -> HSParser [d]
        pWhere'' pD = pWHERE *> pLayoutList pD

        pWhere' :: HSParser d -> HSParser (Maybe [d])
        pWhere' pD = pMb (pWhere'' pD)

        pWhere :: HSParser MaybeDeclarations
        pWhere = pWhere' pDeclaration

{-# LINE 345 "src/ehc/HS/Parser.chs" #-}
        pDeclarationFixity :: HSParser Declaration
        pDeclarationFixity
          = (\f p os@(o:_) -> Declaration_Fixity (mkRange1 o) f p (tokMkQNames os))
            <$> pFixity
            <*> ((Just . tokMkInt) <$> pInteger10Tk <|> pSucceed Nothing)
            <*> pList1Sep pCOMMA op

        pFixity :: HSParser' Fixity
        pFixity = Fixity_Infixl <$ pINFIXL <|> Fixity_Infixr <$ pINFIXR <|> Fixity_Infix <$ pINFIX

{-# LINE 361 "src/ehc/HS/Parser.chs" #-}
        pDeclarationTypeSignature :: HSParser Declaration
        pDeclarationTypeSignature
          =   (\(v:vs) t -> Declaration_TypeSignature (mkRange1 v) (tokMkQNames (v:vs)) t)
              <$> pList1Sep pCOMMA var <* pDCOLON <*> pType
          <?> "pDeclarationTypeSignature"

        pDeclarationValue :: HSParser Declaration
        pDeclarationValue
          =   mkF <$> pLhs <*> rhs
          <|> pPatternOp
              <**> (   (flip mkP) <$> rhs
                   <|> (\o r rhs l -> mkF (mkLI l o r) rhs)
                       <$> pVarOp <*> pPatternOp <*> rhs
                   )
          <?> "pDeclarationValue"
          where pVarOp :: HSParser Token
                pVarOp = if ehcOptBangPatterns opts then varop_no_bang else varop
                pLhsTail ::  HSParser [Pattern]
                pLhsTail =   pList1 pPatternBaseCon
                pLhs     ::  HSParser (Range,LeftHandSide)
                pLhs     =   (\v lhs -> let r = mkRange1 v in (r, LeftHandSide_Function r (tokMkQName v) lhs)) <$> var <*> pLhsTail
                         <|> pParens'
                               (   (\l r t -> (r, mkLP r l t))
                                   <$> pLhs
                               <|> (\pl o pr r t -> (r, mkLP r (mkLI pl o pr) t))
                                   <$> pPatternOp <*> varop <*> pPatternOp
                               )
                             <*> pLhsTail
                mkP  p     rhs = Declaration_PatternBinding emptyRange (p2p p) rhs'
                               where (p2p,rhs') = mkTyPat rhs
                mkF (r,lhs) rhs= Declaration_FunctionBindings r [FunctionBinding_FunctionBinding r (l2l lhs) rhs']
                               where (l2l,rhs') = mkTyLhs rhs
                mkLI l o rh    = (r, LeftHandSide_Infix r l (tokMkQName o) rh)
                               where r = mkRange1 o
                mkLP r (_,l) t = LeftHandSide_Parenthesized r l t
                rhs      =   pMbTy <+> pRhs pEQUAL
                pMbTy    ::  HSParser (Maybe (Token,Type))
                pMbTy    =   pMb (pDCOLON <+> pType)
                mkTyLhs (Just (tok,ty),rhs) = (\l -> LeftHandSide_Typed (mkRange1 tok) l ty,rhs)
                mkTyLhs (_            ,rhs) = (id                                          ,rhs)
                mkTyPat (Just (tok,ty),rhs) = (\p -> Pattern_Typed      (mkRange1 tok) p ty,rhs)
                mkTyPat (_            ,rhs) = (id                                          ,rhs)

{-# LINE 416 "src/ehc/HS/Parser.chs" #-}
        pDeclarationSimpleValue :: HSParser Declaration
        pDeclarationSimpleValue
          =   Declaration_PatternBinding emptyRange <$> lhs <*> rhs
          <?> "pDeclarationSimpleValue"
          where lhs = mkRngNm Pattern_Variable <$> var
                rhs = (\t e -> RightHandSide_Expression (mkRange1 t) e Nothing) <$> pEQUAL <*> pExpression

{-# LINE 425 "src/ehc/HS/Parser.chs" #-}
        pRhs :: HSParser Token -> HSParser RightHandSide
        pRhs pSep
          =   (RightHandSide_Expression . mkRange1) <$> pSep <*> pExpression <*> pWhere
          <|> RightHandSide_Guarded emptyRange
              <$> pList1 ((GuardedExpression_GuardedExpression . mkRange1) <$> pVBAR <*> pExpression <* pSep <*> pExpression)
              <*> pWhere
          <?> "pRhs"

{-# LINE 437 "src/ehc/HS/Parser.chs" #-}
        pDeclarationKindSignature :: HSParser Declaration
        pDeclarationKindSignature
          =   (\(v:vs) t -> Declaration_KindSignature (mkRange1 v) (tokMkQNames (v:vs)) t)
              <$> pList1Sep pCOMMA con <* pDCOLON <*> pKind
          <?> "pDeclarationKindSignature"

{-# LINE 449 "src/ehc/HS/Parser.chs" #-}
        pDeclarationData :: HSParser Declaration
        pDeclarationData
          =   pDATA
              <**> (pCtxt
              <**>  (pTypeLeftHandSide
              <**>   (   (\cs der lhs cx k -> mk Declaration_Data k cx lhs cs der)
                         <$> (pEQUAL *> pListSep pVBAR pDCon) <*> pDer
                     <|> (\cs der lhs cx k -> mk Declaration_Data k cx lhs cs der)
                         <$> pWhere'' (pGADTConstructor) <*> pDer
                     <|> pSucceed (\lhs cx k -> mk Declaration_Data k cx lhs [] [])
                   )))
          <|> pNEWTYPE
              <**> (   (\cx lhs c der k -> mk Declaration_Newtype k cx lhs c der)
                       <$> pCtxt <*> pTypeLeftHandSide <* pEQUAL <*> pNCon <*> pDer
                   )
          <?> "pDeclarationData"
          where mk sem =
                      \k cx lhs cs der
                         -> sem (mkRange1 k) cx lhs cs der
                -- TBD, for now: parse, but ignore quantifiers
                pDCon, pNCon :: HSParser Constructor
                pDCon = pList pTypeQuantPrefix *> pContextedConstructor
                pNCon = pList pTypeQuantPrefix *> pConstructor
                pCtxt :: HSParser ContextItems
                pCtxt = pContextItemsPrefixOpt
                pDer :: HSParser [Deriving]
                pDer = pDERIVING *> ((:[]) <$> pDeriving <|> pParens (pListSep pCOMMA pDeriving)) <|> pSucceed []

{-# LINE 501 "src/ehc/HS/Parser.chs" #-}
        pDeriving :: HSParser Deriving
        pDeriving
          = (\(n,u) t -> Deriving_Deriving (mkRange1 t) n u (tokMkQName t)) <$> pInstanceName <*> qconid

{-# LINE 507 "src/ehc/HS/Parser.chs" #-}
        pConstructor :: HSParser Constructor
        pConstructor
          =   con
              <**> (   (\ts c -> mkRngNm Constructor_Constructor c ts) <$> pList pTB
{-# LINE 513 "src/ehc/HS/Parser.chs" #-}
                   <|> pCurlys' ((\fs r c -> mkRngNm Constructor_Record c fs) <$> pList1Sep pCOMMA pFieldDeclaration)
{-# LINE 516 "src/ehc/HS/Parser.chs" #-}
                   )
          <|> (\l o r -> Constructor_Infix (mkRange1 o) l (tokMkQName o) r) <$> pT <*> conop <*> pT
          where pT  = pAnnotatedType pType
                pTB = pAnnotatedType pTypeBase

{-# LINE 523 "src/ehc/HS/Parser.chs" #-}
        pContextedConstructor :: HSParser Constructor
        pContextedConstructor
          =   Constructor_Contexted emptyRange <$> pContextItemsPrefix <*> pConstructor
          <|> pConstructor

{-# LINE 530 "src/ehc/HS/Parser.chs" #-}
        pGADTConstructor :: HSParser Constructor
        pGADTConstructor
          =   mkRngNm Constructor_GADTFunction <$> con <* pDCOLON <*> pType

{-# LINE 536 "src/ehc/HS/Parser.chs" #-}
        pFieldDeclaration :: HSParser FieldDeclaration
        pFieldDeclaration
          = (\vs@(v:_) -> FieldDeclaration_FieldDeclaration (mkRange1 v) (tokMkQNames vs))
            <$> pList1Sep pCOMMA var <* pDCOLON <*> pAnnotatedType pType

{-# LINE 547 "src/ehc/HS/Parser.chs" #-}
        pDeclarationForeign :: HSParser Declaration
        pDeclarationForeign
          = pFOREIGN
            <**> (   (\c s (i,n,t) r -> Declaration_ForeignImport (mkRange1 r) (fst c) s i (tokMkQName n) t)
                     <$ pIMPORT <*> pFFIWay <*> pSafety <*> pFSpec
                 <|> (\c (i,n,t) r -> Declaration_ForeignExport (mkRange1 r) (fst c) i (tokMkQName n) t)
                     <$ pEXPORT <*> pFFIWay <*> pFSpec
                 )
          where pSafety =  (Just . tokMkStr) <$> safety <|> pSucceed Nothing
                pFSpec = (,,) <$> ((Just . tokMkStr) <$> pStringTk <|> pSucceed Nothing) <*> var{-id_no_foreign-} <* pDCOLON <*> pType

{-# LINE 566 "src/ehc/HS/Parser.chs" #-}
        pDeclarationClass :: HSParser Declaration
        pDeclarationClass
          = (\t -> Declaration_Class (mkRange1 t))
            <$> pCLASS
            <*> pContextItemsPrefixOpt <*> pTypeLeftHandSide
            <*> (pVBAR *> pListSep pCOMMA pFunctionalDependency
                `opt` []
                )
            <*> pWhere' (pDeclarationValue <|> pDeclarationTypeSignature)
          where pFunctionalDependency :: HSParser FunctionalDependency
                pFunctionalDependency
                  = (\vs1@(v:_) vs2 -> FunctionalDependency_Dependency (mkRange1 v) (tokMkQNames vs1) (tokMkQNames vs2))
                    <$> pList1 tyvar <* pRARROW <*> pList1 tyvar

{-# LINE 586 "src/ehc/HS/Parser.chs" #-}
        pInstanceName :: HSParser (Maybe HsName,Bool)
        pInstanceName
          =   (\n e -> (Just (tokMkQName n),e)) <$> varid <*> (True <$ pLTCOLON <|> False <$ pDCOLON)
          <|> pSucceed (Nothing,True)

{-# LINE 593 "src/ehc/HS/Parser.chs" #-}
        pDeclarationInstance :: HSParser Declaration
        pDeclarationInstance
          =   pINSTANCE
              <**> (   -- (\((n,u),c,cl,ts) d t -> Declaration_Instance (mkRange1 t) InstNormal n u c (tokMkQName cl) ts d)
                       (\((n,u),c,h) d t -> Declaration_Instance (mkRange1 t) InstNormal n u c h d)
                       <$> pHeader
                       <*> pWhere' pDeclarationValue
                   <|> (\e cl ts t -> Declaration_InstanceUseImplicitly (mkRange1 t) e (tokMkQName cl) ts)
                       <$> pExpression <* pLTCOLON <*> qconid <*> pList1 pTypeBase
                   )
          <|> -- (\t ((n,u),c,cl,ts) -> Declaration_Instance (mkRange1 t) (InstDeriving InstDerivingFrom_Standalone) n u c (tokMkQName cl) ts Nothing)
              (\t ((n,u),c,h) -> Declaration_Instance (mkRange1 t) (InstDeriving InstDerivingFrom_Standalone) n u c h Nothing)
              <$> pDERIVING <* pINSTANCE <*> pHeader
          -- where pHeader = (,,,) <$> pInstanceName <*> pContextItemsPrefixOpt <*> qconid <*> pList1 pTypeBase
          where pHeader = (,,) <$> pInstanceName <*> pContextItemsPrefixOpt <*> pType' pTypeOpBase (\_ p -> p)

{-# LINE 613 "src/ehc/HS/Parser.chs" #-}
        pDeclarationDefault :: HSParser Declaration
        pDeclarationDefault
          = (Declaration_Default . mkRange1) <$> pDEFAULT <*> pMb (tokMkQName <$> qtyconid)
            <*> (   (:[]) <$> pTypeBaseCon
                <|> pParens (pListSep pCOMMA pTypeBaseCon)
                )

{-# LINE 626 "src/ehc/HS/Parser.chs" #-}
        pDeclarationType :: HSParser Declaration
        pDeclarationType
          =   (Declaration_Type . mkRange1) <$> pTYPE <*> pTypeLeftHandSide <* pEQUAL <*> pType

{-# LINE 637 "src/ehc/HS/Parser.chs" #-}

        pDeclarationFusion :: HSParser Declaration
        pDeclarationFusion
          = (Declaration_FusionDeclaration . mkRange1) <$> pFUSE <*> (tokMkQName <$> qvar) -- <* pWITH <*> (tokMkQName <$> qvar)

        pDeclarationConversion :: HSParser Declaration
        pDeclarationConversion
          = (Declaration_FusionConversion . mkRange1) <$> pCONVERT <*> (tokMkQName <$> qvar) <* pCOMMA <*> (tokMkQName <$> qvar)

{-# LINE 653 "src/ehc/HS/Parser.chs" #-}
        pKindBase :: HSParser Kind
        pKindBase
          =   mkRngNm Kind_Constructor <$> pSTAR
          <|> mkRngNm Kind_Variable <$> tyvar
          <|> pParens' pInParens
          <?> "pKindBase"
          where pInParens :: HSParser (Range -> Kind)
                pInParens
                  =   (pKind
                       <**> (   pSucceed (flip Kind_Parenthesized)
                      )     )

        pKind :: HSParser Kind
        pKind
          =   mkK <$> pK
          <?> "pKind"
          where pK ::  HSParser (Kind,Int)
                pK =   pKindBase
                       <**> (   pSucceed (\k -> (k,1))
                            <|> (\(op,rng) (r,opCnt) l -> (Kind_InfixApplication rng l op r,opCnt+1)) <$> pKindOp <*> pK
                            )
                   <|> (\p e -> (p $ mkK $ e,1)) <$> pKindPrefix <*> pK
                mkK (e,1) = e
                mkK (e,_) = {- Expression_InfixApplicationChainTop emptyRange -} e
                pKindOp :: HSParser (Kind,Range)
                pKindOp = mkRngNm' Kind_Constructor <$> pRARROW

        pKindPrefix :: HSParser (Kind -> Kind)
        pKindPrefix
          =  ((Kind_Forall . mkRange1) <$> pFORALL)
             <*> (tokMkQNames <$> pTyVarBinds) <* pDOT

{-# LINE 691 "src/ehc/HS/Parser.chs" #-}
        pTypeBaseCon :: HSParser Type
        pTypeBaseCon
          =   mkRngNm Type_Constructor <$> gtycon_no_delims_commas

        pTypeBase :: HSParser Type
        pTypeBase
          =   pTypeBaseCon
          <|> (Type_Wildcard . mkRange1) <$> pTDOT
          <|> (Type_MonoWildcard . mkRange1) <$> pQDOT
          <|> mkRngNm Type_Variable <$> var_no_ty
          <|> mkRngNm Type_NamedWildcard <$ pPERCENT <*> tyvar
          <|> pBracks'
                (   (\t r -> Type_NormalApplication r (Type_Constructor r hsnDataList) [t])
                    <$> pType
                <|> pSucceed (\r -> Type_Constructor r hsnDataList)
                )
          <|> pParens' pInParens
          <|> pPacked' pOROWROW pCROWROW
                (    pExtFlds Type_RowEmpty Type_RowUpdate
                <|> (\fs r -> Type_RowUpdate r (Type_RowEmpty r) fs) <$> pFlds
                )
          <|> pPacked' pOROWSUM pCROWSUM
                (    pExtFlds Type_RowSumEmpty Type_RowSumUpdate
                <|> (\fs r -> Type_RowSumUpdate r (Type_RowSumEmpty r) fs) <$> pFlds
                )
          where pInParens :: HSParser (Range -> Type)
                pInParens
                  =   (pType
                       <**> (   pSucceed (flip Type_Parenthesized)
                            <|> (\es e r -> Type_RowRecUpdate r (Type_RowRecEmpty r)
                                              (map (RowTypeUpdate_Extends r Nothing) (e:es)))
                                <$>  pList1 (pComma *> pType)
                            <|> (\(o,_) e r -> Type_SectionApplication r (Just e) o Nothing)
                                <$> pTypeOpBaseEq
                      )     )
                  <|> (pTypeOpBaseEq
                       <**> (   (\e (o,_) r -> Type_SectionApplication r Nothing o (Just e)) <$> pType
                            -- <|> pSucceed (\(o,_) r -> Type_SectionApplication r Nothing o Nothing)
                      )     )
                  <|> flip Type_TupleConstructor <$> commas_arity
                  <|> pSucceed (\r -> Type_RowRecEmpty r)
                  <|> (\fs r -> Type_RowRecUpdate r (Type_RowRecEmpty r) fs) <$> pFlds
                  <|> pExtFlds Type_RowRecEmpty Type_RowRecUpdate
                pFld :: HSParser (Type -> RowTypeUpdate)
                pFld = qvarid
                       <**> (   (\l -> RowTypeUpdate_Extends (mkRange1 l) (Just (tokMkQName l))) <$ pDCOLON
                            )
                pFlds :: HSParser [RowTypeUpdate]
                pFlds = pList1Sep pComma (pFld <*> pType)
                pExtFlds :: (Range -> Type) -> (Range -> Type -> [RowTypeUpdate] -> Type) -> HSParser (Range -> Type)
                pExtFlds semEmp semFromRow
                     = (\e fs r -> semFromRow r e fs)
                       <$> (   mkRngNm Type_Variable <$> qvarid
                           <|> pSucceed (semEmp emptyRange)
                           )
                       <*  pVBAR <*> pFlds

{-# LINE 781 "src/ehc/HS/Parser.chs" #-}
        pType' :: HSParser (Type,Range) -> (HSParser Type -> HSParser (Type,Int) -> HSParser (Type,Int)) -> HSParser Type
        pType' pOp extend
          = pT'
          where pT' :: HSParser Type
                pT' = mkT <$> pT
                pT :: HSParser (Type,Int)
                pT = extend pT'
                     $ pTypeApp
                        <**> (   pSucceed unit
                             <|> (\(op,rng) (r,cnt) l -> (Type_InfixApplication rng l op r,cnt+1)) <$> pOp <*> pT
                             )
                unit e    = (e,0)
                mkT (e,0) =  e
                mkT (e,_) =  Type_InfixApplicationChainTop emptyRange e

{-# LINE 802 "src/ehc/HS/Parser.chs" #-}
        pType ::  HSParser Type
        pType
          = pType'
              pTypeOp
              (\pT pTApp ->
                         pTApp
                     <|> (\c t -> unit $ Type_Qualified emptyRange [c] t) <$> pContextItemImpl <* pRARROW <*> pT
                     <|> unit <$> (pTypeQuantPrefix <*> pT)
              )
          where unit e    = (e,0)
{-
          = mkT <$> pT
          where pT :: HSParser (Type,Int)
                pT = pTypeApp
                      <**> (   pSucceed unit
                           <|> (\(op,rng) (r,cnt) l -> (Type_InfixApplication rng l op r,cnt+1)) <$> pTypeOp <*> pT
                           )
                     <|> (\c t -> unit $ Type_Qualified emptyRange [c] t) <$> pContextItemImpl <* pRARROW <*> pType
                     <|> unit <$> (pTypeQuantPrefix <*> pType)
                unit e    = (e,0)
                mkT (e,0) =  e
                mkT (e,_) =  Type_InfixApplicationChainTop emptyRange e
-}

{-# LINE 836 "src/ehc/HS/Parser.chs" #-}
        pTypeQuantPrefix :: HSParser (Type -> Type)
        pTypeQuantPrefix
          =  ((Type_Forall . mkRange1) <$> pFORALL <|> (Type_Exists . mkRange1) <$> pEXISTS)
             <*> (tokMkQNames <$> pTyVarBinds) <* pDOT

{-# LINE 843 "src/ehc/HS/Parser.chs" #-}
        pTypeOpPrefix :: HSParser (Type -> Type)
        pTypeOpPrefix
          =   (\l (op,rng) r -> Type_InfixApplication rng l op r) <$> pTypeApp <*> pTypeOp
          <|> (\c -> Type_Qualified emptyRange [c]) <$> pContextItemImpl <* pRARROW

{-# LINE 852 "src/ehc/HS/Parser.chs" #-}
        pTypeOp :: HSParser (Type,Range)
        pTypeOp
          =   pTypeOpBaseEq
          <|> mkRngNm' Type_Variable    <$> varop_no_ty
          <|> mkRngNm' Type_Constructor <$> pDARROW

{-# LINE 864 "src/ehc/HS/Parser.chs" #-}
        pTypeOpBase :: HSParser (Type,Range)
        pTypeOpBase
          = mkRngNm' Type_Constructor
            <$> (   gtycon_for_insection
                )

{-# LINE 872 "src/ehc/HS/Parser.chs" #-}
        pTypeOpBaseEq :: HSParser (Type,Range)
        pTypeOpBaseEq
          =   pTypeOpBase
          <|> mkRngNm' Type_Constructor <$> pTILDE

{-# LINE 885 "src/ehc/HS/Parser.chs" #-}
        pTypeApp :: HSParser Type
        pTypeApp
          =  pT <??> pA
          where pT = pTypeBase
                pA = (\es e -> Type_NormalApplication emptyRange e es) <$> pList1 pT

{-# LINE 893 "src/ehc/HS/Parser.chs" #-}
        pTyVarBind :: HSParser Token
        pTyVarBind =  tyvar

        pTyVarBinds :: HSParser [Token]
        pTyVarBinds =  pList1 pTyVarBind

{-# LINE 907 "src/ehc/HS/Parser.chs" #-}
        pTypeLeftHandSide :: HSParser TypeLeftHandSide
        pTypeLeftHandSide
          =   pLhs
          <|> (\c -> mkRngNm TypeLeftHandSide_Function c []) <$> gtycon' tyconsym
          <?> "pTypeLeftHandSide"
          where pLhs     :: HSParser TypeLeftHandSide
                pLhs     =   mkRngNm TypeLeftHandSide_Function <$> gtycon' tyconsym <*> pLhsTail
                         <|> pParens'
                               (   (\l r t -> TypeLeftHandSide_Parenthesized r l t)
                                   <$> pLhs
                               )
                             <*> pLhsTail
                         <|> (\l o r -> TypeLeftHandSide_Infix (mkRange1 o) l (tokMkQName o) r)
                             <$> pTypePatternBase <*> tyconop <*> pTypePatternBase
                pLhsTail ::  HSParser [TypePattern]
                pLhsTail =   pList1 pTypePatternBase

{-# LINE 926 "src/ehc/HS/Parser.chs" #-}
        pAnnotatedType :: HSParser Type -> HSParser Type
        pAnnotatedType pT
          =   (\(r,s) t -> if s
                           then Type_Annotate r TypeAnnotation_Strict t
                           else t
              )
              <$> ((\t -> (mkRange1 t,True)) <$> pBANG <|> pSucceed (emptyRange,False))
              <*> pT

{-# LINE 949 "src/ehc/HS/Parser.chs" #-}
        pContextItemsPrefix1 :: HSParser ContextItems
        pContextItemsPrefix1
          =   (:[]) <$> pContextItemImpl <* pRARROW

        pContextItemsPrefix2 :: HSParser ContextItems
        pContextItemsPrefix2
          =   (   (:[]) <$> (pContextItemBase <|> pContextItemImplWild)
              <|> pParens ((:) <$> pContextItemBase
                               <*> (   pImO
                                   <|> (++) <$> pList1 (pCOMMA *> pContextItemBase) <*> pImO
                          )        )
              )
              <*  pDARROW
          where pImO  =  (:[]) <$ pCOMMA <*> pContextItemImplWild `opt` []
                pImO  :: HSParser ContextItems

        pContextItemsPrefix :: HSParser ContextItems
        pContextItemsPrefix
          =   pContextItemsPrefix1
          <|> pContextItemsPrefix2

        pContextItemsPrefixOpt :: HSParser ContextItems
        pContextItemsPrefixOpt = pContextItemsPrefix <|> pSucceed []

        pTypeContextPrefix :: HSParser (Type -> Type)
        pTypeContextPrefix
          = Type_Qualified emptyRange <$> pContextItemsPrefix

{-# LINE 983 "src/ehc/HS/Parser.chs" #-}
        pContextItemClass :: HSParser ContextItem
        pContextItemClass
          =    mkRngNm ContextItem_Class <$> qconid <*> pList1 pTypeBase

{-# LINE 989 "src/ehc/HS/Parser.chs" #-}
        pContextItemPrefix :: HSParser (ContextItem -> ContextItem)
        pContextItemPrefix
          =   (ContextItem_Forall . mkRange1) <$> pFORALL <*> (tokMkQNames <$> pTyVarBinds) <* pDOT

{-# LINE 995 "src/ehc/HS/Parser.chs" #-}
        pContextItem :: HSParser ContextItem
        pContextItem
          =   pContextItemBase
              <**> (   pSucceed id
                   <|> (\o r l -> ContextItem_Arrow (mkRange1 o) l r) <$> pDARROW <*> pContextItem
                   )
          <|> pContextItemPrefix <*> pContextItem

{-# LINE 1009 "src/ehc/HS/Parser.chs" #-}
        pContextItemImplWild :: HSParser ContextItem
        pContextItemImplWild = (ContextItem_Implicits . mkRange1) <$> pTDOT

{-# LINE 1014 "src/ehc/HS/Parser.chs" #-}
        pContextItemImpl :: HSParser ContextItem
        pContextItemImpl
          = pImpls'
              (    const <$> (pContextItem <|> pContextItemImplWild)
              <|>  pSucceed ContextItem_NoImplicits
              )

{-# LINE 1023 "src/ehc/HS/Parser.chs" #-}
        pContextItemBase ::   HSParser ContextItem
        pContextItemBase
          =   pContextItemClass
          <|> tyvar <**>  (    (\s tv -> mkRngNm ContextItem_RowLacksLabel tv (tokMkQName s))
                               <$ pLAM <*> pSelector
                          )

{-# LINE 1047 "src/ehc/HS/Parser.chs" #-}
        pLiteralNumber :: HSParser Literal
        pLiteralNumber
          =   mk  8 <$> pInteger8Tk
          <|> mk 10 <$> pInteger10Tk
          <|> mk 16 <$> pInteger16Tk
          <?> "pLiteralNumber"
          where mk b t = Literal_Int (mkRange1 t) b (tokMkStr t)

{-# LINE 1063 "src/ehc/HS/Parser.chs" #-}
        pLiteral :: HSParser Literal
        pLiteral
          =   pLiteralNumber
          <|> mkRngStr Literal_Char <$> pCharTk
          <|> mkRngStr Literal_String <$> pStringTk
          <|> mkRngStr Literal_Float  <$> pFractionTk
          <?> "pLiteral"

{-# LINE 1081 "src/ehc/HS/Parser.chs" #-}
        pExpressionMinusPrefix :: HSParser (Expression -> Expression)
        pExpressionMinusPrefix
          =   (Expression_Negate . mkRange1) <$> pMINUS

        pExpressionMbMinusPrefix :: HSParser (Expression -> Expression)
        pExpressionMbMinusPrefix
          =   pExpressionMinusPrefix
          <|> pSucceed id

        pExpressionBase :: HSParser Expression
        pExpressionBase
          =   Expression_Literal emptyRange  <$> pLiteral
          <|> mkRngNm Expression_Variable    <$> qvar
          <|> pExpressionList
          <|> pParens' pInParens
          <?> "pExpressionBase"
          where pInParens :: HSParser (Range -> Expression)
                pInParens
        {-
                  =   (\(e,res) r ->
                         let mk res e
                               = case res of
                                   Expression3OpSection_None
                                     -> Expression_Parenthesized r e
                                   Expression3OpSection_Op (o,_)
                                     -> Expression_SectionApplication r (Just e) o Nothing
                                   Expression3OpSection_CommaList es
                                     -> Expression_RowRecordUpdate r (Expression_RowRecordEmpty r)
                                                                   (map (RowRecordExpressionUpdate_Extends r Nothing) (e:es))
                                   Expression3OpSection_Typed (t,r)
                                     -> Expression_Typed r e t
                         in foldr mk e $ reverse res
                      )
                      <$> pExpression3OpSection pOp pExpressionPreBase
        -}
        {-
        -}
                  =   (\(e,_,res) r ->
                         let chk ress e
                               = case ress of
                                   (Expression4Result_Op (o,_) : _)
                                     -> Expression_SectionApplication r (Just e) o Nothing
                                   (Expression4Result_CommaList es : _)
                                     -> Expression_RowRecordUpdate r (Expression_RowRecordEmpty r)
                                                                   (map (RowRecordExpressionUpdate_Extends r Nothing) (e:es))
                                   _ -> Expression_Parenthesized r e
                         in chk res e
                      )
                      <$> pExpression4'' True pOp pExpressionPrefix pExpressionLayout
        {-
                  =   (pExpression <**>
                            (   (\(o,_) e r -> Expression_SectionApplication r (Just e) o Nothing)
                                <$> pOp
                            <|> pSucceed (flip Expression_Parenthesized)
                            <|> (\es e r -> Expression_RowRecordUpdate r (Expression_RowRecordEmpty r)
                                              (map (RowRecordExpressionUpdate_Extends r Nothing) (e:es)))
                                <$> pList1 (pComma *> pExpression)
                      )     )
        -}
                  <|> flip Expression_TupleConstructor <$> commas_arity
                  <|> (pOpm
                       <**> (   (\e (o,_) r -> Expression_SectionApplication r Nothing o (Just e)) <$> pExpression
                            -- <|> pSucceed (\(o,_) r -> Expression_SectionApplication r Nothing o Nothing)
                      )     )
                  <|> pSucceed (\r -> Expression_RowRecordEmpty r)
                  <|> (\fs r -> Expression_RowRecordUpdate r (Expression_RowRecordEmpty r) fs) <$> pFlds
                  <|> pExtFlds
                  where pFld :: HSParser (Expression -> RowRecordExpressionUpdate)
                        pFld = qvarid
                               <**> (   (\l -> RowRecordExpressionUpdate_Extends (mkRange1 l) (Just (tokMkQName l))) <$ pEQUAL
                                    <|> mkRngNm RowRecordExpressionUpdate_Update <$ pCOLEQUAL
                                    )
                        pFlds :: HSParser [RowRecordExpressionUpdate]
                        pFlds = pList1Sep pComma (pFld <*> pExpression)
                        pExtFlds :: HSParser (Range -> Expression)
                        pExtFlds
                             = (\e fs r -> Expression_RowRecordUpdate r e fs)
                               <$> (   pParens' pExtFlds
                                   <|> mkRngNm Expression_Variable <$> qvarid
                                   <|> pSucceed (Expression_RowRecordEmpty emptyRange)
                                   )
                               <*  pVBAR <*> pFlds

{-# LINE 1185 "src/ehc/HS/Parser.chs" #-}
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
          <?> "pExpressionList"
          where zero r   = Expression_List r []
                one  r h = Expression_List r [h]
                pQualifier :: HSParser Qualifier
                pQualifier
                  =   Qualifier_Guard emptyRange <$> pExpressionNoLet
                  <|> (Qualifier_Let . mkRange1) <$> pLET <*> pDeclarations
                  <|> Qualifier_Generator emptyRange <$> pPattern <* pLARROW <*> pExpression

{-# LINE 1225 "src/ehc/HS/Parser.chs" #-}
        pExpressionDo :: HSParser Expression
        pExpressionDo
          =   (Expression_Do . mkRange1) <$> pDO <*> pBlock1 pOCURLY pSEMI pCCURLY pStatement
          <?> "pExpressionDo"
          where pStatement :: HSParser Statement
                pStatement
                  =   Statement_Expression emptyRange <$> pExpression {- pExpressionNoLet -}
                  <|> (\p t e -> Statement_Generator (mkRange1 t) p e) <$> pPattern <*> pLARROW <*> pExpression
                      -- common prefix with 'let x=e in e' dies out
                  <|> (Statement_Let . mkRange1) <$> pLET <*> pDeclarations

{-# LINE 1273 "src/ehc/HS/Parser.chs" #-}
        pExpressionConUpd :: HSParser Expression
        pExpressionConUpd
          =   qcon
              <**> (   pSucceed (mkRngNm Expression_Constructor)
                   <|> pCurlys' ((\bs _ c -> mkRngNm Expression_RecordConstruction c bs) <$> pListSep pCOMMA pRecordExpressionBinding)
                   )
          <|> pExpressionBase
              <**> ((\u e -> foldr ($) e u) <$> pList pU)
          <?> "pExpressionConUpd"
          where pU =   pCurlys' ((\bs r e -> Expression_RecordUpdate r e bs) <$> pList1Sep pCOMMA pRecordExpressionBinding)
                   <|> pRowRecordSelectionSuffix

{-# LINE 1293 "src/ehc/HS/Parser.chs" #-}
        pRecordExpressionBinding :: HSParser RecordExpressionBinding
        pRecordExpressionBinding
          =   mkRngNm RecordExpressionBinding_Binding <$> qvar <* pEQUAL <*> pExpression
          <?> "pRecordExpressionBinding"

{-# LINE 1300 "src/ehc/HS/Parser.chs" #-}
        pExpressionApp :: HSParser Expression
        pExpressionApp
          =   pE <**> ((\as e -> foldl (flip ($)) e as) <$> pList pA)
          <?> "pExpressionApp"
          where pE =   pExpressionConUpd
                pA =   (\es e -> Expression_NormalApplication emptyRange e es) <$> pList1 pE
                   <|> (\es e -> Expression_ImpredicativeApplication emptyRange e es) <$> pList1 (pTILDE *> pE)
                   <|> (\es e -> Expression_ImplicitApplication emptyRange e es) <$> pList1 (pImpls' pContextedExpression)
                   where pContextedExpression = (\e c r -> ContextedExpression_Contexted r e c) <$> pExpression <* pLTCOLON <*> pContextItem
                         pContextedExpression :: HSParser (Range -> ContextedExpression)

{-# LINE 1317 "src/ehc/HS/Parser.chs" #-}
        pExpressionLayout :: HSParser Expression
        pExpressionLayout
          =   pMaybe id id pExpressionMinusPrefix <*> pExpressionApp
          <|> (Expression_Case . mkRange1) <$> pCASE <*> pExpression <* pOptSEMISeparator <* pOF <*> pAlternatives
          <|> pExpressionDo
          <?> "pExpressionLayout"

{-# LINE 1330 "src/ehc/HS/Parser.chs" #-}
        pOp, pOpm :: HSParser (Expression,Range)
        pOp  = mkRngNm' Expression_Variable <$> qvarop          <|> mkRngNm' Expression_Constructor <$> qconop
        pOpm = mkRngNm' Expression_Variable <$> qvarop_no_minus <|> mkRngNm' Expression_Constructor <$> qconop

{-# LINE 1336 "src/ehc/HS/Parser.chs" #-}
        pExpression4'' :: Bool -> HSParser (Expression,Range) -> HSParser (Expression -> Expression) -> HSParser Expression -> HSParser Expression4
        pExpression4'' inParen pOp pPreNotOp pBase
          =   ((\(e,cnt,res) -> (mkC cnt e,0,res)) <$> pE)
              <**> (addCommaP $ addOpP
                    $ (   (addCommaP2
                           $ ((\c t (e,cnt,res) -> (Expression_Typed (mkRange1 c) (mkC cnt e) t, 0, Expression4Result_Typed : res))
                              <$> pDCOLON <*> pType
                          )  )
                      <|> pSucceed id
                   )  )
          where pE  ::  HSParser Expression4
                pE  =   pBase <**>
                          (   pSucceed (\e -> (e,0,[]))
                          <|> (\(op,rng) (r,opCnt,res) l -> (Expression_InfixApplication rng l op r, opCnt+1, res)) <$> pOp <*> pE
                          )
                    <|> (\p (e,cnt,res) -> (p $ mkC cnt $ e, 0, Expression4Result_NotOpPre : res))
                        <$> pPreNotOp <*> pE

                -- add trailing parsers, depending on being inside parenthesis
                addCommaP, addCommaP2, addOpP :: HSParser (Expression4 -> Expression4) -> HSParser (Expression4 -> Expression4)

                -- optionally add tuple expr remainder as choice
                addCommaP  p | inParen   = p <|> (\es (e,cnt,res) -> (mkC cnt e, 0, Expression4Result_CommaList es : res))
                                                 <$> pList1 (pComma *> pExpression)
                             | otherwise = p

                -- optionally add tuple expr remainder as following in a sequence
                addCommaP2 p | inParen   = (\mkecntres es ecntres ->
                                              let (e,cnt,res) = mkecntres ecntres
                                              in  (mkC cnt e, 0, (if null es then [] else [Expression4Result_CommaList es]) ++ res)
                                           )
                                           <$> p <*> pList (pComma *> pExpression)
                             | otherwise = p

                -- optionally add operator as choice
                addOpP     p | inParen   = p <|> (\o (e,cnt,res) -> (mkC cnt e, 0, Expression4Result_Op o : res))
                                                 <$> pOp
                             | otherwise = p

                -- add additional AST depending on nr of operators
                mkC cnt = if cnt > 0 then Expression_InfixApplicationChainTop emptyRange else id

        pExpression4' :: HSParser (Expression -> Expression) -> HSParser Expression
        pExpression4' pPreNotOp = (\(e,_,_) -> e) <$> pExpression4'' False pOp pPreNotOp pExpressionLayout

{-# LINE 1383 "src/ehc/HS/Parser.chs" #-}
        pExpressionPreBase :: HSParser Expression
        pExpressionPreBase = (\ps e -> foldr ($) e ps) <$> pList_gr pExpressionPrefix <*> pExpressionLayout

        pExpression :: HSParser Expression
        pExpression
          -- =   pExpression1' pExpressionPrefix
          -- =   pExpression2' pExpressionPrefix
          -- = pExpression3' pExpressionPreBase
          = pExpression4' pExpressionPrefix
            <?> "pExpression"

{-# LINE 1396 "src/ehc/HS/Parser.chs" #-}
        pExpressionNoLet :: HSParser Expression
        pExpressionNoLet
          -- =   pExpression' pExpressionNoLetPrefix
          -- =   pExpression2' pExpressionNoLetPrefix
          -- = pExpression3' pBase3
          =   pExpression4' pExpressionNoLetPrefix
            <?> "pExpressionNoLet"
          where pBase3 :: HSParser Expression
                pBase3 = (\ps e -> foldr ($) e ps) <$> pList pExpressionNoLetPrefix <*> pExpressionLayout

{-# LINE 1408 "src/ehc/HS/Parser.chs" #-}
        pExpressionLetPrefix :: HSParser (Expression -> Expression)
        pExpressionLetPrefix
          =   (\(s,t,d) -> Expression_Let (mkRange1 t) s d)
              <$> (   (,,) False <$> pLET       <*> pDeclarations                       <* pIN
                  <|> (,,) True  <$> pLETSTRICT <*> pLayoutList pDeclarationSimpleValue <* pIN
                  )
          <?> "pExpressionLetPrefix"

{-# LINE 1424 "src/ehc/HS/Parser.chs" #-}
        pExpressionNoLetPrefix :: HSParser (Expression -> Expression)
        pExpressionNoLetPrefix
          =   pLAM <**> pLamArgs
          <|> (Expression_If . mkRange1) <$> pIF <*> pExpression <* pOptSEMISeparator <* pTHEN <*> pExpression <* pOptSEMISeparator <* pELSE
          -- <|> pExpressionMinusPrefix
          <?> "pExpressionNoLetPrefix"
          where pLamArgs
                  =   (\a1 a2 t e -> a1 t (a2 t e))
                      <$> (   (\ps t e -> Expression_Lambda (mkRange1 t) ps e) <$> pList1 pPatternBaseCon
                          <|> (\ps t e -> Expression_ImplicitLambda (mkRange1 t) ps e) <$> pList1 (pImpls' pContextedPattern)
                          )
                      <*> pLamArgs
                  <|> (\_ e -> e) <$ pRARROW
                pContextedPattern = (\p c r -> ContextedPattern_Contexted r p c) <$> pPattern <* pLTCOLON <*> pContextItem
                pContextedPattern :: HSParser (Range -> ContextedPattern)

{-# LINE 1448 "src/ehc/HS/Parser.chs" #-}
        pExpressionPrefix :: HSParser (Expression -> Expression)
        pExpressionPrefix
          =   pExpressionLetPrefix
          <|> pExpressionNoLetPrefix
          <?> "pExpressionPrefix"

{-# LINE 1460 "src/ehc/HS/Parser.chs" #-}
        pAlternative :: HSParser Alternative
        pAlternative
          = Alternative_Alternative emptyRange <$> pPattern <*> pRhs pRARROW

        pAlternatives :: HSParser Alternatives
        pAlternatives
          = pBlock1 pOCURLY pSEMI pCCURLY pAlternative

{-# LINE 1474 "src/ehc/HS/Parser.chs" #-}
        pPatternBaseInParens :: HSParser (Range -> Pattern)
        pPatternBaseInParens
          =   (pPattern
               <**> (   pSucceed (flip Pattern_Parenthesized)
                    <|> (\es e r -> Pattern_RowRecordBinding r (Pattern_RowRecordEmpty r)
                                      (map (RowRecordPatternBinding_Binding r Nothing) (e:es)))
                        <$>  pList1 (pComma *> pPattern)
              )     )
          <|> (\v _ -> mkRngNm Pattern_Variable v) <$> qvarsym_for_inparens
          <|> pSucceed (\r -> Pattern_RowRecordEmpty r)
          <|> (\fs r -> Pattern_RowRecordBinding r (Pattern_RowRecordEmpty r) fs) <$> pFlds
          <|> pExtFlds
          <?> "pPatternBaseInParens"
          where pFld :: HSParser (Pattern -> RowRecordPatternBinding)
                pFld = qvarid
                       <**> (   (\l -> RowRecordPatternBinding_Binding (mkRange1 l) (Just (tokMkQName l))) <$ pEQUAL
                            )
                pFlds :: HSParser [RowRecordPatternBinding]
                pFlds = pList1Sep pComma (pFld <*> pPattern)
                pExtFlds :: HSParser (Range -> Pattern)
                pExtFlds
                     = (\e fs r -> Pattern_RowRecordBinding r e fs)
                       <$> (   mkRngNm Pattern_Variable <$> qvar
                           <|> pSucceed (Pattern_RowRecordEmpty emptyRange)
                           )
                       <*  pVBAR <*> pFlds

{-# LINE 1512 "src/ehc/HS/Parser.chs" #-}
        pPatternBaseMinusLiteral :: HSParser Pattern
        pPatternBaseMinusLiteral = (\m n -> Pattern_Literal (mkRange1 m) (-1) n) <$> pMINUS <*> pLiteralNumber

{-# LINE 1517 "src/ehc/HS/Parser.chs" #-}
        pPatternBaseNoParens :: HSParser Pattern
        pPatternBaseNoParens
          = (
              (if ehcOptBangPatterns opts
               then (\p -> p <|> (Pattern_Bang . mkRange1) <$> pBANG  <*> pPatternBaseCon)
               else id
              ) $
              (   qvarid
                  <**> (   (\a p v -> Pattern_As (mkRange1 a) (tokMkQName v) p) <$> pAT <*> pPatternBaseCon
                       <|> pSucceed (mkRngNm Pattern_Variable)
                       )
              <|> Pattern_Literal emptyRange 1 <$> pLiteral
              <|> pBracks' (flip Pattern_List <$> pListSep pCOMMA pPattern)
              <|> (Pattern_Irrefutable . mkRange1) <$> pTILDE <*> pPatternBaseCon
            ) )
          <?> "pPatternBaseNoParens"

{-# LINE 1543 "src/ehc/HS/Parser.chs" #-}
        pPatternBase :: HSParser Pattern
        pPatternBase
          =   pPatternBaseNoParens
          <|> pParens' pPatternBaseInParens
          <?> "pPatternBase"

{-# LINE 1551 "src/ehc/HS/Parser.chs" #-}
        pPatternConSuffix :: HSParser (Token -> Pattern)
        pPatternConSuffix
          =   pSucceed (\c -> mkRngNm Pattern_Constructor c [])
          <|> pCurlys' ((\bs _ c -> mkRngNm Pattern_Record c bs) <$> pListSep pCOMMA pRecordPatternBinding)
          <?> "pPatternConSuffix"

{-# LINE 1561 "src/ehc/HS/Parser.chs" #-}
        pPatternBaseCon :: HSParser Pattern
        pPatternBaseCon
          =   pPatternBase
          <|> qconid <**> pPatternConSuffix
          <?> "pPatternBaseCon"

{-# LINE 1569 "src/ehc/HS/Parser.chs" #-}
        pRecordPatternBinding :: HSParser RecordPatternBinding
        pRecordPatternBinding
          =   qvar
              <**> (   pSucceed (\v -> mkRngNm RecordPatternBinding_Pun v)
                   <|> (\p v -> mkRngNm RecordPatternBinding_Binding v p) <$ pEQUAL <*> pPattern
                   )
          <?> "pRecordPatternBinding"

{-# LINE 1579 "src/ehc/HS/Parser.chs" #-}
        pPatternApp :: HSParser Pattern
        pPatternApp
          =   pPatternBase
          <|> pPatternBaseMinusLiteral
          <|> qcon
              <**> (   (\l c -> mkRngNm Pattern_Constructor c l) <$> pList1 pPatternBaseCon
                   <|> pPatternConSuffix
                   )
          <|> (Pattern_Tuple emptyRange) <$> pParens commas_arity <*> pList1 pPatternBaseCon
          <?> "pPatternApp"

{-# LINE 1594 "src/ehc/HS/Parser.chs" #-}
        pPatternOp :: HSParser Pattern
        pPatternOp
          -- =   (\l rs -> foldr (\(o,r) mk -> \l -> o l (mk r)) id rs l) <$> pPatternApp <*> pList_ng (pOp <+> pPatternApp)
          = pChainr_ng pOp pPatternApp
          <?> "pPatternOp"
          where pOp =
                    ((\o l r -> Pattern_InfixConstructor (mkRange1 o) l (tokMkQName o) r) <$> qconop)

{-# LINE 1608 "src/ehc/HS/Parser.chs" #-}
        pPattern :: HSParser Pattern
        pPattern
          =   pPatternOp
              <??> ((\c t p -> Pattern_Typed (mkRange1 c) p t) <$> pDCOLON <*> pType)
          <?> "pPattern"

{-# LINE 1618 "src/ehc/HS/Parser.chs" #-}
        pRowRecordSelectionSuffix :: HSParser (Expression -> Expression)
        pRowRecordSelectionSuffix
          = (\lbls e -> foldl (\e l -> Expression_RowRecordSelect (mkRange1 l) e (tokMkQName l)) e lbls)
            <$> pList1 (pHASH *> pSelector)

{-# LINE 1625 "src/ehc/HS/Parser.chs" #-}
        pSelector :: HSParser Token
        pSelector
          =   qvarid <|> qconid <|> pIntegerTk

{-# LINE 1635 "src/ehc/HS/Parser.chs" #-}
        pTypePatternBase :: HSParser TypePattern
        pTypePatternBase
          =   mkRngNm TypePattern_Variable <$> var_no_ty
          <?> "pTypePatternBase"


{-# LINE 1657 "src/ehc/HS/Parser.chs" #-}
commas' :: HSParser [Token]
commas' = pList1 pCOMMA

commas :: HSParser Token
commas =  (map (\s -> strProd (length s + 1)) . foldr tokConcat tokEmpty) <$> commas'
  where map = tokenMap

{-# LINE 1670 "src/ehc/HS/Parser.chs" #-}
commas_arity :: HSParser Int
commas_arity =  (\ts -> length ts + 1) <$> commas'

{-# LINE 1677 "src/ehc/HS/Parser.chs" #-}
pOptSEMISeparator :: HSParser (Maybe ())
pOptSEMISeparator = pMb (pSeparator <|> () <$ pSEMI)

{-# LINE 1682 "src/ehc/HS/Parser.chs" #-}
modid :: HSParser Token
modid
  =   pCONID
  <|> pQCONID
  <?> "modid"

{-# LINE 1692 "src/ehc/HS/Parser.chs" #-}
qcnames :: HSParser [Token]
qcnames
  =   pListSep pCOMMA qcname
  <?> "qcnames"

qcname  :: HSParser Token   -- Variable or data constructor
qcname
  =   qvar
  <|> gcon
  <?> "qcname"

{-# LINE 1705 "src/ehc/HS/Parser.chs" #-}
identifier :: HSParser Token
identifier
  =   qvar
  <|> gcon
  <|> qop
  <?> "identifier"

depreclist :: HSParser [Token]
depreclist
  = pList1Sep pCOMMA deprec_var

deprec_var :: HSParser Token
deprec_var
  =   var
  <|> tyconid
  <?> "deprec_var"

gcon    :: HSParser Token   -- Data constructor namespace
gcon
  =   sysdcon
  <|> qcon
-- the case of '[:' ':]' is part of the production `parr'
  <?> "gcon"

sysdcon :: HSParser Token   -- Wired in data constructors
sysdcon
  =   pParens commas
  <|> tokConcat <$> pOBRACK <*> pCBRACK
  <?> "sysdcon"


{-# LINE 1744 "src/ehc/HS/Parser.chs" #-}
qcon    :: HSParser Token
qcon
  =   qconid
  <|> pParens qconsym
  <?> "qcon"



conop :: HSParser Token
conop
  =   consym
  <|> pBACKQUOTE *> conid <* pBACKQUOTE
  <?> "conop"

qconop :: HSParser Token
qconop
  =   qconsym
  <|> pBACKQUOTE *> qconid <* pBACKQUOTE
  <?> "qconop"

{-# LINE 1771 "src/ehc/HS/Parser.chs" #-}
qconid :: HSParser Token    -- Qualified or unqualifiedb
qconid
  =   conid
  <|> pQCONID
  <?> "qconid"

{-# LINE 1781 "src/ehc/HS/Parser.chs" #-}
conid_nopragma   :: HSParser Token
conid_nopragma
  =   pCONID
  <?> "conid_nopragma"

conid   :: HSParser Token
conid
  =   conid_nopragma
  <|> pLANGUAGE_prag
  <|> pDERIVABLE_prag
  <?> "conid"

qconsym :: HSParser Token   -- Qualified or unqualified
qconsym
  =   consym
  <|> pQCONSYM
  <?> "qconsym"

{-# LINE 1805 "src/ehc/HS/Parser.chs" #-}
consym :: HSParser Token
consym
  =   pCONSYM
  <|> pCOLON -- ':' means only list cons
  <?> "consym"

{-# LINE 1813 "src/ehc/HS/Parser.chs" #-}
con :: HSParser Token
con
  =   conid
  <|> pParens consym
  <?> "con"

{-# LINE 1821 "src/ehc/HS/Parser.chs" #-}
-----------------------------------------------------------------------------
-- Any operator

op  :: HSParser Token   -- used in infix decls
op
  =   varop
  <|> conop
  <?> "op"

qop :: HSParser Token   -- used in sections
qop
  =   qvarop
  <|> qconop
  <?> "qop"



{-# LINE 1858 "src/ehc/HS/Parser.chs" #-}
tyvar   :: HSParser Token
tyvar
  =   varid_no_ty
  <?> "tyvar"

{-# LINE 1869 "src/ehc/HS/Parser.chs" #-}
-- | special identifier: FFI safety
safety :: HSParser Token
safety
  =   pSAFE
  <|> pUNSAFE
  <|> pTHREADSAFE
  <?> "safety"

{-# LINE 1881 "src/ehc/HS/Parser.chs" #-}
-- | special identifier: FFI calling convention
callconv :: HSParser Token
callconv
  =   snd <$> pFFIWay
  <?> "callconv"

{-# LINE 1889 "src/ehc/HS/Parser.chs" #-}
-- These special_ids are treated as keywords in various places,
-- but as ordinary ids elsewhere.   'special_id' collects all these
-- except 'unsafe' and 'forall' whose treatment differs depending on context
-- | special identifier: some context sensitive meaningful
special_id_no_callconv :: HSParser Token
special_id_no_callconv
  =   pLABEL
  <|> pEXPORT
  <|> pAS
  <|> pQUALIFIED
  <|> pHIDING
  <|> pDYNAMIC
  <?> "special_id_no_callconv"

{-# LINE 1909 "src/ehc/HS/Parser.chs" #-}
special_id :: HSParser Token
special_id
  =   special_id_no_callconv
  <|> callconv
  <?> "special_id"

{-# LINE 1917 "src/ehc/HS/Parser.chs" #-}
-- | Unqualified value/function, e.g.: f, except type related keywords
varid_no_ty :: HSParser Token
varid_no_ty
  =   pVARID
  <|> special_id_no_callconv
  <|> callconv
  <|> safety
  <|> pVARIDUNBOXED
  <?> "varid_no_ty"

{-# LINE 1933 "src/ehc/HS/Parser.chs" #-}
-- | Unqualified value/function, e.g.: f
varid :: HSParser Token
varid
  =   varid_no_ty
  <|> pFORALL
  <?> "varid"

{-# LINE 1944 "src/ehc/HS/Parser.chs" #-}
-- | (Un)qualified value/function, e.g.: X.f
qvarid :: HSParser Token
qvarid
  =   varid
  <|> pQVARID
  <?> "qvarid"

{-# LINE 1955 "src/ehc/HS/Parser.chs" #-}
-- | See comments with special_id
special_sym_no_bang :: HSParser Token
special_sym_no_bang
  =   pDOT
  <|> pSTAR
  <|> pPERCENT
  <?> "special_sym_no_bang"

special_sym :: HSParser Token
special_sym
  =   pBANG
  <|> special_sym_no_bang
  <?> "special_sym"

{-# LINE 1971 "src/ehc/HS/Parser.chs" #-}
-- | Unqualified operator, e.g.: +, except -, !
varsym_no_minus_bang :: HSParser Token
varsym_no_minus_bang
  =   pVARSYM
  <|> special_sym_no_bang
  <?> "varsym_no_minus_bang"

varsym_no_minus :: HSParser Token
varsym_no_minus
  =   pVARSYM
  <|> special_sym
  <?> "varsym_no_minus"

{-# LINE 1986 "src/ehc/HS/Parser.chs" #-}
-- | Unqualified operator, e.g.: +
varsym :: HSParser Token
varsym
  =   varsym_no_minus
  <|> pMINUS
  <?> "varsym"

{-# LINE 1995 "src/ehc/HS/Parser.chs" #-}
-- | Qualified operator, e.g.: X.+, only base
qvarsym_base :: HSParser Token
qvarsym_base
  =   pQVARSYM
  <?> "qvarsym1"

{-# LINE 2003 "src/ehc/HS/Parser.chs" #-}
-- | (Un)qualified operator, e.g.: X.+, except -
qvarsym_no_minus :: HSParser Token
qvarsym_no_minus
  =   varsym_no_minus
  <|> qvarsym_base
  <?> "qvarsym_no_minus"

{-# LINE 2014 "src/ehc/HS/Parser.chs" #-}
-- | (Un)qualified operator, e.g.: X.+, for use inside parens
qvarsym_for_inparens :: HSParser Token
qvarsym_for_inparens
  =   varsym
  <|> qvarsym_base
  <?> "qvarsym_for_inparens"

{-# LINE 2025 "src/ehc/HS/Parser.chs" #-}
-- | (Un)qualified operator, e.g.: X.+
qvarsym :: HSParser Token
qvarsym
  =   qvarsym_for_inparens
  <?> "qvarsym"

{-# LINE 2033 "src/ehc/HS/Parser.chs" #-}
-- | (Un)qualified operator, e.g.: +, `f`, except -, !
varop_no_minus_bang   :: HSParser Token
varop_no_minus_bang
  =   varsym_no_minus_bang
  <|> pBACKQUOTE *> varid <* pBACKQUOTE
  <?> "varop_no_minus_bang"

-- | (Un)qualified operator, e.g.: +, `f`, except -
varop_no_minus   :: HSParser Token
varop_no_minus
  =   varsym_no_minus
  <|> pBACKQUOTE *> varid <* pBACKQUOTE
  <?> "varop_no_minus"

{-# LINE 2049 "src/ehc/HS/Parser.chs" #-}
-- | (Un)qualified operator, e.g.: +, `f`, except type related keywords
varop_no_ty   :: HSParser Token
varop_no_ty
  =   varsym
  <|> pBACKQUOTE *> varid_no_ty <* pBACKQUOTE
  <?> "varop_no_minus"

{-# LINE 2058 "src/ehc/HS/Parser.chs" #-}
-- | (Un)qualified operator, e.g.: +, `f`, except !
varop_no_bang   :: HSParser Token
varop_no_bang
  =   varop_no_minus_bang
  <|> pMINUS
  <?> "varop_no_bang"

-- | (Un)qualified operator, e.g.: +, `f`
varop   :: HSParser Token
varop
  =   varop_no_minus
  <|> pMINUS
  <?> "varop"

{-# LINE 2074 "src/ehc/HS/Parser.chs" #-}
-- | (Un)qualified operator, e.g.: X.+, `X.f`, except -
qvarop_no_minus :: HSParser Token
qvarop_no_minus
  =   qvarsym_no_minus
  <|> pBACKQUOTE *> qvarid <* pBACKQUOTE
  <?> "qvarop_no_minus"

{-# LINE 2083 "src/ehc/HS/Parser.chs" #-}
-- | (Un)qualified operator, e.g.: X.+, `X.f`
qvarop :: HSParser Token
qvarop
  =   qvarsym
  <|> pBACKQUOTE *> qvarid <* pBACKQUOTE
  <?> "qvarop"

{-# LINE 2092 "src/ehc/HS/Parser.chs" #-}
-- | Unqualified value/function, e.g.: f, (+), except type related keywords
var_no_ty     :: HSParser Token
var_no_ty
  =   varid_no_ty
  <|> pParens varsym
  <?> "var"

{-# LINE 2101 "src/ehc/HS/Parser.chs" #-}
-- | Unqualified value/function, e.g.: f, (+)
var     :: HSParser Token
var
  =   varid
  <|> pParens varsym
  <?> "var"

{-# LINE 2110 "src/ehc/HS/Parser.chs" #-}
-- | (Un)qualified value/function, e.g.: X.f, (X.+)
qvar    :: HSParser Token
qvar
  =   qvarid
  <|> pParens qvarsym_for_inparens
  <?> "qvar"

{-# LINE 2127 "src/ehc/HS/Parser.chs" #-}
-- | Unqualified type constructor, e.g.: :+:
tyconsym :: HSParser Token
tyconsym
  =   pCONSYM
  <?> "tyconsym"

{-# LINE 2135 "src/ehc/HS/Parser.chs" #-}
-- | (Un)qualified type constructor, e.g.: X.:+:
qtyconsym :: HSParser Token
qtyconsym
  =   tyconsym
  <|> pQCONSYM
  <?> "qtyconsym"

{-# LINE 2146 "src/ehc/HS/Parser.chs" #-}
-- | Unqualified type constructor, e.g.: Either
tyconid   :: HSParser Token
tyconid
  =   pCONID
  <|> pCONIDUNBOXED
  <?> "tyconid"

{-# LINE 2157 "src/ehc/HS/Parser.chs" #-}
-- | (Un)qualified type constructor, e.g.: X.Either
qtyconid :: HSParser Token
qtyconid
  =   tyconid
  <|> pQCONID
  <?> "qtyconid"

{-# LINE 2168 "src/ehc/HS/Parser.chs" #-}
-- | Unqualified infix type operator, e.g.: `Either`, :+:
tyconop :: HSParser Token   -- Unqualified
tyconop
  =   tyconsym
  <|> pBACKQUOTE *> tyconid <* pBACKQUOTE
  <?> "tyconop"

{-# LINE 2177 "src/ehc/HS/Parser.chs" #-}
-- | (Un)qualified infix type operator, e.g.: `X.Either`, X.:+:
qtyconop :: HSParser Token
qtyconop
  =   qtyconsym
  <|> pBACKQUOTE *> qtyconid <* pBACKQUOTE
  <?> "qtyconop"

{-# LINE 2186 "src/ehc/HS/Parser.chs" #-}
-- | (Un)qualified prefix type constructor, e.g.: X.Either, (X.:+:)
qtycon :: HSParser Token
qtycon
  =   qtyconid
  <|> pParens qtyconsym
  <?> "qtycon"

{-# LINE 2195 "src/ehc/HS/Parser.chs" #-}
-- | General (un)qualified prefix type constructor, no delimiting brackets (e.g. []), to be parameterized with the part inside parenthesis (the general part)
gtycon_no_delims' :: HSParser Token -> HSParser Token
gtycon_no_delims' pInParens
  =   qtyconid
  <|> pParens pInParens
  <?> "gtycon_no_delims"

{-# LINE 2204 "src/ehc/HS/Parser.chs" #-}
-- | Inside parenthesis part for gtycon
gtycon_for_inparens_arrow :: HSParser Token
gtycon_for_inparens_arrow
  =   pRARROW
  <|> qtyconsym
  <?> "gtycon_for_inparens_arrow"

-- | Inside parenthesis part for gtycon
gtycon_for_inparens_arrow_commas :: HSParser Token
gtycon_for_inparens_arrow_commas
  =   gtycon_for_inparens_arrow
  <|> commas
  <?> "gtycon_for_inparens_arrow"

-- | Inside parenthesis part for type sections
gtycon_for_insection :: HSParser Token
gtycon_for_insection
  =   pRARROW
  <|> qtyconop
  <?> "gtycon_for_insection"

{-# LINE 2229 "src/ehc/HS/Parser.chs" #-}
-- | General (un)qualified prefix type constructor, e.g.: X.Either, (X.:+:), and (->)
gtycon_no_delims_commas :: HSParser Token
gtycon_no_delims_commas
  =   gtycon_no_delims' gtycon_for_inparens_arrow
  <?> "gtycon_no_delims_commas"

{-# LINE 2237 "src/ehc/HS/Parser.chs" #-}
-- | General (un)qualified prefix type constructor, e.g.: X.Either, (X.:+:), and (,,,), (->)
gtycon_no_delims :: HSParser Token   -- A "general" qualified tycon
gtycon_no_delims
  =   gtycon_no_delims' gtycon_for_inparens_arrow_commas
  <?> "gtycon_no_delims"

{-# LINE 2245 "src/ehc/HS/Parser.chs" #-}
gtycon_only_bracks :: HSParser Token   -- A "general" qualified tycon
gtycon_only_bracks
  =   tokConcat <$> pOBRACK <*> pCBRACK
  -- <|> tokConcat <$> pOPABRACK <*> pCPABRACK
  <?> "gtycon_only_delims"

{-# LINE 2253 "src/ehc/HS/Parser.chs" #-}
-- | General (un)qualified prefix type constructor, including delimiting brackets, to be parameterized with the part inside parenthesis (the general part)
gtycon' :: HSParser Token -> HSParser Token
gtycon' pInParens
  =   gtycon_no_delims' pInParens
  <|> gtycon_only_bracks
  <?> "gtycon'"

{-# LINE 2264 "src/ehc/HS/Parser.chs" #-}
-- | General (un)qualified prefix type constructor, e.g.: X.Either, (X.:+:), and (,,,), (->), []
gtycon :: HSParser Token   -- A "general" qualified tycon
gtycon
  =   gtycon' gtycon_for_inparens_arrow_commas
  <?> "gtycon"


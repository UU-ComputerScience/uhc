%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}EH.Parser} import(IO, UU.Parsing, UU.Parsing.Offside, EH.Util.ParseUtils, UU.Scanner.GenToken, {%{EH}Base.Builtin},{%{EH}Base.Common}, {%{EH}Scanner.Common}, {%{EH}EH})
%%]

%%[1 export(pAGItf)
%%]

%%[4 import ({%{EH}Ty})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Semantics classes for parsers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.SemParser
%%]
class SemApp e => SemParser e p t ds where
  semVar        ::  HsName -> e
  semLam        ::  p -> e -> e
  semLet        ::  ds -> e -> e
  semTypeAs     ::  t -> e -> e

%%[1
%%]
instance SemParser Expr PatExpr TyExpr Decls where
  semVar        =   Expr_Var
  semLam        =   Expr_Lam
  semLet        =   Expr_Let
  semTypeAs     =   Expr_TypeAs

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser signatures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.parserSigs
type EHCParser        ep    =    LayoutParser Token ep

pAGItf                      ::   EHCParser AGItf

pExpr, pExprApp, pExprBase  ::   EHCParser Expr
pExprPrefix                 ::   EHCParser (Expr -> Expr)

pDecls                      ::   EHCParser Decls
pDecl                       ::   EHCParser Decl

pPatExpr, pPatExprBase      ::   EHCParser PatExpr

pTyExpr, pTyExprBase        ::   EHCParser TyExpr

pInt                        ::   EHCParser Int
pChr                        ::   EHCParser Char

pCon                        ::   EHCParser HsName
pVar                        ::   EHCParser HsName
%%]

%%[4
pTyExprPrefix               ::   EHCParser (TyExpr -> TyExpr)
%%]

%%[4
pTyExprApp                  ::   EHCParser TyExpr
%%]

%%[5
pCaseAlts                   ::   EHCParser CaseAlts
pCaseAlt                    ::   EHCParser CaseAlt

pDataConstr                 ::   EHCParser DataConstr
pDataConstrs                ::   EHCParser DataConstrs

pTyVars                     ::   EHCParser TyVars
pTyVar                      ::   EHCParser TyVar
%%]

%%[7
pDataLabFields              ::   EHCParser DataFields
pDataFields                 ::   EHCParser DataFields
pDataLabField               ::   EHCParser DataField
pDataField                  ::   EHCParser DataField
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parsers, shared/common
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.pApp
pApp            ::   SemApp ep => EHCParser ep -> EHCParser ep
pApp p          =    mkApp <$> pList1 p
%%]

%%[1.pParenProd
pParenProd :: SemApp ep => EHCParser ep -> EHCParser ep
pParenProd pE
  =  pParens pP
  where  pP  =    mkProdApp <$> pSucceed []
             <|>  pE
                  <**>  (    (\es e -> mkProdApp (e:es))
                             <$>  pList1 (pComma *> pE)
                        <|>  pSucceed semParens
                        )
%%]

%%[1.scanWrappers
pChr            =    head <$> pChar
pInt            =    read <$> pInteger
pCon            =    HNm <$> pConid
pVar            =    HNm <$> pVarid
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for the root
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.pAGItf
pAGItf          =    AGItf_AGItf <$> pExpr    
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for Decl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.pDecl
pDecls          =    foldr (:) []    <$>  pBlock pOCurly pSemi pCCurly pDecl
pDecl           =    Decl_Val        <$>  pPatExprBase  <*   pEQUAL   <*> pExpr
                <|>  Decl_TySig      <$>  pVar          <*   pDCOLON  <*> pTyExpr
%%]
%%[5.pDecl
                <|>  Decl_Data False <$   pDATA         <*>  pCon       <*> pTyVars
                                                        <*   pEQUAL     <*> pDataConstrs
%%]
%%[6.pDecl
                <|>  Decl_KiSig      <$>  pCon          <*   pDCOLON    <*> pKiExpr
%%]
%%[8.pDecl
                <|>  (\conv saf imp nm sig -> Decl_FFI conv saf (if null imp then show nm else imp) nm sig)
                     <$   pFOREIGN <* pIMPORT <*> pV pJAZY
                     <*>  (pV (pSAFE <|> pUNSAFE) `opt` "safe")
                     <*>  (pString `opt` "")
                     <*>  pVar
                     <*   pDCOLON <*> pTyExpr
%%]
%%[9.pDecl
                <|>  pDeclClass
                <|>  pDeclInstance
%%]
%%[11
                <|>  Decl_Type       <$   pTYPE         <*>  pCon
                                                        <*   pEQUAL     <*> pTyExpr
%%]
%%[1010.pDecl
                <|>  Decl_DynVal     <$>  pDynVar       <*   pEQUAL     <*> pExpr
                <|>  Decl_DynTySig   <$>  pDynVar       <*   pDCOLON    <*> pTyExpr
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for PatExpr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.patExprAlg
%%]
patExprAlg      =    (PatExpr_Con,PatExpr_App
                     ,PatExpr_AppTop,PatExpr_Parens)

%%[1.pPatExprBase
pPatExprBase    =    pVar <**>  (    flip PatExpr_VarAs <$ pAT <*> pPatExprBase
                                <|>  pSucceed PatExpr_Var
                                )
                <|>  PatExpr_Con     <$>  pCon
                <|>  PatExpr_IConst  <$>  pInt
                <|>  PatExpr_CConst  <$>  pChr
%%]
%%[1.pPatExprBase.prod
                <|>  pParenProd pPatExpr
%%]
%%[7.pPatExprBase.prod -1.pPatExprBase.prod
                <|>  pParenRow True (show hsnORec) (show hsnCRec) "=" Nothing
                        (RecPatExpr_Empty,RecPatExpr_Expr . PatExpr_Var,RecPatExpr_Ext,PatExpr_Rec,PatExpr_Parens)
                        pSel pPatExpr
%%]

%%[1.pPatExpr
pPatExpr        =    pApp pPatExprBase
%%]
%%[4.patExpr
                     <??> (PatExpr_TypeAs <$ pDCOLON <*> pTyExpr)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for KiExpr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[6
pKiExpr, pKiExprBase        ::   EHCParser KiExpr

pKiExprBase     =    KiExpr_Con <$> (pCon <|> pHNm pSTAR)
                <|>  KiExpr_Var <$> pVar
                <|>  pParens pKiExpr
pKiExpr         =    pChainr (mk1Arrow <$ pKeyw hsnArrow) pKiExprBase
%%]
kiExprAlg       =    (KiExpr_Con,KiExpr_App,KiExpr_AppTop,KiExpr_Parens)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for TyExpr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.tyExprAlg
%%]
tyExprAlg       =    (TyExpr_Con,TyExpr_App
                     ,TyExpr_AppTop,TyExpr_Parens)

%%[1.pTyExprBase
pTyExprBase     =    TyExpr_Con       <$>  pCon
%%]
%%[2.pTyExprBase
                <|>  TyExpr_Wild      <$   pTDOT
%%]
%%[3.pTyExprBase
                <|>  TyExpr_Var       <$>  pVar
                <|>  TyExpr_VarWild   <$   pPERCENT <*> pVar
%%]
%%[1.pTyExprBase.prod
                <|>  pParenProd pTyExpr
%%]
%%[7.pTyExprBase.prod -1.pTyExprBase.prod
                <|>  pParenRow False (show hsnORow) (show hsnCRow) "::" Nothing
                        (RowTyExpr_Empty,semVar,RowTyExpr_Ext,TyExpr_Row,id)
                        pVar pTyExpr
                <|>  pParenRow True (show hsnORec) (show hsnCRec) "::" Nothing
                        (RowTyExpr_Empty,semVar,RowTyExpr_Ext
                            ,\r -> mkConApp hsnRec [TyExpr_Row r]
                            ,TyExpr_Parens)
                        pVar pTyExpr
                <|>  pParenRow False (show hsnOSum) (show hsnCSum) "::" Nothing
                        (RowTyExpr_Empty,semVar,RowTyExpr_Ext
                            ,\r -> mkConApp hsnSum [TyExpr_Row r]
                            ,id)
                        pVar pTyExpr
%%]
%%[7.pTyExprBase.prodVar
                where  semVar = const RowTyExpr_Empty
%%]
%%[9.pTyExprBase.prodVar -7.pTyExprBase.prodVar
                where  semVar = RowTyExpr_Var
%%]

%%[1.pTyExpr
pTyExpr         =    pChainr
                       (mk1Arrow <$ pKeyw hsnArrow)
                       pTyExprBase
%%]
%%[4.pTyExpr -1.pTyExpr
pTyExpr         =    pTyExprPrefix <*> pTyExpr
                <|>  pTyExprApp <??> (flip mk1Arrow <$ pKeyw hsnArrow <*> pTyExpr)
%%]

%%[5.pTyExprs
pTyExprs        ::   EHCParser TyExprs
pTyExprs        =    pList pTyExprBase
%%]

%%[4.pTyExprPrefix
pTyExprPrefix   =    TyExpr_Quant
                     <$>  (TyQu_Forall <$ pKey "forall" <|> TyQu_Exists <$ pKey "exists")
                     <*>  pVar <* pKey "."
%%]
%%[9.pTyExprPrefix
                <|>  pTyPrExprPrefix
%%]
%%[11
                <|>  TyExpr_Lam <$ pLAM <*> pVar <* pRARROW
%%]

%%[9.pTyPrExprPrefix
pTyPrExprPrefix ::   EHCParser (TyExpr -> TyExpr)
pTyPrExprPrefix =    mk1Arrow
                     <$>  pPackImpl
                            (    pPr <|> pIm
                            <|>  pSucceed  TyExpr_NoImpls
                            )
                     <*   pKeyw hsnArrow
                <|>  (    mk1Arrow <$> (pPrB <|> pIm)
                     <|>  flip (foldr mk1Arrow)
                          <$> pParens ((:) <$> pPr <*> (pImO <|> (++) <$> pList1 (pComma *> pPr) <*> pImO))
                     )
                     <*   pKeyw hsnPrArrow
                where  pPrB  =   TyExpr_Pred   <$>  pPrExprBase
                       pPr   ::  EHCParser TyExpr
                       pPr   =   TyExpr_Pred   <$>  pPrExpr
                       pIm   ::  EHCParser TyExpr
                       pIm   =   TyExpr_Impls  <$   pKey "..."
                       pImO  =   (:[]) <$ pComma <*> pIm `opt` []
%%]

%%[4.pTyExprApp
pTyExprApp      =    pTyExprBase
%%]
%%[5.pTyExprApp -4.pTyExprApp
pTyExprApp      =    pApp pTyExprBase
%%]

%%[9.pPackImpl
pPackImpl       ::   IsParser p Token => p v -> p v
pPackImpl       =    pPacked pOIMPL pCIMPL
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for Expr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- pExprBase
%%[1.pExprBase
pExprBase       =    Expr_IConst     <$>  pInt
                <|>  Expr_CConst     <$>  pChr
                <|>  Expr_Var        <$>  pVar
                <|>  Expr_Con        <$>  pCon
%%]
%%[1.pExprBaseParenProd
                <|>  pParenProd pExpr
%%]
%%[5.pExprBase
%%[[5
                <|>  Expr_Case
%%][8
                <|>  (\e a -> Expr_Case e a Nothing False)
%%]]
                     <$   pKey "case" <*> pExpr <* pKey "of" <*> pCaseAlts
%%]
%%[7.pExprBase -1.pExprBaseParenProd
                <|>  pParenRow True (show hsnORec) (show hsnCRec) "=" (Just (":=",RecExpr_Upd))
                        (RecExpr_Empty,RecExpr_Expr . Expr_Var,RecExpr_Ext,Expr_Rec,semParens)
                        pVar pExpr
%%]
%%[8.pExprBase
                <|>  Expr_Undefined  <$   pKey "..."
%%]
%%[10.pExprBase
                <|>  Expr_DynVar     <$>  pDynVar
%%]

-- pExpr
%%[1.exprAlg
%%]
exprAlg         =    (Expr_Con,Expr_App
                     ,Expr_AppTop,Expr_Parens)

%%[1.pExpr
pExpr           =    pE <??> (Expr_TypeAs <$ pKey "::" <*> pTyExpr)
                where pE  =    pExprPrefix <*> pE
                          <|>  pExprApp
%%]

%%[1.pExprApp
pExprApp        =    pApp pExprBase
%%]
%%[4.pExprApp -1.pExprApp
pExprApp        =    pE <??> ((\l e -> semAppTop (foldl (flip ($)) e l)) <$> pList1 pA)
%%]
%%[4.pExprAppA
                where  pA = flip semApp <$> pE <|> pImpred
%%]
%%[9.pExprAppA -4.pExprAppA
                where  pA = flip semApp <$> pE <|> pImpred <|> pImpl
%%]
%%[4.pExprAppE
                       pE = pExprBase
%%]
%%[4.pExprAppImpred
                       pImpred = (flip Expr_AppImpred) <$ pKey "~" <*> pE
%%]
%%[7.pExprAppE -4.pExprAppE
                       pE = pExprBase <**> pExprSelSuffix
%%]
%%[9.pExprAppImpl
                       pImpl = pPackImpl ((\a p e -> Expr_AppImpl e p a) <$> pExpr <* pKey "<:" <*> pPrExpr)
%%]

%%[1.pExprPrefix
pExprPrefix     =    Expr_Let      <$ pLET
%%[[8
                     <*> pMaybe False (const True) pBANG
%%]]
                     <*> pDecls    <* pIN
%%]
%%[5.pExprPrefix
                <|>  (\c t e ->  Expr_Case c
                                   [ CaseAlt_Pat (PatExpr_Con (HNm "True")) t
                                   , CaseAlt_Pat (PatExpr_Con (HNm "False")) e
                                   ]
%%[[8
                                   Nothing False
%%]]
                     )
                     <$ pIF <*> pExpr <* pTHEN <*> pExpr <* pELSE
%%]
%%[1.pExprPrefixLam
                <|>  Expr_Lam      <$ pLAM
                     <*> pPatExprBase  <* pRARROW
%%]
%%[7.pExprPrefixLam -1.pExprPrefixLam
                <|>  (\ps -> \e -> foldr Expr_Lam e ps)  <$ pLAM
                     <*> pList1 pPatExprBase                 <* pRARROW
%%]
%%[9.pExprPrefixLam -7.pExprPrefixLam
                <|>  (flip (foldr ($)))
                     <$   pKey "\\"
                     <*>  pList1  (    Expr_Lam <$> pPatExprBase
                                  <|>  pPackImpl (flip Expr_LamImpl <$> pPatExpr <* pKey "<:" <*> pPrExpr)
                                  )
                     <*   pKey "->"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for Case/Data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- data type

%%[5.DataConstr1
pDataConstr     =    DataConstr_Constr <$> pCon <*> pTyExprs
%%]
%%[7.DataConstr1 -5.DataConstr1
pDataConstr     =    DataConstr_Constr
                     <$> pCon <*> (pDataFields <|> pCurly pDataLabFields)
%%]
%%[50.DataConstr
                     <*> pList (DataConstrEq_Eq <$ pComma <*> pTyVar <* pKey "=" <*> pTyExpr)
%%]
%%[5.DataConstr2
pDataConstrs    =    pListSep (pKey "|") pDataConstr
%%]

%%[7.Data
pDataField      =    DataField_Field Nothing <$> pTyExprBase
pDataLabField   =    DataField_Field <$> (Just <$> pList1Sep pComma pVar) <* pKey "::" <*> pTyExpr
pDataFields     =    pList pDataField
pDataLabFields  =    pList1Sep pComma pDataLabField
%%]

-- case expr

%%[5
pCaseAlts       =    foldr (:) []
                     <$> pBlock1 pOCurly pSemi pCCurly pCaseAlt
pCaseAlt        =    CaseAlt_Pat  <$>  pPatExpr <* pKey "->" <*> pExpr

pTyVars         =    pList pTyVar
pTyVar          =    TyVar_Var <$> pVar
%%]

%%[9
pTyVars1        ::   EHCParser TyVars
pTyVars1        =    pList1 pTyVar
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for Records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[7.pParenRow -1.pParenProd
data RowFld a = FldSel HsName a | FldNoSel a | FldUpd HsName a

pParenRow       ::   Bool -> String -> String -> String -> Maybe (String,r -> HsName -> e -> r)
                     -> (r,HsName -> r,r -> Maybe HsName -> e -> r,r -> e,e -> e)
                     -> EHCParser HsName -> EHCParser e -> EHCParser e

pParenRow singleAsIs o c sep mbUpd (semEmpty,semVar,semExt,semRow,semParens) pSel pE
                =    pKey o *> pRowFlds <* pKey c
                where  pFld          =    ((pSel <**> pSep) <|> pSucceed FldNoSel) <*> pE
                       pFlds         =    pListSep pComma pFld
                       pExtFlds      =    mkE <$> (pRowNested <|> semVar <$> pVar) <* pKey "|" <*> pFlds
                       pFldsOrExt    =    mkE semEmpty <$> pFlds <|> pExtFlds
                       pRowNested    =    pKey o *> pFldsOrExt <* pKey c
                       pRowFlds      =    if singleAsIs
                                          then       pFld <**>  (    (\fs f -> mkR (f:fs)) <$ pComma <*> pFlds
                                                                <|>  pSucceed (\le -> case le of {FldNoSel e -> semParens e; _ -> mkR [le]})
                                                                )
                                                <|>  semRow <$> pExtFlds
                                                <|>  pSucceed (mkR [])
                                          else  semRow <$> pFldsOrExt
                       mkR fs        =    semRow (mkE semEmpty fs )
                       mkE ext fs    =    foldl (\r f -> case f of 
                                                            FldSel l e -> semExt r (Just l) e
                                                            FldNoSel e -> semExt r Nothing e
                                                            FldUpd l e -> semUpd r l e
                                                ) ext fs
                       (pSep,semUpd) =    case mbUpd of
                                            Just (sepUpd,sem) -> (FldSel <$ pKey sep <|> FldUpd <$ pKey sepUpd,sem)
                                            Nothing           -> (FldSel <$ pKey sep,\r _ _ -> r)
%%]

%%[7
pExprSelSuffix  ::   EHCParser (Expr -> Expr)
pExprSelSuffix  =    (\lbls e -> foldl Expr_Sel e lbls)
                     <$> pList (pHASH *> pSel)

pSel            ::   EHCParser HsName
pSel            =    pVar <|> pCon <|> HNPos <$> pInt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for Predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
pPrExprClass    ::   EHCParser PrExpr
pPrExprClass    =    PrExpr_Class  <$> pCon <*> pTyExprs
%%]

%%[9
pPrExprPrefix   ::   EHCParser (PrExpr -> PrExpr)
pPrExprPrefix   =    PrExpr_Arrow  <$> pPrExprBase <* pKeyw hsnPrArrow
                <|>  PrExpr_Forall <$  pKey "forall" <*> pVar <* pKey "."
%%]

%%[9
pPrExpr         ::   EHCParser PrExpr
pPrExpr         =    pPrExprPrefix <*> pPrExpr
                <|>  pPrExprBase
%%]

%%[9
pTyPrExpr       ::   EHCParser TyExpr
pTyPrExpr       =    pTyPrExprPrefix <*> pTyPrExpr
                <|>  TyExpr_Pred <$> pPrExprBase
%%]

%%[9
pPrExprBase     ::   EHCParser PrExpr
pPrExprBase     =    pPrExprClass
                <|>  pParens pPrExpr
%%]
%%[10
                <|>  PrExpr_DynVar <$> pDynVar <* pKey "::" <*> pTyExpr
                <|>  pVar <**>  (    (\s v -> PrExpr_Lacks (RowTyExpr_Var v) s)
                                     <$ pKey "\\" <*> pSel
%%]
%%[50
%%]
                                <|>  (flip PrExpr_Equal)
                                     <$ pKey "=" <*> pTyExpr
%%[10
                                )
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for Class & Instance
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
pClassHead      ::   EHCParser TyExpr
pClassHead      =    pTyPrExprPrefix <*> pHd <|> pHd
                where pHd = TyExpr_Pred <$> pPrExprClass

pDeclClass      ::   EHCParser Decl
pDeclClass      =    (\h deps d -> Decl_Class h deps Nothing d)
                     <$   pKey "class"
                     <*>  pClassHead
                     <*>  (pKey "|" *> pListSep pComma (FuncDep_Dep <$> pTyVars1 <* pKey "->" <*> pTyVars1)
                          `opt` []
                          )
                     <*   pKey "where" <*> pDecls

pDeclInstance   ::   EHCParser Decl
pDeclInstance   =    pKey "instance"
                     *>   (    (\n h d -> Decl_Instance n InstNormal h d)
                               <$>  ((\n e -> Just (n,e)) <$> pVar <*> (True <$ pKey "<:" <|> False <$ pKey "::") `opt` Nothing)
                               <*>  pClassHead
                               <*   pKey "where" <*> pDecls
                          <|>  Decl_InstanceIntro Nothing <$> pExpr <* pKey "<:" <*> pPrExprClass
                          )
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser dynamic variable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[10
pDynVar         ::   EHCParser HsName
pDynVar         =    pKeyw hsnDynVar *> pVar
%%]

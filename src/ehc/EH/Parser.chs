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
pCon            =    hsnFromString <$> pConid
pVar            =    hsnFromString <$> pVarid
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
pDecls          =    foldr (:) []         <$>  pBlock pOCurly pSemi pCCurly pDecl
pDecl           =    mkEH Decl_Val        <$>  pPatExprBase  <*   pEQUAL   <*> pExpr
                <|>  mkEH Decl_TySig      <$>  pVar          <*   pDCOLON  <*> pTyExpr
%%]
%%[5.pDecl
                <|>  mkEH Decl_Data False <$   pDATA         <*>  pCon       <*> pTyVars
                                                             <*   pEQUAL     <*> pDataConstrs
%%]
%%[6.pDecl
                <|>  mkEH Decl_KiSig      <$>  pCon          <*   pDCOLON    <*> pKiExpr
%%]
%%[8.pDecl
                <|>  (\conv saf imp nm sig -> mkEH Decl_FFI conv saf (if null imp then show nm else imp) nm sig)
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
                <|>  mkEH Decl_Type       <$   pTYPE         <*>  pCon
                                                             <*   pEQUAL     <*> pTyExpr
%%]
%%[1010.pDecl
                <|>  mkEH Decl_DynVal     <$>  pDynVar       <*   pEQUAL     <*> pExpr
                <|> mkEH  Decl_DynTySig   <$>  pDynVar       <*   pDCOLON    <*> pTyExpr
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for PatExpr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.pPatExprBase
pPatExprBase    =    pVar <**>  (    flip (mkEH PatExpr_VarAs) <$ pAT <*> pPatExprBase
                                <|>  pSucceed (mkEH PatExpr_Var)
                                )
                <|>  mkEH PatExpr_Con     <$>  pCon
                <|>  mkEH PatExpr_IConst  <$>  pInt
                <|>  mkEH PatExpr_CConst  <$>  pChr
%%]
%%[1.pPatExprBase.prod
                <|>  pParenProd pPatExpr
%%]
%%[7.pPatExprBase.prod -1.pPatExprBase.prod
                <|>  pParenRow True (show hsnORec) (show hsnCRec) "=" Nothing
                        (mkEH RecPatExpr_Empty,mkEH RecPatExpr_Expr . mkEH PatExpr_Var,mkEH RecPatExpr_Ext,mkEH PatExpr_Rec,mkEH PatExpr_Parens)
                        pSel pPatExpr
%%]

%%[1.pPatExpr
pPatExpr        =    pApp pPatExprBase
%%]
%%[4.patExpr
                     <??> (mkEH PatExpr_TypeAs <$ pDCOLON <*> pTyExpr)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for KiExpr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[6
pKiExpr, pKiExprBase        ::   EHCParser KiExpr

pKiExprBase     =    mkEH KiExpr_Con <$> (pCon <|> pHNm pSTAR)
                <|>  mkEH KiExpr_Var <$> pVar
                <|>  pParens pKiExpr
pKiExpr         =    pChainr (mk1Arrow <$ pKeyw hsnArrow) pKiExprBase
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for TyExpr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.pTyExprBase
pTyExprBase     =    mkEH TyExpr_Con       <$>  pCon
%%]
%%[2.pTyExprBase
                <|>  mkEH TyExpr_Wild      <$   pTDOT
%%]
%%[3.pTyExprBase
                <|>  mkEH TyExpr_Var       <$>  pVar
                <|>  mkEH TyExpr_VarWild   <$   pPERCENT <*> pVar
%%]
%%[1.pTyExprBase.prod
                <|>  pParenProd pTyExpr
%%]
%%[7.pTyExprBase.prod -1.pTyExprBase.prod
                <|>  pParenRow False (show hsnORow) (show hsnCRow) "::" Nothing
                        (mkEH RowTyExpr_Empty,semVar,mkEH RowTyExpr_Ext,mkEH TyExpr_Row,id)
                        pVar pTyExpr
                <|>  pParenRow True (show hsnORec) (show hsnCRec) "::" Nothing
                        (mkEH RowTyExpr_Empty,semVar,mkEH RowTyExpr_Ext
                            ,\r -> mkConApp hsnRec [mkEH TyExpr_Row r]
                            ,mkEH TyExpr_Parens)
                        pVar pTyExpr
                <|>  pParenRow False (show hsnOSum) (show hsnCSum) "::" Nothing
                        (mkEH RowTyExpr_Empty,semVar,mkEH RowTyExpr_Ext
                            ,\r -> mkConApp hsnSum [mkEH TyExpr_Row r]
                            ,id)
                        pVar pTyExpr
%%]
%%[7.pTyExprBase.prodVar
                where  semVar = const (mkEH RowTyExpr_Empty)
%%]
%%[9.pTyExprBase.prodVar -7.pTyExprBase.prodVar
                where  semVar = (mkEH RowTyExpr_Var)
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
pTyExprPrefix   =    mkEH TyExpr_Quant
                     <$>  (TyQu_Forall <$ pKey "forall" <|> TyQu_Exists <$ pKey "exists")
                     <*>  pVar <* pKey "."
%%]
%%[9.pTyExprPrefix
                <|>  pTyPrExprPrefix
%%]
%%[11
                <|>  mkEH TyExpr_Lam <$ pLAM <*> pVar <* pRARROW
%%]

%%[9.pTyPrExprPrefix
pTyPrExprPrefix ::   EHCParser (TyExpr -> TyExpr)
pTyPrExprPrefix =    mk1Arrow
                     <$>  pPackImpl
                            (    pPr <|> pIm
                            <|>  pSucceed  (mkEH TyExpr_NoImpls)
                            )
                     <*   pKeyw hsnArrow
                <|>  (    mk1Arrow <$> (pPrB <|> pIm)
                     <|>  flip (foldr mk1Arrow)
                          <$> pParens ((:) <$> pPr <*> (pImO <|> (++) <$> pList1 (pComma *> pPr) <*> pImO))
                     )
                     <*   pKeyw hsnPrArrow
                where  pPrB  =   mkEH TyExpr_Pred   <$>  pPrExprBase
                       pPr   ::  EHCParser TyExpr
                       pPr   =   mkEH TyExpr_Pred   <$>  pPrExpr
                       pIm   ::  EHCParser TyExpr
                       pIm   =   mkEH TyExpr_Impls  <$   pKey "..."
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
pExprBase       =    mkEH Expr_IConst     <$>  pInt
                <|>  mkEH Expr_CConst     <$>  pChr
                <|>  mkEH Expr_Var        <$>  pVar
                <|>  mkEH Expr_Con        <$>  pCon
%%]
%%[1.pExprBaseParenProd
                <|>  pParenProd pExpr
%%]
%%[5.pExprBase
%%[[5
                <|>  mkEH Expr_Case
%%][8
                <|>  (\e a -> mkEH Expr_Case e a Nothing False)
%%]]
                     <$   pKey "case" <*> pExpr <* pKey "of" <*> pCaseAlts
%%]
%%[7.pExprBase -1.pExprBaseParenProd
                <|>  pParenRow True (show hsnORec) (show hsnCRec) "=" (Just (":=",mkEH RecExpr_Upd))
                        (mkEH RecExpr_Empty,mkEH RecExpr_Expr . mkEH Expr_Var,mkEH RecExpr_Ext,mkEH Expr_Rec,semParens)
                        pVar pExpr
%%]
%%[8.pExprBase
                <|>  mkEH Expr_Undefined  <$   pKey "..."
%%]
%%[10.pExprBase
                <|>  mkEH Expr_DynVar     <$>  pDynVar
%%]

-- pExpr
%%[1.pExpr
pExpr           =    pE <??> (mkEH Expr_TypeAs <$ pKey "::" <*> pTyExpr)
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
%%[12.pExprAppA -4.pExprAppA
                where  pA = flip semApp <$> pE <|> pImpred <|> pImpl
%%]
%%[4.pExprAppE
                       pE = pExprBase
%%]
%%[4.pExprAppImpred
                       pImpred = (flip (mkEH Expr_AppImpred)) <$ pKey "~" <*> pE
%%]
%%[7.pExprAppE -4.pExprAppE
                       pE = pExprBase <**> pExprSelSuffix
%%]
%%[12.pExprAppImpl
                       pImpl = pPackImpl ((\a p e -> mkEH Expr_AppImpl e p a) <$> pExpr <* pKey "<:" <*> pPrExpr)
%%]

%%[1.pExprPrefix
pExprPrefix     =    mkEH Expr_Let <$ pLET
%%[[8
                     <*> pMaybe False (const True) pBANG
%%]]
                     <*> pDecls    <* pIN
%%]
%%[5.pExprPrefix
                <|>  (\c t e ->  mkEH Expr_Case c
                                   [ mkEH CaseAlt_Pat (mkEH PatExpr_Con hsnTrue) t
                                   , mkEH CaseAlt_Pat (mkEH PatExpr_Con hsnFalse) e
                                   ]
%%[[8
                                   Nothing False
%%]]
                     )
                     <$ pIF <*> pExpr <* pTHEN <*> pExpr <* pELSE
%%]
%%[1.pExprPrefixLam
                <|>  mkEH Expr_Lam      <$ pLAM
                     <*> pPatExprBase   <* pRARROW
%%]
%%[7.pExprPrefixLam -1.pExprPrefixLam
                <|>  (\ps -> \e -> foldr (mkEH Expr_Lam) e ps)  <$ pLAM
                     <*> pList1 pPatExprBase                    <* pRARROW
%%]
%%[9.pExprPrefixLam -7.pExprPrefixLam
                <|>  (flip (foldr ($)))
                     <$   pKey "\\"
                     <*>  pList1  (    mkEH Expr_Lam <$> pPatExprBase
%%[[12
                                  <|>  pPackImpl (flip (mkEH Expr_LamImpl) <$> pPatExpr <* pKey "<:" <*> pPrExpr)
%%]]
                                  )
                     <*   pKey "->"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for Case/Data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- data type

%%[5.DataConstr1
pDataConstr     =    mkEH DataConstr_Constr <$> pCon <*> pTyExprs
%%]
%%[7.DataConstr1 -5.DataConstr1
%%[[7
pDataConstr     =    mkEH DataConstr_Constr
%%][95
pDataConstr     =    (\c f -> mkEH DataConstr_Constr c Nothing f)
%%]]
                     <$> pCon <*> (pDataFields <|> pCurly pDataLabFields)
%%]
%%[50.DataConstr
                     <*> pList (mkEH DataConstrEq_Eq <$ pComma <*> pTyVar <* pKey "=" <*> pTyExpr)
%%]
%%[5.DataConstr2
pDataConstrs    =    pListSep (pKey "|") pDataConstr
%%]

%%[7.Data
pDataField      =    mkEH DataField_Field Nothing <$> pTyExprBase
pDataLabField   =    mkEH DataField_Field <$> (Just <$> pList1Sep pComma pVar) <* pKey "::" <*> pTyExpr
pDataFields     =    pList pDataField
pDataLabFields  =    pList1Sep pComma pDataLabField
%%]

-- case expr

%%[5
pCaseAlts       =    foldr (:) []
                     <$> pBlock1 pOCurly pSemi pCCurly pCaseAlt
pCaseAlt        =    mkEH CaseAlt_Pat  <$>  pPatExpr <* pKey "->" <*> pExpr

pTyVars         =    pList pTyVar
pTyVar          =    mkEH TyVar_Var <$> pVar
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
pExprSelSuffix  =    (\lbls e -> foldl (mkEH Expr_Sel) e lbls)
                     <$> pList (pHASH *> pSel)

pSel            ::   EHCParser HsName
pSel            =    pVar <|> pCon <|> HNPos <$> pInt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for Predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
pPrExprClass    ::   EHCParser PrExpr
pPrExprClass    =    mkEH PrExpr_Class  <$> pCon <*> pTyExprs
%%]

%%[9
pPrExprPrefix   ::   EHCParser (PrExpr -> PrExpr)
pPrExprPrefix   =    mkEH PrExpr_Arrow  <$> pPrExprBase <* pKeyw hsnPrArrow
                <|>  mkEH PrExpr_Forall <$  pKey "forall" <*> pVar <* pKey "."
%%]

%%[9
pPrExpr         ::   EHCParser PrExpr
pPrExpr         =    pPrExprPrefix <*> pPrExpr
                <|>  pPrExprBase
%%]

%%[9
pTyPrExpr       ::   EHCParser TyExpr
pTyPrExpr       =    pTyPrExprPrefix <*> pTyPrExpr
                <|>  mkEH TyExpr_Pred <$> pPrExprBase
%%]

%%[9
pPrExprBase     ::   EHCParser PrExpr
pPrExprBase     =    pPrExprClass
                <|>  pParens pPrExpr
%%]
%%[10
                <|>  mkEH PrExpr_DynVar <$> pDynVar <* pKey "::" <*> pTyExpr
                <|>  pVar <**>  (    (\s v -> mkEH PrExpr_Lacks (mkEH RowTyExpr_Var v) s)
                                     <$ pKey "\\" <*> pSel
%%]
%%[50
%%]
                                <|>  (flip (mkEH PrExpr_Equal))
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
                where pHd = mkEH TyExpr_Pred <$> pPrExprClass

pDeclClass      ::   EHCParser Decl
%%[[9
pDeclClass      =    (\h d -> mkEH Decl_Class h Nothing d)
%%][15
pDeclClass      =    (\h deps d -> mkEH Decl_Class h deps Nothing d)
%%]]
                     <$   pKey "class"
                     <*>  pClassHead
%%[[15
                     <*>  (pKey "|" *> pListSep pComma (mkEH FuncDep_Dep <$> pTyVars1 <* pKey "->" <*> pTyVars1)
                          `opt` []
                          )
%%]]
                     <*   pKey "where" <*> pDecls

pDeclInstance   ::   EHCParser Decl
pDeclInstance   =    pKey "instance"
                     *>   (    (\n h d -> mkEH Decl_Instance n InstNormal h d)
                               <$>  ((\n e -> Just (n,e)) <$> pVar <*> (True <$ pKey "<:" <|> False <$ pKey "::") `opt` Nothing)
                               <*>  pClassHead
                               <*   pKey "where" <*> pDecls
                          <|>  mkEH Decl_InstanceIntro Nothing <$> pExpr <* pKey "<:" <*> pPrExprClass
                          )
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser dynamic variable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[10
pDynVar         ::   EHCParser HsName
pDynVar         =    pKeyw hsnDynVar *> pVar
%%]

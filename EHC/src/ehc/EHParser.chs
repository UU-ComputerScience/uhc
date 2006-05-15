% $Id: EHParser.chs 269 2005-08-14 12:49:00Z cddouma $

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module EHParser import(IO, UU.Parsing, UU.Parsing.Offside, UU.Scanner.GenToken, EHCommon, EHScannerCommon, EHMainAG)
%%]

%%[1 export(pAGItf)
%%]

%%[4 import (EHTy)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Semantics classes for parsers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.SemParser
class SemApp e => SemParser e p t ds where
  semVar        ::  HsName -> e
  semLam        ::  p -> e -> e
  semLet        ::  ds -> e -> e
  semTypeAs     ::  t -> e -> e
%%]

%%[1
instance SemParser T_Expr T_PatExpr T_TyExpr T_Decls where
  semVar        =   sem_Expr_Var
  semLam        =   sem_Expr_Lam
  semLet        =   sem_Expr_Let
  semTypeAs     =   sem_Expr_TypeAs
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser semantics class instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
instance SemApp T_Expr where
  semApp       = sem_Expr_App
  semAppTop    = sem_Expr_AppTop
  semCon       = sem_Expr_Con
  semParens    = sem_Expr_Parens
%%]

%%[1
instance SemApp T_PatExpr where
  semApp       = sem_PatExpr_App
  semAppTop    = sem_PatExpr_AppTop
  semCon       = sem_PatExpr_Con
  semParens    = sem_PatExpr_Parens
%%]

%%[1
instance SemApp T_TyExpr where
  semApp       = sem_TyExpr_App
  semAppTop    = sem_TyExpr_AppTop
  semCon       = sem_TyExpr_Con
  semParens    = sem_TyExpr_Parens
%%]

%%[6
instance SemApp T_KiExpr where
  semApp       = sem_KiExpr_App
  semAppTop    = sem_KiExpr_AppTop
  semCon       = sem_KiExpr_Con
  semParens    = sem_KiExpr_Parens
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser signatures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

type ExprParser       ep    =    (IsParser (OffsideParser i o Token p) Token,InputState i Token p, OutputState o, Position p)
                                    => OffsideParser i o Token p ep

%%[1.parserSigs
type EHParser         ep    =    (IsParser (OffsideParser i o Token p) Token,InputState i Token p, OutputState o, Position p)
                                    => OffsideParser i o Token p ep

pAGItf                      ::   EHParser T_AGItf

pExpr, pExprApp, pExprBase  ::   EHParser T_Expr
pExprPrefix                 ::   EHParser (T_Expr -> T_Expr)

pDecls                      ::   EHParser T_Decls
pDecl                       ::   EHParser T_Decl

pPatExpr, pPatExprBase      ::   EHParser T_PatExpr

pTyExpr, pTyExprBase        ::   EHParser T_TyExpr

pInt                        ::   EHParser Int
pChr                        ::   EHParser Char

pCon                        ::   EHParser HsName
pVar                        ::   EHParser HsName
%%]

%%[4
pTyExprPrefix               ::   EHParser (T_TyExpr -> T_TyExpr)
%%]

%%[4
pTyExprApp                  ::   EHParser T_TyExpr
%%]

%%[5
pCaseAlts                   ::   EHParser T_CaseAlts
pCaseAlt                    ::   EHParser T_CaseAlt

pDataConstr                 ::   EHParser T_DataConstr
pDataConstrs                ::   EHParser T_DataConstrs

pTyVars                     ::   EHParser T_TyVars
pTyVar                      ::   EHParser T_TyVar
%%]

%%[7
pDataLabFields              ::   EHParser T_DataFields
pDataFields                 ::   EHParser T_DataFields
pDataLabField               ::   EHParser T_DataField
pDataField                  ::   EHParser T_DataField
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parsers, shared/common
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.pApp
pApp            ::   SemApp ep => EHParser ep -> EHParser ep
pApp p          =    mkApp <$> pList1 p
%%]

%%[1.pParenProd
pParenProd :: SemApp ep => EHParser ep -> EHParser ep
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
pAGItf          =    sem_AGItf_AGItf <$> pExpr    
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for Decl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.pDecl
pDecls          =    foldr sem_Decls_Cons sem_Decls_Nil
                                         <$>  pBlock pOCurly pSemi pCCurly pDecl
pDecl           =    sem_Decl_Val        <$>  pPatExprBase  <*   pEQUAL   <*> pExpr
                <|>  sem_Decl_TySig      <$>  pVar          <*   pDCOLON  <*> pTyExpr
%%]
%%[5.pDecl
                <|>  sem_Decl_Data       <$   pDATA         <*>  pCon       <*> pTyVars
                                                            <*   pEQUAL     <*> pDataConstrs
%%]
%%[6.pDecl
                <|>  sem_Decl_KiSig      <$>  pCon          <*   pDCOLON    <*> pKiExpr
%%]
%%[8.pDecl
                <|>  (\conv saf imp nm sig -> sem_Decl_FFI conv saf (if null imp then show nm else imp) nm sig)
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
%%[10.pDecl
                <|>  sem_Decl_DynVal     <$>  pDynVar       <*   pEQUAL     <*> pExpr
                <|>  sem_Decl_DynTySig   <$>  pDynVar       <*   pDCOLON    <*> pTyExpr
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for PatExpr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.patExprAlg
%%]
patExprAlg      =    (sem_PatExpr_Con,sem_PatExpr_App
                     ,sem_PatExpr_AppTop,sem_PatExpr_Parens)

%%[1.pPatExprBase
pPatExprBase    =    pVar <**>  (    flip sem_PatExpr_VarAs <$ pAT <*> pPatExprBase
                                <|>  pSucceed sem_PatExpr_Var
                                )
                <|>  sem_PatExpr_Con     <$>  pCon
                <|>  sem_PatExpr_IConst  <$>  pInt
                <|>  sem_PatExpr_CConst  <$>  pChr
%%]
%%[1.pPatExprBase.prod
                <|>  pParenProd pPatExpr
%%]
%%[7.pPatExprBase.prod -1.pPatExprBase.prod
                <|>  pParenRow True (show hsnORec) (show hsnCRec) "=" Nothing
                        (sem_RecPatExpr_Empty,sem_RecPatExpr_Expr . sem_PatExpr_Var,sem_RecPatExpr_Ext,sem_PatExpr_Rec,sem_PatExpr_Parens)
                        pSel pPatExpr
%%]

%%[1.pPatExpr
pPatExpr        =    pApp pPatExprBase
%%]
%%[4.patExpr
                     <??> (sem_PatExpr_TypeAs <$ pDCOLON <*> pTyExpr)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for KiExpr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[6
pKiExpr, pKiExprBase        ::   EHParser T_KiExpr

pKiExprBase     =    sem_KiExpr_Con <$> (pCon <|> pHNm pSTAR)
                <|>  sem_KiExpr_Var <$> pVar
                <|>  pParens pKiExpr
pKiExpr         =    pChainr (mk1Arrow <$ pKeyw hsnArrow) pKiExprBase
%%]
kiExprAlg       =    (sem_KiExpr_Con,sem_KiExpr_App,sem_KiExpr_AppTop,sem_KiExpr_Parens)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for TyExpr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.tyExprAlg
%%]
tyExprAlg       =    (sem_TyExpr_Con,sem_TyExpr_App
                     ,sem_TyExpr_AppTop,sem_TyExpr_Parens)

%%[1.pTyExprBase
pTyExprBase     =    sem_TyExpr_Con       <$>  pCon
%%]
%%[2.pTyExprBase
                <|>  sem_TyExpr_Wild      <$   pTDOT
%%]
%%[3.pTyExprBase
                <|>  sem_TyExpr_Var       <$>  pVar
                <|>  sem_TyExpr_VarWild   <$   pPERCENT <*> pVar
%%]
%%[1.pTyExprBase.prod
                <|>  pParenProd pTyExpr
%%]
%%[7.pTyExprBase.prod -1.pTyExprBase.prod
                <|>  pParenRow False (show hsnORow) (show hsnCRow) "::" Nothing
                        (sem_RowTyExpr_Empty,semVar,sem_RowTyExpr_Ext,sem_TyExpr_Row,id)
                        pVar pTyExpr
                <|>  pParenRow True (show hsnORec) (show hsnCRec) "::" Nothing
                        (sem_RowTyExpr_Empty,semVar,sem_RowTyExpr_Ext
                            ,\r -> mkConApp hsnRec [sem_TyExpr_Row r]
                            ,sem_TyExpr_Parens)
                        pVar pTyExpr
                <|>  pParenRow False (show hsnOSum) (show hsnCSum) "::" Nothing
                        (sem_RowTyExpr_Empty,semVar,sem_RowTyExpr_Ext
                            ,\r -> mkConApp hsnSum [sem_TyExpr_Row r]
                            ,id)
                        pVar pTyExpr
%%]
%%[7.pTyExprBase.prodVar
                where  semVar = const sem_RowTyExpr_Empty
%%]
%%[9.pTyExprBase.prodVar -7.pTyExprBase.prodVar
                where  semVar = sem_RowTyExpr_Var
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
pTyExprs        ::   EHParser T_TyExprs
pTyExprs        =    pFoldr (sem_TyExprs_Cons,sem_TyExprs_Nil) pTyExprBase
%%]

%%[4.pTyExprPrefix
pTyExprPrefix   =    sem_TyExpr_Quant
                     <$>  (TyQu_Forall <$ pKey "forall" <|> TyQu_Exists <$ pKey "exists")
                     <*>  pVar <* pKey "."
%%]
%%[9.pTyExprPrefix
                <|>  pTyPrExprPrefix
%%]

%%[9.pTyPrExprPrefix
pTyPrExprPrefix ::   EHParser (T_TyExpr -> T_TyExpr)
pTyPrExprPrefix =    mk1Arrow
                     <$>  pPackImpl
                            (    pPr <|> pIm
                            <|>  pSucceed  sem_TyExpr_NoImpls
                            )
                     <*   pKeyw hsnArrow
                <|>  (    mk1Arrow <$> (pPrB <|> pIm)
                     <|>  flip (foldr mk1Arrow)
                          <$> pParens ((:) <$> pPr <*> (pImO <|> (++) <$> pList1 (pComma *> pPr) <*> pImO))
                     )
                     <*   pKeyw hsnPrArrow
                where  pPrB  =   sem_TyExpr_Pred   <$>  pPrExprBase
                       pPr   ::  EHParser T_TyExpr
                       pPr   =   sem_TyExpr_Pred   <$>  pPrExpr
                       pIm   ::  EHParser T_TyExpr
                       pIm   =   sem_TyExpr_Impls  <$   pKey "..."
                       pImO  =   (:[]) <$ pComma <*> pIm `opt` []
%%]

%%[4.pTyExprApp
pTyExprApp      =    pTyExprBase
%%]
%%[5.pTyExprApp -4.pTyExprApp
pTyExprApp      =    pApp pTyExprBase
%%]

%%[9.pPackImpl
pPackImpl       ::   EHParser p -> EHParser p
pPackImpl       =    pPacked (pKeyw hsnOImpl) (pKeyw hsnCImpl)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for Expr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- pExprBase
%%[1.pExprBase
pExprBase       =    sem_Expr_IConst     <$>  pInt
                <|>  sem_Expr_CConst     <$>  pChr
                <|>  sem_Expr_Var        <$>  pVar
                <|>  sem_Expr_Con        <$>  pCon
%%]
%%[1.pExprBaseParenProd
                <|>  pParenProd pExpr
%%]
%%[5.pExprBase
                <|>  sem_Expr_Case       <$   pKey "case" <*> pExpr <* pKey "of" <*> pCaseAlts
%%]
%%[7.pExprBase -1.pExprBaseParenProd
                <|>  pParenRow True (show hsnORec) (show hsnCRec) "=" (Just (":=",sem_RecExpr_Upd))
                        (sem_RecExpr_Empty,sem_RecExpr_Expr . sem_Expr_Var,sem_RecExpr_Ext,sem_Expr_Rec,semParens)
                        pVar pExpr
%%]
%%[8.pExprBase
                <|>  sem_Expr_Undefined  <$   pKey "..."
%%]
%%[10.pExprBase
                <|>  sem_Expr_DynVar     <$>  pDynVar
%%]

-- pExpr
%%[1.exprAlg
%%]
exprAlg         =    (sem_Expr_Con,sem_Expr_App
                     ,sem_Expr_AppTop,sem_Expr_Parens)

%%[1.pExpr
pExpr           =    pE <??> (sem_Expr_TypeAs <$ pKey "::" <*> pTyExpr)
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
                       pImpred = (flip sem_Expr_AppImpred) <$ pKey "~" <*> pE
%%]
%%[7.pExprAppE -4.pExprAppE
                       pE = pExprBase <**> pExprSelSuffix
%%]
%%[9.pExprAppImpl
                       pImpl = pPackImpl ((\a p e -> sem_Expr_AppImpl e p a) <$> pExpr <* pKey "<:" <*> pPrExpr)
%%]

%%[1.pExprPrefix
pExprPrefix     =    sem_Expr_Let  <$ pLET
                     <*> pDecls    <* pIN
%%]
%%[5.pExprPrefix
                <|>  (\c t e ->  sem_Expr_Case c
                                   (sem_CaseAlts_Cons (sem_CaseAlt_Pat (sem_PatExpr_Con (HNm "True")) t)
                                      (sem_CaseAlts_Cons (sem_CaseAlt_Pat (sem_PatExpr_Con (HNm "False")) e)
                                         sem_CaseAlts_Nil
                     )             )  )
                     <$ pIF <*> pExpr <* pTHEN <*> pExpr <* pELSE
%%]
%%[1.pExprPrefixLam
                <|>  sem_Expr_Lam      <$ pLAM
                     <*> pPatExprBase  <* pRARROW
%%]
%%[7.pExprPrefixLam -1.pExprPrefixLam
                <|>  (\ps -> \e -> foldr sem_Expr_Lam e ps)  <$ pLAM
                     <*> pList1 pPatExprBase                 <* pRARROW
%%]
%%[9.pExprPrefixLam -7.pExprPrefixLam
                <|>  (flip (foldr ($)))
                     <$   pKey "\\"
                     <*>  pList1  (    sem_Expr_Lam <$> pPatExprBase
                                  <|>  pPackImpl (flip sem_Expr_LamImpl <$> pPatExpr <* pKey "<:" <*> pPrExpr)
                                  )
                     <*   pKey "->"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for Case/Data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- data type

%%[5.DataConstr1
pDataConstr     =    sem_DataConstr_Constr <$> pCon <*> pTyExprs
%%]
%%[7.DataConstr1 -5.DataConstr1
pDataConstr     =    sem_DataConstr_Constr
                     <$> pCon <*> (pDataFields <|> pCurly pDataLabFields)
%%]
%%[11.DataConstr
                     <*> pFoldr  (sem_DataConstrEqs_Cons,sem_DataConstrEqs_Nil)
                                 (sem_DataConstrEq_Eq <$ pComma <*> pTyVar <* pKey "=" <*> pTyExpr)
%%]
%%[5.DataConstr2
pDataConstrs    =    pFoldrSep (sem_DataConstrs_Cons,sem_DataConstrs_Nil) (pKey "|") pDataConstr
%%]

%%[7.Data
pDataField      =    sem_DataField_Field Nothing <$> pTyExprBase
pDataLabField   =    sem_DataField_Field <$> (Just <$> pList1Sep pComma pVar) <* pKey "::" <*> pTyExpr
pDataFields     =    pFoldr (sem_DataFields_Cons,sem_DataFields_Nil) pDataField
pDataLabFields  =    pFoldr1Sep (sem_DataFields_Cons,sem_DataFields_Nil) pComma pDataLabField
%%]

-- case expr

%%[5
pCaseAlts       =    foldr sem_CaseAlts_Cons sem_CaseAlts_Nil
                     <$> pBlock1 pOCurly pSemi pCCurly pCaseAlt
pCaseAlt        =    sem_CaseAlt_Pat  <$>  pPatExpr <* pKey "->" <*> pExpr

pTyVars         =    pFoldr (sem_TyVars_Cons,sem_TyVars_Nil) pTyVar
pTyVar          =    sem_TyVar_Var <$> pVar
%%]

%%[9
pTyVars1        ::   EHParser T_TyVars
pTyVars1        =    pFoldr1 (sem_TyVars_Cons,sem_TyVars_Nil) pTyVar
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for Records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[7.pParenRow -1.pParenProd
data RowFld a = FldSel HsName a | FldNoSel a | FldUpd HsName a

pParenRow       ::   Bool -> String -> String -> String -> Maybe (String,r -> HsName -> e -> r)
                     -> (r,HsName -> r,r -> Maybe HsName -> e -> r,r -> e,e -> e)
                     -> EHParser HsName -> EHParser e -> EHParser e

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
pExprSelSuffix  ::   EHParser (T_Expr -> T_Expr)
pExprSelSuffix  =    (\lbls e -> foldl sem_Expr_Sel e lbls)
                     <$> pList (pKey "." *> pSel)

pSel            ::   EHParser HsName
pSel            =    pVar <|> pCon <|> HNPos <$> pInt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for Predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
pPrExprClass    ::   EHParser T_PrExpr
pPrExprClass    =    sem_PrExpr_Class  <$> pCon <*> pTyExprs
%%]

%%[9
pPrExprPrefix   ::   EHParser (T_PrExpr -> T_PrExpr)
pPrExprPrefix   =    sem_PrExpr_Arrow  <$> pPrExprBase <* pKeyw hsnPrArrow
                <|>  sem_PrExpr_Forall <$  pKey "forall" <*> pVar <* pKey "."
%%]

%%[9
pPrExpr         ::   EHParser T_PrExpr
pPrExpr         =    pPrExprPrefix <*> pPrExpr
                <|>  pPrExprBase
%%]

%%[9
pTyPrExpr       ::   EHParser T_TyExpr
pTyPrExpr       =    pTyPrExprPrefix <*> pTyPrExpr
                <|>  sem_TyExpr_Pred <$> pPrExprBase
%%]

%%[9
pPrExprBase     ::   EHParser T_PrExpr
pPrExprBase     =    pPrExprClass
                <|>  pParens pPrExpr
%%]
%%[10
                <|>  sem_PrExpr_DynVar <$> pDynVar <* pKey "::" <*> pTyExpr
                <|>  pVar <**>  (    (\s v -> sem_PrExpr_Lacks (sem_RowTyExpr_Var v) s)
                                     <$ pKey "\\" <*> pSel
%%]
%%[11
%%]
                                <|>  (flip sem_PrExpr_Equal)
                                     <$ pKey "=" <*> pTyExpr
%%[10
                                )
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for Class & Instance
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
pClassHead      ::   EHParser T_TyExpr
pClassHead      =    pTyPrExprPrefix <*> pHd <|> pHd
                where pHd = sem_TyExpr_Pred <$> pPrExprClass

pDeclClass      ::   EHParser T_Decl
pDeclClass      =    sem_Decl_Class
                     <$   pKey "class"
                     <*>  pClassHead
                     <*>  (pKey "|" *> pFoldrSep  (sem_FuncDeps_Cons,sem_FuncDeps_Nil) pComma
                                                  (sem_FuncDep_Dep <$> pTyVars1 <* pKey "->" <*> pTyVars1)
                          `opt` sem_FuncDeps_Nil
                          )
                     <*   pKey "where" <*> pDecls

pDeclInstance   ::   EHParser T_Decl
pDeclInstance   =    pKey "instance"
                     *>   (    sem_Decl_Instance
                               <$>  ((\n e -> Just (n,e)) <$> pVar <*> (True <$ pKey "<:" <|> False <$ pKey "::") `opt` Nothing)
                               <*>  pClassHead
                               <*   pKey "where" <*> pDecls
                          <|>  sem_Decl_InstanceIntro <$> pExpr <* pKey "<:" <*> pPrExprClass
                          )
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser dynamic variable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[10
pDynVar         ::   EHParser HsName
pDynVar         =    pKeyw hsnDynVar *> pVar
%%]

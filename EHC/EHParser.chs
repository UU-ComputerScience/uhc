% $Id$

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module EHParser import(IO, UU.Parsing, UU.Parsing.Offside, UU.Scanner.Position, UU.Scanner.GenToken, EHCommon, EHMainAG)
%%]

%%[1.Scanner import(UU.Scanner)
%%]

%%[1 export(pAGItf, offsideScanHandle)
%%]

%%[4 import (EHTy)
%%]

%%[7.Scanner -1.Scanner import(EHScanner)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scanner
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.ScanOpts
data ScanOpts
  =  ScanOpts
        {   scoKeywordsTxt      ::  [String]
        ,   scoKeywordsOps      ::  [String]
        ,   scoSpecChars        ::  String
        ,   scoOpChars          ::  String
        }
%%]

%%[1.scanOpts
scanOpts :: ScanOpts
scanOpts
%%]
%%[1.defaultScanOpts
  =  ScanOpts
%%]
%%[7 -1.defaultScanOpts
  =  defaultScanOpts
%%]
%%[1
        {   scoKeywordsTxt      =   
                [ "in"
%%]
%%[4
                , "forall", "exists"
%%]
%%[5
                , "data", "case", "if", "then", "else"
%%]
%%[8
                , "foreign", "import", "jazy"
%%]
%%[9
                , "class", "instance"
%%]
%%[1
                ] ++ offsideTrigs
        ,   scoKeywordsOps      =
                [ "=", "\\", show hsnArrow, "::", "@"
%%]
%%[2
                , "..."
%%]
%%[3
                , "%"
%%]
%%[4
                , "."
%%]
%%[5
                , "|"
%%]
%%[6
                , "*"
%%]
%%[7
                , ":="
%%]
%%[9
                , show hsnPrArrow, "<:"
%%]
%%[1
                ]
        ,   scoSpecChars        =
                "();,[]{}"
        ,   scoOpChars          =
                "!#$%&*+/<=>?@\\^|-:.~"
%%]
%%[7 -1.ScanOpts
        ,   scoSpecPairs        =
                [  show hsnORow, show hsnCRow
                ,  show hsnOSum, show hsnCSum
%%]
%%[9
                ,  show hsnOImpl, show hsnCImpl
%%]
%%[7
                ]
%%]
%%[1
        }
%%]

%%[1.offsideTrigs
offsideTrigs  =  [ "let" ]
%%]

%%[5.offsideTrigs -1.offsideTrigs
offsideTrigs  =  [ "let", "of" ]
%%]

%%[9.offsideTrigs -5.offsideTrigs
offsideTrigs  =  [ "let", "of", "where" ]
%%]

%%[1.scanHandle
scanHandle :: ScanOpts -> FilePath -> Handle -> IO [Token]
scanHandle opts fn fh
  = do  {  txt <- hGetContents fh
        ;  return (scan (scoKeywordsTxt opts) (scoKeywordsOps opts) (scoSpecChars opts) (scoOpChars opts) (initPos fn) txt) 
        }
%%]

%%[7 -1.scanHandle
%%]

%%[1.offsideScanHandle
offsideScanHandle fn fh
  = do  {  tokens <- scanHandle scanOpts fn fh
        ;  return (scanOffside moduleT oBrace cBrace triggers tokens)
        }
  where   moduleT   = reserved "let" noPos
          oBrace    = reserved "{" noPos
          cBrace    = reserved "}" noPos
          triggers  = [ reserved x noPos | x <- offsideTrigs ]
%%]

%%[1
instance Position (Maybe Token) where
  line    =  maybe (-1)  (line.position) 
  column  =  maybe (-1)  (column.position)
  file    =  maybe ""    (file.position)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser signatures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
pKeyw                       ::   Show k => k -> EHParser String

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
pApp alg p      =    mkApp alg <$> pList1 p
%%]

%%[1.pParenProd
pParenProd alg@(_,_,_,par) pE
                =    pParens pP
                     where
                       pP  =    mkProdApp alg <$> pSucceed []
                           <|>  pE
                                <**>  (    (\es e -> mkProdApp alg (e:es))
                                           <$>  pList1 (pComma *> pE)
                                      <|>  pSucceed par
                                      )
%%]

%%[1.scanWrappers
pChr            =    head <$> pChar
pInt            =    read <$> pInteger
pKeyw k         =    pKey (show k)
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
pDecl           =    sem_Decl_Val     <$>  pPatExprBase  <*   pKey "="   <*> pExpr
                <|>  sem_Decl_TySig   <$>  pVar          <*   pKey "::"  <*> pTyExpr
%%]
%%[5.pDecl
                <|>  sem_Decl_Data    <$   pKey "data"   <*>  pCon       <*> pTyVars
                                                         <*   pKey "="   <*> pDataConstrs
%%]
%%[6.pDecl
                <|>  sem_Decl_KiSig   <$>  pCon          <*   pKey "::"  <*> pKiExpr
%%]
%%[8.pDecl
                <|>  (\conv saf imp nm sig -> sem_Decl_FFI conv saf (if null imp then show nm else imp) nm sig)
                     <$   pKey "foreign" <* pKey "import" <*> pKey "jazy"
                     <*>  ((pKey "safe" <|> pKey "unsafe") `opt` "safe")
                     <*>  (pString `opt` "")
                     <*>  pVar
                     <*   pKey "::" <*> pTyExpr
%%]
%%[9.pDecl
                <|>  pDeclClass
                <|>  pDeclInstance
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for PatExpr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.patExprAlg
patExprAlg      =    (sem_PatExpr_Con,sem_PatExpr_App
                     ,sem_PatExpr_AppTop,sem_PatExpr_Parens)
%%]

%%[1.pPatExprBase
pPatExprBase    =    pVar <**>  (    flip sem_PatExpr_VarAs <$ pKey "@" <*> pPatExprBase
                                <|>  pSucceed sem_PatExpr_Var
                                )
                <|>  sem_PatExpr_Con <$> pCon
%%]
%%[1.pPatExprBase.prod
                <|>  pParenProd patExprAlg pPatExpr
%%]
%%[7.pPatExprBase.prod -1.pPatExprBase.prod
                <|>  pParenRow True (show hsnORec) (show hsnCRec) "=" Nothing
                        (sem_RecPatExpr_Empty,sem_RecPatExpr_Expr . sem_PatExpr_Var,sem_RecPatExpr_Ext,sem_PatExpr_Rec,sem_PatExpr_Parens)
                        pSel pPatExpr
%%]

%%[1.pPatExpr
pPatExpr        =    pApp patExprAlg pPatExprBase
%%]
%%[4.patExpr
                     <??> (sem_PatExpr_TypeAs <$ pKey "::" <*> pTyExpr)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for KiExpr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[6
pKiExpr, pKiExprBase        ::   EHParser T_KiExpr

kiExprAlg       =    (sem_KiExpr_Con,sem_KiExpr_App,sem_KiExpr_AppTop,sem_KiExpr_Parens)
pKiExprBase     =    sem_KiExpr_Con <$> (pCon <|> HNm <$> pKey "*")
                <|>  sem_KiExpr_Var <$> pVar
                <|>  pParens pKiExpr
pKiExpr         =    pChainr (mkArrow kiExprAlg <$ pKeyw hsnArrow) pKiExprBase
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for TyExpr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.tyExprAlg
tyExprAlg       =    (sem_TyExpr_Con,sem_TyExpr_App
                     ,sem_TyExpr_AppTop,sem_TyExpr_Parens)
%%]

%%[1.pTyExprBase
pTyExprBase     =    sem_TyExpr_Con       <$>  pCon
%%]
%%[2.pTyExprBase
                <|>  sem_TyExpr_Wild      <$   pKey "..."
%%]
%%[3.pTyExprBase
                <|>  sem_TyExpr_Var       <$>  pVar
                <|>  sem_TyExpr_VarWild   <$   pKey "%" <*> pVar
%%]
%%[1.pTyExprBase.prod
                <|>  pParenProd tyExprAlg pTyExpr
%%]
%%[7.pTyExprBase.prod -1.pTyExprBase.prod
                <|>  pParenRow False (show hsnORow) (show hsnCRow) "::" Nothing
                        (sem_RowTyExpr_Empty,semVar,sem_RowTyExpr_Ext,sem_TyExpr_Row,id)
                        pVar pTyExpr
                <|>  pParenRow True (show hsnORec) (show hsnCRec) "::" Nothing
                        (sem_RowTyExpr_Empty,semVar,sem_RowTyExpr_Ext
                            ,\r -> mkConApp tyExprAlg hsnRec [sem_TyExpr_Row r]
                            ,sem_TyExpr_Parens)
                        pVar pTyExpr
                <|>  pParenRow False (show hsnOSum) (show hsnCSum) "::" Nothing
                        (sem_RowTyExpr_Empty,semVar,sem_RowTyExpr_Ext
                            ,\r -> mkConApp tyExprAlg hsnSum [sem_TyExpr_Row r]
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
                       (mkArrow tyExprAlg <$ pKeyw hsnArrow)
                       pTyExprBase
%%]
%%[4.pTyExpr -1.pTyExpr
pTyExpr         =    pTyExprPrefix <*> pTyExpr
                <|>  pTyExprApp <??> (flip (mkArrow tyExprAlg) <$ pKeyw hsnArrow <*> pTyExpr)
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
pTyPrExprPrefix =    mkArrow tyExprAlg
                     <$>  pPackImpl
                            (    pPr <|> pIm
                            <|>  pSucceed  sem_TyExpr_NoImpls
                            )
                     <*   pKeyw hsnArrow
                <|>  (    mkArrow tyExprAlg <$> (pPrB <|> pIm)
                     <|>  flip (foldr (mkArrow tyExprAlg))
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
pTyExprApp      =    pApp tyExprAlg pTyExprBase
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
                <|>  pParenProd exprAlg pExpr
%%]
%%[7.pExprBase -1.pExprBaseParenProd
                <|>  pParenRow True (show hsnORec) (show hsnCRec) "=" (Just (":=",sem_RecExpr_Upd))
                        (sem_RecExpr_Empty,sem_RecExpr_Expr . sem_Expr_Var,sem_RecExpr_Ext,sem_Expr_Rec,sem_Expr_Parens)
                        pVar pExpr
%%]
%%[5.pExprBase
                <|>  sem_Expr_Case       <$   pKey "case" <*> pExpr <* pKey "of" <*> pCaseAlts
%%]
%%[8.pExprBase
                <|>  sem_Expr_Undefined  <$   pKey "..."
%%]

-- pExpr
%%[1.exprAlg
exprAlg         =    (sem_Expr_Con,sem_Expr_App
                     ,sem_Expr_AppTop,sem_Expr_Parens)
%%]

%%[1.pExpr
pExpr           =    pE <??> (sem_Expr_TypeAs <$ pKey "::" <*> pTyExpr)
                where pE  =    pExprPrefix <*> pE
                          <|>  pExprApp
%%]

%%[1.pExprApp
pExprApp        =    pApp exprAlg pExprBase
%%]
%%[7.pExprApp -1.pExprApp
pExprApp        =    pApp exprAlg (pExprBase <**> pExprSelSuffix)
%%]
%%[9.pExprApp -7.pExprApp
pExprApp        =    let  pE = pExprBase <**> pExprSelSuffix
                          pA = flip sem_Expr_App <$> pE
                          pI = pPackImpl ((\a p e -> sem_Expr_AppImpl e p a) <$> pExpr <* pKey "<:" <*> pPrExpr)
                     in   pE <??> ((\l e -> sem_Expr_AppTop (foldl (flip ($)) e l)) <$> pList1 (pA <|> pI))
%%]

%%[1.pExprPrefix
pExprPrefix     =    sem_Expr_Let  <$ pKey "let"
                     <*> pDecls    <* pKey "in"
%%]
%%[5.pExprPrefix
                <|>  (\c t e ->  sem_Expr_Case c
                                   (sem_CaseAlts_Cons (sem_CaseAlt_Pat (sem_PatExpr_Con (HNm "True")) t)
                                      (sem_CaseAlts_Cons (sem_CaseAlt_Pat (sem_PatExpr_Con (HNm "False")) e)
                                         sem_CaseAlts_Nil
                     )             )  )
                     <$ pKey "if" <*> pExpr <* pKey "then" <*> pExpr <* pKey "else"
%%]
%%[1.pExprPrefixLam
                <|>  sem_Expr_Lam      <$ pKey "\\"
                     <*> pPatExprBase  <* pKey "->"
%%]
%%[7.pExprPrefixLam -1.pExprPrefixLam
                <|>  (\ps -> \e -> foldr sem_Expr_Lam e ps)  <$ pKey "\\"
                     <*> pList1 pPatExprBase                 <* pKey "->"
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
pPrExprPrefix   =    sem_PrExpr_Arrow <$> pPrExprBase <* pKeyw hsnPrArrow
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
pClassHead 		=	 pTyPrExprPrefix <*> pHd <|> pHd
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
pClassHead      ::   EHParser (T_PrExprs,T_PrExpr)
pClassHead      =    pPrExprClass <**>  (    (\p c -> (sem_PrExprs_Cons c sem_PrExprs_Nil,p))
                                             <$ pKeyw hsnPrArrow <*> pPrExprClass
                                        <|>  pSucceed (\p -> (sem_PrExprs_Nil,p))
                                        )
                <|>  (,) <$> pParens (pFoldrSep (sem_PrExprs_Cons,sem_PrExprs_Nil) pComma pPrExprClass)
                     <* pKeyw hsnPrArrow <*> pPrExprClass

pDeclInstance   ::   EHParser T_Decl
pDeclInstance   =    pKey "instance"
                     *>   (    (\ne -> uncurry (sem_Decl_Instance ne))
                               <$>  ((\n e -> Just (n,e)) <$> pVar <*> (True <$ pKey "<:" <|> False <$ pKey "::") `opt` Nothing)
                               <*>  pClassHead
                               <*   pKey "where" <*> pDecls
                          <|>  sem_Decl_InstanceIntro <$> pExpr <* pKey "<:" <*> pPrExprClass
                          )


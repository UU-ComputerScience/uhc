% $Id: EHC.lag 199 2004-05-12 19:11:13Z andres $

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GRI parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module GRIParser import(IO, UU.Parsing, EHCommon, EHScanner, GrinCode)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scanner
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
scanOpts :: ScanOpts
scanOpts
  =  defaultScanOpts
        {   scoKeywordsTxt      =   [ "eval", "apply", "module", "update", "fetch", "store", "unit", "of", "rec", "case", "ffi"
                                    , "C", "F", "P", "A", "R", "H"
                                    ]
        ,   scoKeywordsOps      =   [ "->", "=", "+=", ":=" ]
        ,   scoSpecChars        =   "();{}#/\\|,"
        ,   scoOpChars          =   "->:=+"
        ,   scoDollarIdent      =   True
        }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
type GRIParser       gp     =    IsParser p Token => p gp

pModule         ::   GRIParser GrModule
pModule         =    GrModule_Mod <$ pKey "module" <*> pGrNm <*> pBindL

pBindL          ::   GRIParser GrBindL
pBindL          =    pCurly_pSemics pBind

pBind           ::   GRIParser GrBind
pBind           =    GrBind_Bind <$> pGrNm <*> pGrNmL <* pKey "=" <*> pCurly pExprSeq
                <|>  GrBind_Rec <$ pKey "rec" <*> pBindL

pExprSeq        ::   GRIParser GrExpr
pExprSeq        =    pChainr ((\p e1 e2 -> GrExpr_Seq e1 p e2) <$ pSemi <* pKey "\\" <*> pPat <* pKey "->") pExpr

pExpr           ::   GRIParser GrExpr
pExpr           =    GrExpr_Unit    <$  pKey "unit"     <*> pVal
                <|>  GrExpr_Store   <$  pKey "store"    <*> pVal
                <|>  GrExpr_Eval    <$  pKey "eval"     <*> pGrNm
                <|>  GrExpr_Fetch   <$  pKey "fetch"    <*> pGrNm   <*>  (Just <$> pInt `opt` Nothing)
                <|>  GrExpr_Update  <$  pKey "update"   <*> pGrNm   <*>  pVal
                <|>  GrExpr_Case    <$  pKey "case"     <*> pVal    <*   pKey "of" <*> pCurly_pSemics pAlt
                <|>  GrExpr_App     <$  pKey "apply"    <*> pGrNm   <*>  pSValL
                <|>  GrExpr_FFI     <$  pKey "ffi"      <*> pId     <*>  pGrNmL
                <|>  GrExpr_Call                        <$> pGrNm   <*>  pSValL

pSVal           ::   GRIParser GrVal
pSVal           =    GrVal_Var      <$> pGrNm
                <|>  GrVal_LitInt   <$> pInt

pVal            ::   GRIParser GrVal
pVal            =    pSVal
                <|>  GrVal_Tag      <$> pTag
                <|>  pParens
                        (    GrVal_Node <$> (pTag <|> pTagVar) <*> pSValL
                        <|>  GrVal_NodeAdapt <$> pGrNm <* pKey "|" <*> pList1Sep pComma pAdapt
                        <|>  pSucceed GrVal_Empty
                        )

pSValL          ::   GRIParser GrValL
pSValL          =    pList pSVal

pValL           ::   GRIParser GrValL
pValL           =    pList pVal

pAdapt          ::   GRIParser GrAdapt
pAdapt          =    pSVal <**> (GrAdapt_Ext <$ pKey "+=" <|> GrAdapt_Upd <$ pKey ":=") <*> pSVal

pAlt            ::   GRIParser GrAlt
pAlt            =    GrAlt_Alt <$> pPat <* pKey "->" <*> pCurly pExprSeq

pSPat           ::   GRIParser GrPat
pSPat           =    GrPat_Var      <$> pGrNm
                <|>  GrPat_LitInt   <$> pInt

pPat            ::   GRIParser GrPat
pPat            =    pSPat
                <|>  GrPat_Tag      <$> pTag
                <|>  pParens
                        (    GrPat_Node <$> (pTag <|> pTagVar) <*> pGrNmL
                        <|>  GrPat_NodeSplit <$> pGrNm <* pKey "|" <*> pList1Sep pComma pSplit
                        <|>  pSucceed GrPat_Empty
                        )

pSplit          ::   GRIParser GrSplit
pSplit          =    GrSplit_Sel <$> pGrNm <* pKey "=" <*> pVal

pTag            ::   GRIParser GrTag
pTag            =    (\i c n -> GrTag_Lit c i n) <$ pKey "#" <*> pInt <* pKey "/" <*> pTagCateg <* pKey "/" <*> pGrNm

pTagVar         ::   GRIParser GrTag
pTagVar         =    GrTag_Var <$> pGrNm

pTagCateg       ::   GRIParser GrTagCateg
pTagCateg       =    GrTagCon       <$ pKey "C"
                <|>  GrTagRec       <$ pKey "R"
                <|>  GrTagHole      <$ pKey "H"
                <|>  GrTagApp       <$ pKey "A"
                <|>  GrTagFun       <$ pKey "F"
                <|>  GrTagPApp      <$ pKey "P" <* pKey "/" <*> pInt

pGrNmL          ::   GRIParser [HsName]
pGrNmL          =    pList pGrNm

pGrNm           ::   GRIParser HsName
pGrNm           =    HNm <$> pVarid

pId             ::   GRIParser String
pId             =    pConid <|> pVarid

pInt            ::   GRIParser Int
pInt            =    read <$> pInteger
%%]


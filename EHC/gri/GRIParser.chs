% $Id$

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
        {   scoKeywordsTxt      =   [ show hsnGrEval, show hsnGrApply
                                    , "module", "update", "fetch", "store", "unit", "of", "rec", "case", "ffi"
                                    , "C", "F", "P", "A", "R", "H", "U"
                                    ]
        ,   scoKeywordsOps      =   [ "->", "=", "+=", "-=", ":=", "-" ]
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
pModule         =    GrModule_Mod <$ pKey "module" <*> (HNm <$> pString) <*> pBindL

pBindL          ::   GRIParser GrBindL
pBindL          =    pCurly_pSemics pBind

pBind           ::   GRIParser GrBind
pBind           =    GrBind_Bind <$> (pGrNm <|> pGrSpecialNm) <*> pGrNmL <* pKey "=" <*> pCurly pExprSeq
                <|>  GrBind_Rec <$ pKey "rec" <*> pBindL

pExprSeq        ::   GRIParser GrExpr
pExprSeq        =    pChainr ((\p e1 e2 -> GrExpr_Seq e1 p e2) <$ pSemi <* pKey "\\" <*> pPat <* pKey "->") pExpr

pExpr           ::   GRIParser GrExpr
pExpr           =    GrExpr_Unit    <$  pKey "unit"         <*> pVal
                <|>  GrExpr_Store   <$  pKey "store"        <*> pVal
                <|>  GrExpr_Eval    <$  pGrKey hsnGrEval    <*> pGrNm
                <|>  GrExpr_Fetch   <$  pKey "fetch"        <*> pGrNm   <*>  (Just <$> pInt `opt` Nothing)
                <|>  GrExpr_Update  <$  pKey "update"       <*> pGrNm   <*>  pVal
                <|>  GrExpr_Case    <$  pKey "case"         <*> pVal    <*   pKey "of" <*> pCurly_pSemics pAlt
                <|>  GrExpr_App     <$  pGrKey hsnGrApply   <*> pGrNm   <*>  pSValL
                <|>  GrExpr_FFI     <$  pKey "ffi"          <*> pId     <*>  pGrNmL
                <|>  GrExpr_Call                            <$> pGrNm   <*>  pSValL

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
pAdapt          =    pSVal <**> ((flip GrAdapt_Ins <$ pKey "+=" <|> flip GrAdapt_Upd <$ pKey ":=") <*> pVal <|> GrAdapt_Del <$ pKey "-=")

pAlt            ::   GRIParser GrAlt
pAlt            =    GrAlt_Alt <$> pPat <* pKey "->" <*> pCurly pExprSeq

pSPat           ::   GRIParser GrPat
pSPat           =    GrPat_Var      <$> pGrNm
                <|>  GrPat_LitInt   <$> pInt

pPat            ::   GRIParser GrPat
pPat            =    pSPat
                <|>  GrPat_Tag      <$> pTag
                <|>  pParens
                        (    (pTag <|> pTagVar)
                             <**>  (pGrNm
                                    <**>  (    (\sL r t -> GrPat_NodeSplit t r sL) <$ pKey "|" <*> pList1Sep pComma pSplit
                                          <|>  (\nL n t -> GrPat_Node t (n:nL)) <$> pGrNmL
                                          )
                                   <|> pSucceed (flip GrPat_Node [])
                                   )
                        <|>  pSucceed GrPat_Empty
                        )

pSplit          ::   GRIParser GrSplit
pSplit          =    GrSplit_Sel <$> pGrNm <* pKey "=" <*> pVal

pTag            ::   GRIParser GrTag
pTag            =    pKey "#"
                     *>  (   (\i c n -> GrTag_Lit c i n) <$> pInt <* pKey "/" <*> pTagCateg <* pKey "/" <*> (pGrNm <|> pGrSpecialNm)
                         <|> GrTag_Unboxed <$ pKey "U"
                         )

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

pGrSpecialNm    ::   GRIParser HsName
pGrSpecialNm    =    pGrKey hsnGrEval <|> pGrKey hsnGrApply

pGrKey          ::   HsName -> GRIParser HsName
pGrKey k        =    HNm <$> pKey (show k)

pId             ::   GRIParser String
pId             =    pConid <|> pVarid

pInt            ::   GRIParser Int
pInt            =    (negate <$ pKey "-" `opt` id) <*> (read <$> pInteger)
%%]


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
        {   scoKeywordsTxt      =   [ "eval", "apply", "module", "update", "fetch", "store", "unit", "of", "rec"
                                    , "C", "F", "P", "A"
                                    ]
        ,   scoKeywordsOps      =   [ "->" ]
        ,   scoSpecChars        =   "();{}=#/\\"
        ,   scoOpChars          =   "->"
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
pExpr           =    GrExpr_Unit <$ pKey "unit" <*> pVal
                <|>  GrExpr_Store <$ pKey "store" <*> pVal
                <|>  GrExpr_Eval <$ pKey "eval" <*> pVal
                <|>  GrExpr_Fetch <$ pKey "fetch" <*> pGrNm <*> (Just <$> pInt `opt` Nothing)
                <|>  GrExpr_Update <$ pKey "update" <*> pGrNm <*> pVal
                <|>  GrExpr_Case <$ pKey "case" <*> pVal <* pKey "of" <*> pCurly_pSemics pAlt
                <|>  GrExpr_App <$ pKey "apply" <*> pGrNm <*> pValL
                <|>  GrExpr_Call <$> pGrNm <*> pValL

pVal            ::   GRIParser GrVal
pVal            =    GrVal_Var <$> pGrNm
                <|>  GrVal_LitInt <$> pInt
                <|>  GrVal_Tag <$> pTag
                <|>  pParens (GrVal_Node <$> pTag <*> pValL `opt` GrVal_Empty)

pValL           ::   GRIParser GrValL
pValL           =    pList pVal

pAlt            ::   GRIParser GrAlt
pAlt            =    GrAlt_Alt <$> pPat <* pKey "->" <*> pCurly pExprSeq

pPat            ::   GRIParser GrPat
pPat            =    GrPat_Var <$> pGrNm
                <|>  GrPat_LitInt <$> pInt
                <|>  GrPat_Tag <$> pTag
                <|>  pParens (GrPat_Node <$> pTag <*> pGrNmL `opt` GrPat_Empty)

pTag            ::   GRIParser GrTag
pTag            =    GrTag_Var <$> pGrNm
                <|>  (\i c n -> GrTag_Lit c i n) <$ pKey "#" <*> pInt <* pKey "/" <*> pTagCateg <* pKey "/" <*> pGrNm

pTagCateg       ::   GRIParser GrTagCateg
pTagCateg       =    GrTagCon <$ pKey "C"
                <|>  GrTagApp <$ pKey "A"
                <|>  GrTagFun <$ pKey "F"
                <|>  GrTagPartial <$ pKey "P" <*> pInt

pGrNmL          ::   GRIParser [HsName]
pGrNmL          =    pList pGrNm

pGrNm           ::   GRIParser HsName
pGrNm           =    HNm <$> pVarid

pInt            ::   GRIParser Int
pInt            =    read <$> pInteger
%%]


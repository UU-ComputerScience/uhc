%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GRI parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{EH}GrinCode.Parser} import(IO, UU.Parsing, EH.Util.ParseUtils(PlainParser), EH.Util.ScanUtils, {%{EH}Base.Common}, {%{EH}Scanner.Scanner}, {%{EH}GrinCode}, {%{EH}Base.Parser} hiding (pInt)) export(pModule)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scanner
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]
scanOpts, grinScanOpts :: ScanOpts
scanOpts
  =  defaultScanOpts
        {   scoKeywordsTxt      =   [ "eval", "apply"
                                    , "module", "update", "fetch", "store", "unit", "of", "rec", "case", "ffi"
                                    , "throw", "try", "catch", "ctags", "applymap", "evalmap"
                                    , "C", "F", "P", "A", "R", "H", "U", "W"
                                    ]
        ,   scoKeywordsOps      =   [ "<-", "->", "=", "+=", "-=", ":=", "-" ]
        ,   scoSpecChars        =   "();{}#/\\|,"
        ,   scoOpChars          =   "<->:=+"
        ,   scoDollarIdent      =   True
        }

grinScanOpts = scanOpts

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
type GRIParser       gp     =    PlainParser Token gp

pModule         ::   GRIParser GrModule
pModule         =    GrModule_Mod <$ pKey "module" <*> (pGrNm <|> mkHNm <$> pString)
                     <*> pGlobalL
                     <*> pBindL
                     <*  pKey "ctags"     <*> pCTags
                     <*  pKey "evalmap"   <*> pEvApTagMp
                     <*  pKey "applymap"  <*> pEvApTagMp

pGlobalL        ::   GRIParser GrGlobalL
pGlobalL        =    pCurly_pSemics pGlobal

pGlobal         ::   GRIParser GrGlobal
pGlobal         =    GrGlobal_Global <$> pGrNm <* pKey "<-" <* pKey "store" <*> pVal

pBindL          ::   GRIParser GrBindL
pBindL          =    pCurly_pSemics pBind

pBind           ::   GRIParser GrBind
pBind           =    GrBind_Bind <$> pGrNm <*> pGrNmL <* pKey "=" <*> pCurly pExprSeq
                <|>  GrBind_Rec <$ pKey "rec" <*> pBindL

pCTags          ::   GRIParser CTagsMp
pCTags          =    pCurly_pSemics
                        ((\tn ts -> (tn,map (\(n,t,a) -> (n,CTag tn n t a)) ts))
                        <$> pGrNm <* pKey "=" <*> pListSep (pKey "|") ((,,) <$> pGrNm <*> pInt <*> pInt)
                        )

pEvApTagMp      ::   GRIParser EvApTagMp
pEvApTagMp      =    pCurly_pSemics
                        ((\t a ea -> ((t,a),ea))
                        <$> pTag <*> pInt <* pKey "->"
                            <*> (EvApTagTag <$> pTag <|> EvApTagUnit <$ pKey "unit" <|> EvApTagVar <$> pGrNm <|> EvApTagThrow <$ pKey "throw")
                        )

pExprSeq        ::   GRIParser GrExpr
pExprSeq        =    pChainr ((\p e1 e2 -> GrExpr_Seq e1 p e2) <$ pSemi <* pKey "\\" <*> pPat <* pKey "->") pExpr

pExpr           ::   GRIParser GrExpr
pExpr           =    GrExpr_Unit    <$  pKey "unit"         <*> pVal
                <|>  GrExpr_Store   <$  pKey "store"        <*> pVal
                <|>  GrExpr_Eval    <$  pKey "eval"         <*> pGrNm
                <|>  GrExpr_Fetch   <$  pKey "fetch"        <*> pGrNm   <*>  (Just <$> pInt `opt` Nothing)
                                                                        <*>  (Just <$> pTag `opt` Nothing)
                <|>  GrExpr_FetchUpdate
                					<$  pKey "fetchupdate"  <*> pGrNm   <*>  pGrNm
                <|>  GrExpr_Update  <$  pKey "update"       <*> pGrNm   <*>  pVal
                                                                        <*>  (Just <$> pTag `opt` Nothing)
                <|>  GrExpr_Case    <$  pKey "case"         <*> pVal    <*   pKey "of" <*> pCurly_pSemics pAlt
                <|>  GrExpr_App     <$  pKey "apply"        <*> pGrNm   <*>  pSValL
                <|>  GrExpr_FFI     <$  pKey "ffi"          <*> pId     <*>  pGrNmL
                                                            <*> pCurly_pSemics pTag
                <|>  GrExpr_Throw   <$  pKey "throw"        <*> pGrNm
                <|>  GrExpr_Catch   <$  pKey "try"          <*> pCurly pExprSeq
                                    <*  pKey "catch"        <*> pParens pGrNm <*> pCurly pExprSeq
                <|>  GrExpr_Call                            <$> pGrNm   <*>  pSValL

pSVal           ::   GRIParser GrVal
pSVal           =    GrVal_Var      <$> pGrNm
                <|>  GrVal_LitStr   <$> pString
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
                     *>  (   (\i c n -> GrTag_Lit c i n) <$> pInt <* pKey "/" <*> pTagCateg <* pKey "/" <*> pGrNm
                         <|> GrTag_Unboxed <$ pKey "U"
                         <|> GrTag_Any     <$ pKey "*"
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
                <|>  GrTagWorld     <$ pKey "W"

pGrNmL          ::   GRIParser [HsName]
pGrNmL          =    pList pGrNm

pGrNm           ::   GRIParser HsName
pGrNm           =    mkHNm <$> pVarid

pGrKey          ::   HsName -> GRIParser HsName
pGrKey k        =    HNm <$> pKey (show k)

pId             ::   GRIParser String
pId             =    pConid <|> pVarid

pInt            ::   GRIParser Int
pInt            =    (negate <$ pKey "-" `opt` id) <*> (read <$> pInteger)
%%]


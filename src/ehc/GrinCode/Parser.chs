%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GRI parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{EH}GrinCode.Parser} import(IO, UU.Parsing, EH.Util.ParseUtils(PlainParser), EH.Util.ScanUtils, {%{EH}Base.Common}, {%{EH}Scanner.Scanner}, {%{EH}GrinCode}, {%{EH}Base.Parser} hiding (pInt))
%%]

%%[8 export(pModule,pExprSeq)
%%]

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
                        ((\tn ts -> (tn,map (\(n,t,a,ma) -> (n,CTag tn n t a ma)) ts))
                        <$> pGrNm <* pKey "=" <*> pListSep (pKey "|") ((,,,) <$> pGrNm <*> pInt <*> pInt <*> pInt)
                        )

pExprSeq        ::   GRIParser GrExpr
pExprSeq        =    pChainr ((\p e1 e2 -> GrExpr_Seq e1 p e2) <$ pSemi <* pKey "\\" <*> pPatLam <* pKey "->") pExpr

pExpr           ::   GRIParser GrExpr
pExpr           =    GrExpr_Unit    <$  pKey "unit"         <*> pVal
                <|>  GrExpr_UpdateUnit <$  pKey "updateunit"<*> pGrNm <*> pVal    
                <|>  GrExpr_Store   <$  pKey "store"        <*> pVal
                <|>  GrExpr_Eval    <$  pKey "eval"         <*> pGrNm
                <|>  GrExpr_FetchNode
                                    <$ pKey "fetchnode"     <*> pGrNm
                <|>  GrExpr_FetchField
                                    <$pKey "fetchfield"     <*> pGrNm   <*>  pInt   <*>  (Just <$> pTag `opt` Nothing)
                <|>  GrExpr_FetchUpdate
                					<$  pKey "fetchupdate"  <*> pGrNm   <*>  pGrNm
                <|>  GrExpr_Case    <$  pKey "case"         <*> pVal    <*   pKey "of" <*> pCurly_pSemics pAlt
                <|>  GrExpr_App     <$  pKey "apply"        <*> pGrNm   <*>  pSValL
                <|>  GrExpr_FFI     <$  pKey "ffi"          <*> pId     <*>  pValL
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
                        (    GrVal_Node <$> pTag <*> pSValL
%%[[10                        
                        <|>  GrVal_NodeAdapt <$> pGrNm <* pKey "|" <*> pList1Sep pComma pAdapt
%%]]                        
                        <|>  GrVal_VarNode <$> ( (:) <$> (GrVal_Var <$> pGrNm) <*> pSValL )
                        <|>  GrVal_BasicNode  <$  pKey "basicnode"  <*> pTag <*> pGrNm
                        <|>  GrVal_EnumNode   <$  pKey "enumnode"   <*> pGrNm
                        <|>  GrVal_PtrNode    <$  pKey "ptrnode"    <*> pGrNm
                        <|>  GrVal_OpaqueNode <$  pKey "opaquenode" <*> pGrNm
                        <|>  pSucceed GrVal_Empty
                        )

pSValL          ::   GRIParser GrValL
pSValL          =    pList pSVal

pValL           ::   GRIParser GrValL
pValL           =    pList pVal

pAlt            ::   GRIParser GrAlt
pAlt            =    GrAlt_Alt <$> pAltAnn <*> pPatAlt <* pKey "->" <*> pCurly pExprSeq

pAltAnn         ::   GRIParser GrAltAnn
pAltAnn         =    (    GrAltAnnNormal  <$ pKey "normal"
                     <|>  GrAltAnnIdent   <$ pKey "ident"
                     <|>  GrAltAnnReenter <$ pKey "reenter"
                     <|>  pSucceed GrAltAnnNormal
                     )
                     

pPatLam         ::   GRIParser GrPatLam
pPatLam         =    GrPatLam_Var      <$> pGrNm
                <|>  pParens
                        (    GrPatLam_VarNode      <$> ( (:) <$> (GrVar_Var <$> pGrNm <|> GrVar_KnownTag <$> pTag) <*> (map GrVar_Var <$> pGrNmL) )
                        <|>  GrPatLam_BasicNode    <$  pKey "basicnode"    <*> pBasicAnnot <*> pGrNm
                        <|>  GrPatLam_EnumNode     <$  pKey "enumnode"     <*> pGrNm
                        <|>  GrPatLam_PtrNode      <$  pKey "ptrnode"      <*> pGrNm
                        <|>  GrPatLam_OpaqueNode   <$  pKey "opaquenode"   <*> pGrNm
                        <|>  GrPatLam_BasicAnnot   <$  pKey "basicannot"   <*> pBasicAnnot <*> pGrNm
                        <|>  GrPatLam_EnumAnnot    <$  pKey "enumannot"    <*> pParens (pList pTag) <*> pGrNm
                        <|>  GrPatLam_OpaqueAnnot  <$  pKey "opaqueannot"  <*> pGrNm
                        <|>  GrPatLam_PtrAnnot     <$  pKey "ptrannot"     <*> pParens (pList pTag) <*> pGrNm
                        <|>  pSucceed GrPatLam_Empty
                        )

pPatAlt         ::   GRIParser GrPatAlt
pPatAlt         =    GrPatAlt_LitInt   <$> pInt
                <|>  GrPatAlt_Tag      <$> pTag
                <|>  pParens
                        (    pTag
                             <**>  (pGrNm
                                    <**>  (    
%%[[10                                    
                                               (\sL r t -> GrPatAlt_NodeSplit t r sL) <$ pKey "|" <*> pList1Sep pComma pSplit <|>  
%%]]                                          
                                               (\nL n t -> GrPatAlt_Node t (n:nL)) <$> pGrNmL
                                          )
                                   <|> pSucceed (flip GrPatAlt_Node [])
                                   )
                        )

pBasicTy        ::   GRIParser BasicTy
pBasicTy        =    BasicTy_Word   <$ pKey "word"
%%[[97
                <|>  BasicTy_Float  <$ pKey "float"
                <|>  BasicTy_Double <$ pKey "double"
%%]]

pBasicAnnot     ::   GRIParser BasicAnnot
pBasicAnnot     =    BasicAnnot_Size          <$> pInt <*> pBasicTy
                <|>  BasicAnnot_FromTaggedPtr <$  pKey "annotfromtaggedptr"
                <|>  BasicAnnot_ToTaggedPtr   <$  pKey "annottotaggedptr"
                <|>  BasicAnnot_Dflt          <$  pKey "annotdflt"

pTag            ::   GRIParser GrTag
pTag            =    pKey "#"
                     *>  (   (\i c n -> c i n) <$> pInt <* pKey "/" <*> pTagCateg <* pKey "/" <*> pGrNm
                         <|> GrTag_Unboxed <$ pKey "U"
                         <|> GrTag_Any     <$ pKey "*"
                         )

pTagAnn         ::   GRIParser GrTagAnn
pTagAnn         =    GrTagAnn       <$ pOCurly <*> pInt <* pComma <*> pInt <* pCCurly

pTagCateg       ::   GRIParser (Int -> HsName -> GrTag)
pTagCateg       =    (\a     -> GrTag_Con a   )    <$ pKey "C" <*> pTagAnn
                <|>  (\  i n -> GrTag_Rec     )    <$ pKey "R"
                <|>  (\  i n -> GrTag_Hole    )    <$ pKey "H"
                <|>  (\  i   -> GrTag_App     )    <$ pKey "A"
                <|>  (\  i   -> GrTag_Fun     )    <$ pKey "F"
                <|>  (\x i   -> GrTag_PApp x  )    <$ pKey "P" <* pKey "/" <*> pInt
                <|>  (\  i n -> GrTag_World   )    <$ pKey "W"

pGrNmL          ::   GRIParser [HsName]
pGrNmL          =    pList pGrNm

pGrNm           ::   GRIParser HsName
pGrNm           =    mkHNm <$> pVarid

pGrKey          ::   HsName -> GRIParser HsName
pGrKey k        =    hsnFromString <$> pKey (show k)

pId             ::   GRIParser String
pId             =    pConid <|> pVarid

pInt            ::   GRIParser Int
pInt            =    (negate <$ pKey "-" `opt` id) <*> (read <$> pInteger)
%%]


%%[10
pSplit          ::   GRIParser GrSplit
pSplit          =    GrSplit_Sel <$> pGrNm <* pKey "=" <*> pVal

pAdapt          ::   GRIParser GrAdapt
pAdapt          =    pSVal <**> ((flip GrAdapt_Ins <$ pKey "+=" <|> flip GrAdapt_Upd <$ pKey ":=") <*> pVal <|> GrAdapt_Del <$ pKey "-=")

%%]


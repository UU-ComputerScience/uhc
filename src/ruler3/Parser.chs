-------------------------------------------------------------------------
-- Ruler parser
-------------------------------------------------------------------------

%%[1 hs module (Parser)
%%]

%%[1 hs export (pAGItf)
%%]

%%[1 hs import (qualified Data.Set as Set, qualified Data.Map as Map, UU.Parsing)
%%]

%%[1 hs import (UU.Parsing.Offside, UU.Scanner.Position( initPos, Pos, Position(..) ))
%%]

%%[1 hs import (UU.Scanner.GenToken, Scanner, EH.Util.ParseUtils, EH.Util.ScanUtils)
%%]

%%[1 hs import (NmParser, SelParser, KeywParser, ViewSel.ViewSel, ViewSel.Parser)
%%]

%%[1 hs import (Common, AbsSyn.AbsSyn1)
%%]

%%[1 hs

-------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------

type RulerParser ep
  = (IsParser (OffsideParser i o Token p) Token,InputState i Token p, OutputState o, Position p)
      => OffsideParser i o Token p ep
{-
type RulerParser ep
  = (IsParser (OffsideParser i o Token p) Token,InputState i Token p, OutputState o, Position p)
      => OffsideParser i o Token p ep
-}

type MkConAppAlg t = (String -> t,t -> t -> t,t -> t)

mkApp :: MkConAppAlg t -> [t] -> t
mkApp (_,app,top) ts
  = case ts of
      [t]  -> t
      _    -> top t
  where t = foldl1 app ts

pAGItf :: (IsParser p Token) => p AGItf -- RulerParser AGItf
pAGItf
  = let alg                 =   (undefined,Expr_App,Expr_AppTop)
        pAGItf              =   AGItf_AGItf <$> pTopDecls' pDeclGlob
        pLay        p       =   pBlock  pOParen (pKey "|") pCParen p
        pLay1       p       =   pBlock1 pOParen (pKey "|") pCParen p
        pLay2Sep open sep close sepElt p
                            =   pOffside open close explicit implicit
                            where sep'    = () <$ sep
                                  elems s = (,) <$> l <* s <* sepElt <* s <*> l
                                          where l  = pListSep s p
                                                -- sp = pList s
                                  explicit = elems sep'
                                  implicit = elems (sep' <|> pSeparator)
{-
pBlock open sep close p =  pOffside open close explicit implicit
 where elem = (:) <$> p `opt` id
       sep' = () <$ sep        
       elems s = ($[]) <$> pFoldr1Sep ((.),id) s elem
       explicit = elems sep'
       implicit = elems (sep' <|> pSeparator)

pBlock1 open sep close p =  pOffside open close explicit implicit
 where sep'    = () <$ sep
       elems s = pList s *> pList1Sep (pList1 s) p <* pList s
       explicit = elems sep'
       implicit = elems (sep' <|> pSeparator)
        pTopDecls'  pD      =   pLay pD
-}
        pTopDecls'  pD      =   pList pD
        pDecls'     pD      =   pList pD
        pDecls1'    pD      =   pList1 pD
        pDeclRule           =   (\(n,p) mn s mag d -> Decl_Rule p n mn s mag d)
                                                       <$  pKeySPos "rule"      <*> pNmSPos
                                                       <*> pMb (pKey ":" *> pNm)
                                                       <*> pMbViewSel'
                                                       <*> pMbString
                                                       <*  pKey "="             <*> (pDeclRuleDflt <|> pDecls1' pDeclRulView)
        pDeclRuleDflt       =   (\d -> [Decl_RulView emptySPos nmNone d])
                                                       <$> pRuleJudgeIntros
        pDeclRulView        =   (\(v,p) d -> Decl_RulView p v d)
                                                       <$  pKey "view"          <*> pNmVwSPos
                                                       <*  pKey "="             <*> pRuleJudgeIntros
{-
        mkDeclRulView p v pre post
                            =   Decl_RulView p v [RuleJudgeIntro_PrePost pre post]
        pRuleViewJudges     =   (,) <$> pRExprs <* sep <*> pRExprs
                            <|> pKey "judges" *> pLay2Sep pOParen (pKey "|") pCParen sep pRExprBase -- experimental
                            where pRExprs    = pList pRExpr
                                  -- pRExprsLay = pLay  pRExprBase
                                  sep = pKey "-" <|> pKey "---"
-}
        pRuleJudgeIntroPrePost
                            =   RuleJudgeIntro_PrePost <$> (pKey "extern" *> pList1 pNmDir `opt` []) <*> pRExprs <* sep <*> pRExprs
                            where pRExprs    = pList pRExpr
                                  -- pRExprsLay = pLay  pRExprBase
                                  sep = pKey "-" <|> pKey "---"
        pRuleJudgeIntros    =   (:[]) <$> pRuleJudgeIntroPrePost
                            <|> pList1Sep (pKey "|") (pRuleJudgeIntro <|> pParens pRuleJudgeIntroPrePost)
        pRuleJudgeIntro     =   (\(rsn,p) rln -> RuleJudgeIntro_RulesetRule p rsn rln)
                                <$  pKey "ruleset" <*> pNmSPos <* pKey "rule" <*> pNm
                                <*> (pParens_pCommas (BldRename <$ pKey "scheme" <*> pNm <* pKey "->" <*> pNm) `opt` [])
        pDeclScm            =   Decl_ScmView           <$  pKey "view"          <*> pNmVw
                                                       <*  pKey "="             <*> pDeclsScmView
        pDeclScmDflt        =   (\v -> [Decl_ScmView nmNone v])
                                                       <$> pDeclsScmView
        pDeclScmView        =   pKeySPos "judgespec"
                                <**> (   (\e p -> Decl_ShpJudge p FmSpec e) <$> pExprLF
                                     )
                            <|> pKeySPos "judgeshape" <**> pShp pFmKd3WithDflt pFmKd3
                            <|> pKeySPos "judgeuse"   <**> pShp pFmKd2         pFmKd2
                            <|> Decl_Attr              <$  (pKey "hole" <|> pKey "holes")
                                                       <*> pAttrIntroDecls
                            <|> Decl_Explain           <$  pKey "explain" <*> pMb (pNmDir <* pKey "=") <*> pParens pExprExplain
                            where pShp pFmAdd pFmDel
                                    = (   (\k e p -> Decl_ShpJudge p k e) <$> pFmAdd <*> pExprLF
                                      <|> (flip Decl_ShpDel) <$ pKey "-" <*> pList1 pFmDel
                                      )
        pAttrIntroDecl      =   pKey "["
                                *> pAttrIntros
                                   <**> (     (\is s i -> AttrIntroDecl_Attrs i is s)
                                                <$ pKey "|" <*> pAttrIntros <* pKey "|" <*> pAttrIntros
                                        `opt` AttrIntroDecl_AttrsProp
                                        )
                                <* pKey "]"
                            <|> (\(s,p) r -> AttrIntroDecl_Scheme p s r)
                                                       <$  pKey "scheme" <*> pNmSPos <*> (pAttrRenames `opt` [])
        pAttrIntroDecls     =   pList1Sep (pKey "|") pAttrIntroDecl
        pAttrRename         =   pNmSPos
                                <**> (   (\o (n,p) -> AttrRename_Rename  p n o) <$ pKey ":=" <*> pNm
                                     <|> (\r (l,p) -> AttrRename_EqualTo p l r) <$ pKey  "=" <*> pNm
                                     )
        pAttrRenames        =   pParens_pCommas pAttrRename
        pDeclsScmView       =   pDecls1' pDeclScmView
        pDeclDataASTView    =   (\(n,p) d -> Decl_DataASTView p n d)
                                                       <$  pKey "view" <*> pNmSPos <* kIsBar <*> pDeclDataASTAlts
        pDeclDataASTViewDflt=   (\d -> [Decl_DataASTView emptySPos nmNone d])
                                                       <$> pDeclDataASTAlts
        pDeclDataASTAlts    =   pList1Sep (pKey "|") pDeclDataASTAlt
        pDeclDataASTAlt     =   (\(n,p) (rn,mbon) d -> Decl_DataASTAlt p n rn mbon d)
                                                       <$> pNmSPos <*> pBracks (pNm <+> pMb (pKey ":" *> pNm)) <*> pFldIntros
        pDeclGlob           =   pDeclGlobScheme
                            <|> Decl_Fmt               <$  (pKey "format" <|> pKey "rewrite")
                                                       <*> pFmKd2WithDflt
                                                       <*> pAtIO
                                                       <*> pExpr
                                                       <*  pKey "="             <*> pExpr
                            <|> (\(n,p) sn mvs s d -> Decl_Rules p n sn mvs s d)
                                                       <$  (pKey "rules" <|> pKey "ruleset")
                                                       <*> pNmSPos
                                                       <*  pKey "scheme" <*> pNm <*> pMbViewSel <*> pString
                                                       <*  pKey "=" <*> pDecls' pDeclRule
                            <|> (\(n,p) sn mvs s r -> Decl_RulesGroup p n sn mvs s r)
                                                       <$  pKey "rulesgroup"
                                                       <*> pNmSPos
                                                       <*  pKey "scheme" <*> pNm <*> pMbViewSel <*> pString
                                                       <*  pKey "="             <*> pList1 ((,) <$ pKey "rule" <*> pNm <*> pNm)
                            <|> Decl_ViewHierarchy     <$  pKey "viewhierarchy"
                                                       <*  pKey "="             <*> pList1Sep pComma (pList1Sep (pKey "<") pNmVw)
                            <|> Decl_Preamble          <$  pKey "preamble"      <*> pFmKd2WithDflt <*> pString
                            <|> Decl_Extern            <$  (pKey "extern" <|> pKey "external")
                                                       <*> pList1 pNm
                            <|> (\(n,p) sn d -> Decl_DataAST p n sn d)
                                                       <$  pKey "data" <*> pNmSPos <*> pBracks_pCommas pNm
                                                       <*> (pDeclDataASTViewDflt <|> pDecls' pDeclDataASTView)
                            <|> (\(n,p) -> Decl_Include p n)
                                                       <$  pKey "include" <*> pNmSPos
        pDeclGlobScheme     =   (ScJudge <$ pKey "scheme" <|> ScRelation <$ pKey "relation")
                                <**> (pNmSPos
                                     <**> (   (\d (ms,ds) (n,p) k -> Decl_SchemeDeriv p k n d ms ds)
                                              <$ pKey ":" <*> pScDeriv <*> pTl2
                                          <|> (\(ms,ds) (n,p) k -> Decl_Scheme p k n ms ds)
                                              <$> pTl1
                                          )
                                     )
                            where pTl1 = (,) <$> pMbString <*> pDs
                                  pDs = pKey "=" *> (pDeclScmDflt <|> pDecls1' pDeclScm)
                                  pTl2 = (,) <$> pMbString <*> (pMaybe [] id pDs)
        pScDeriv            =   ScList <$ pKey "[" <*> pNm <* pKey "]"
        pMbViewSel'         =   pMaybe Nothing Just (pKey "viewsel" *> pViewSel)
        pMbViewSel          =   pMaybe ViewSel_All id (pKey "viewsel" *> pViewSel)
        pFmKd2WithDflt      =   pMaybe FmAll id pFmKd2
        pFmKd3WithDflt      =   pMaybe FmAll id pFmKd3
        pFmKd2              =   FmTeX <$ pKey "tex" <|> FmAG <$ pKey "ag" <|> FmFmtCmd <$ pKey "fmtcmd"
        pFmKd3              =   FmSpec <$ pKey "spec" <|> pFmKd2
        pAtIO               =   pMaybe AtInOut id (AtIn <$ pKey "def" <|> AtOut <$ pKey "use")
        pNmBase             =   Nm <$> pNmStr
        pNmDir              =   pNmDotted (pSymStr <|> pAnyKey pKey keywordsTextDir)
        -- pNmDirSPos          =   pNmDottedSPos (pSymStr <|> pAnyKey pKey keywordsTextDir)
        pNmC                =   Nm <$> pConid
        pNmV                =   Nm <$> pVarid
        pSym                =   Nm <$> pSymStr
        pRExpr              =   pKey "judge" *> pRExprBase
        pRExprBase          =   (\mrnp (n,p) e
                                    -> let (mrn,p') = maybe (Nothing,p) (\(n,p) -> (Just n,p)) mrnp
                                       in  RExpr_Judge p' mrn n e
                                )
                                <$> pMb (pNmSPos <* pKey ":") <*> pNmSPos <*> pRExprEqn
                            <|> (\(n,p) nl -> RExpr_Del p (n:nl))
                                <$ pKey "-" <*> pNmSPos <*> pList pNm
        pRExprEqn           =   RExprEqn_Attrs <$> pAttrEqns
                            <|> RExprEqn_Expr  <$  pKey "=" <*> pExpr
        pExpr               =   pExprApp
                                <??> ((\es e1 -> let (op,e2) = foldr (\(op,e1) (f2,e2) -> (op,f2 e1 e2)) (const,undefined) es
                                                 in Expr_AppTop (op e1 e2)
                                      )
                                      <$> pList1 ((,) <$> pOp <*> pExprApp)
                                     )
                            where pOp = (\s ss -> Expr_Op s (ss (Expr_Var s))) <$> pSym <*> pExprDot
        pExprExplain        =   foldl1 Expr_SP <$> pList1 pW
                            where pW =   pExprAtom (Nm <$> (pNmStr <|> pSymStr <|> pAnyKey pKey (keywordsOpsExplainEsc ++ keywordsText)))
                                     <|> Expr_Expr <$ pKey "|" <*> pExpr <* pKey "|"
                                     <|> Expr_Paren <$> pParens pExprExplain
        pExprLF             =   pChainr (Expr_LF <$ pKey "|") pExpr
        pExprLFSuffix nl    =   (\es e -> Expr_LF e (foldr1 Expr_LF es)) <$> pList1 (pKey nl *> pExpr)
        pExprApp            =   mkApp alg <$> pList1 pExprBase <|> pSucceed Expr_Empty
        pExprAtom pN        =   Expr_Var     <$> pN
                            <|> Expr_Int     <$> pInteger
                            <|> Expr_StrAsIs <$> pString
        pExprBase' pN       =   pParens (pExprInParen <|> pExprSpecial) <**> pExprDot
                            <|> Expr_Uniq    <$  pKey "unique"
                            <|> pExprAtom pN
        pExprBase           =   pExprBase' pNmDir
        pExprInParen        =   pExpr <**> (   flip Expr_Cnstr <$  pKey "|" <*> pECnstr
                                           <|> pExprLFSuffix "-"
                                           <|> pSucceed Expr_Paren
                                           )
        pExprSpecial        =   (Expr_StrAsIs . concat)     <$> pList1 (pAnyKey pKey (keywordsOpsParenEsc ++ keywordsTextEscapable))
                            -- <|> Expr_Named                  <$> pNm <* pKey "=" <*> pExpr
                            <|> Expr_StrText                <$  pKey "text" <*> pString
                            <|> Expr_Retain                 <$  pKey "retain" <*> pExpr
                            <|> Expr_ChildOrder             <$  pKey "node" <*> pInt <* pKey "=" <*> pExpr
        pExprDot            =   pSel (Expr_SelTop,Expr_Sel,Just) (pE,pMbE)
                            where pE   = pExprBase' pNmBase
                                  pMbE = Just <$> pE <|> pSucceed Nothing
        pECnstr             =   ECnstr_Ty  <$> pList1Sep pComma pNmC
                            <|> ECnstr_Var <$> pNmV
        pTy                 =   Ty_Con <$> pNmC
                            <|> Ty_App (Ty_Con nmList) <$> pBracks pTyApp
                            <|> pParens pTyApp
        pTyApp              =   foldl1 Ty_App <$> pList1 pTy
        pMbString           =   pMb pString
        pFldIntros          =   pList pFldIntro
        pFldIntro           =   FldIntro_Intro <$> pNm <* kDColon <*> pTy
        pAttrIntros         =   pListSep pComma pAttrIntro
        pAttrIntro          =   AttrIntro_Intro <$> pList pAttrProp <*> pNm <* kDColon <*> pNmC
        pAttrEqns           =   pList (pKey "|" *> pAttrEqn)
        pAttrEqn            =   AttrEqn_Eqn <$>              pNmDir <* pKey "=" <*> pExpr
                            <|> AttrEqn_Del <$  pKey "-" <*> pNm
        pAttrProp           =   foldr1 (<|>) [ v <$ pKey n | (n,v) <- Map.toList propsMp ]
        pInt                =   read <$> pInteger
        kDColon             =   pKey ":" <|> pKey "::"
        kIsBar              =   pKey "=" <|> pKey "|"
     in (pAGItf)

%%]

-------------------------------------------------------------------------
-- Ruler parser
-------------------------------------------------------------------------

module RulerParser
  where

import qualified Data.Set as Set
import qualified Data.Map as Map
-- import Data.List
-- import IO
import UU.Parsing
-- import UU.Parsing.CharParser
import UU.Scanner.Position( initPos, Pos, Position(..) )
import UU.Scanner.GenToken
import UU.Scanner
import ParseUtils
import ScanUtils
import NmParser
import SelParser
import KeywParser
import ViewSel
import ViewSelParser
import Common
-- import Utils (wordsBy)
import MainAG

-------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------

type MkConAppAlg t = (String -> t,t -> t -> t,t -> t)

mkApp :: MkConAppAlg t -> [t] -> t
mkApp (_,app,top) ts
  = case ts of
      [t]  -> t
      _    -> top t
  where t = foldl1 app ts

pAGItf :: (IsParser p Token) => (p T_AGItf)
pAGItf
  = let alg                 =   (undefined,sem_Expr_App,sem_Expr_AppTop)
        pAGItf              =   sem_AGItf_AGItf <$> pDecls' pDeclGlob
        pDecls'  pD         =   pFoldr  (sem_Decls_Cons,sem_Decls_Nil) pD
        pDecls1' pD         =   pFoldr1 (sem_Decls_Cons,sem_Decls_Nil) pD
        pDeclRule           =   sem_Decl_Rule          <$> pKeySPos "rule"      <*> (pNm <|> Nm <$> pString)
                                                       <*> pMb (pKey ":" *> pNm)
                                                       <*> pMbViewSel
                                                       <*> pMbString
                                                       <*  pKey "="             <*> (pDeclRuleDflt <|> pDecls1' pDeclRulView)
        pDeclRuleDflt       =   (\(pre,post) -> sem_Decls_Cons (sem_Decl_RulView nmNone pre post) sem_Decls_Nil)
                                                       <$> pRExprsPrePost
        pDeclRulView        =   (\v (pre,post) -> sem_Decl_RulView v pre post)
                                                       <$  pKey "view"          <*> pNmVw
                                                       <*  pKey "="             <*> pRExprsPrePost
        pRExprsPrePost      =   (,) <$> pRExprs <* pKey "-" <*> pRExprs
        pDeclScm            =   sem_Decl_ScmView       <$  pKey "view"          <*> pNmVw
                                                       <*  pKey "="             <*> pDeclsScmView
        pDeclScmDflt        =   (\v -> sem_Decls_Cons (sem_Decl_ScmView nmNone v) sem_Decls_Nil)
                                                       <$> pDeclsScmView
        pDeclScmView        =   pKeySPos "judgespec"
                                <**> (   (\e p -> sem_Decl_ShpJudge p FmSpec e) <$> pExprLF
                                     )
                            <|> pKeySPos "judgeshape" <**> pShp pFmKd3WithDflt pFmKd3
                            <|> pKeySPos "judgeuse"   <**> pShp pFmKd2         pFmKd2
                            <|> sem_Decl_Attr          <$  (pKey "hole" <|> pKey "holes")
                                                       <*  pKey "["             <*> pAttrIntros
                                                       <*  pKey "|"             <*> pAttrIntros
                                                       <*  pKey "|"             <*> pAttrIntros
                                                       <*  pKey "]"
                            <|> sem_Decl_Explain       <$  pKey "explain" <*> pMb (pNm <* pKey "=") <*> pParens pExprExplain
                            where pShp pFmAdd pFmDel
                                    = (   (\k e p -> sem_Decl_ShpJudge p k e) <$> pFmAdd <*> pExprLF
                                      <|> (flip sem_Decl_ShpDel) <$ pKey "-" <*> pList1 pFmDel
                                      )
        pDeclsScmView       =   pDecls1' pDeclScmView
        pDeclGlob           =   pDeclGlobScheme
                            <|> sem_Decl_Fmt           <$  (pKey "format" <|> pKey "rewrite")
                                                       <*> pFmKd2WithDflt
                                                       <*> pAtIO
                                                       <*> pExpr
                                                       <*  pKey "="             <*> pExpr
                            <|> sem_Decl_Rules         <$  (pKey "rules" <|> pKey "ruleset")
                                                       <*> pNm
                                                       <*  pKey "scheme" <*> pNm <*> pMbViewSel <*> pString
                                                       <*  pKey "=" <*> pDecls' pDeclRule
                            <|> sem_Decl_RulesGroup    <$  pKey "rulesgroup"
                                                       <*> pNm
                                                       <*  pKey "scheme" <*> pNm <*> pMbViewSel <*> pString
                                                       <*  pKey "="             <*> pList1 ((,) <$ pKey "rule" <*> pNm <*> pNm)
                            <|> sem_Decl_ViewHierarchy <$  pKey "viewhierarchy"
                                                       <*  pKey "="             <*> pList1Sep pComma (pList1Sep (pKey "<") pNmVw)
                            <|> sem_Decl_Preamble      <$  pKey "preamble"      <*> pFmKd2WithDflt <*> pString
                            <|> sem_Decl_Extern        <$  (pKey "extern" <|> pKey "external")
                                                       <*> pList1 pNm
        pDeclGlobScheme     =   (ScJudge <$ pKey "scheme" <|> ScRelation <$ pKey "relation")
                                <**> (pNm
                                     <**> (   (\d (ms,ds) n k -> sem_Decl_SchemeDeriv k n d ms ds)
                                              <$ pKey ":" <*> pScDeriv <*> pTl2
                                          <|> (\(ms,ds) n k -> sem_Decl_Scheme k n ms ds)
                                              <$> pTl1
                                          )
                                     )
                            where pTl1 = (,) <$> pMbString <*> pDs
                                  pDs = pKey "=" *> (pDeclScmDflt <|> pDecls1' pDeclScm)
                                  pTl2 = (,) <$> pMbString <*> (pMaybe sem_Decls_Nil id pDs)
        pScDeriv            =   ScList <$ pKey "[" <*> pNm <* pKey "]"
        pMbViewSel          =   pMaybe ViewSel_All id (pKey "viewsel" *> pViewSel)
{-
        pViewSel            =   pV <??> ((flip sem_ViewSel_Range) <$ pKey "-" <*> pV)
                            where pV =   sem_ViewSel_View <$> pNmVw <|> sem_ViewSel_All <$ pKey "*"
                                     <|> pParens (sem_ViewSel_Renamed <$> (foldr1 nmStrApd <$> pList1 pNmVw) <* pKey "=" <*> pNmVw)
        pViewSels           =   pFoldrSep (sem_ViewSels_Cons,sem_ViewSels_Nil) pComma pViewSel
        pNmSel              =   sem_NmSel_All <$ pKey "*" <|> sem_NmSel_Nms <$> pList pNm
        pRlSel              =   sem_RlSel_Sel <$> pParens pViewSels <* pKey "." <*> pParens pNmSel <* pKey "." <*> pParens pNmSel
-}
        pFmKd2WithDflt      =   pMaybe FmAll id pFmKd2
        pFmKd3WithDflt      =   pMaybe FmAll id pFmKd3
        pFmKd2              =   FmTeX <$ pKey "tex" <|> FmAG <$ pKey "ag"
        pFmKd3              =   FmSpec <$ pKey "spec" <|> pFmKd2
        pAtIO               =   pMaybe AtInOut id (AtIn <$ pKey "def" <|> AtOut <$ pKey "use")
        -- pNmStr              =   pVarid <|> pConid
        -- pNmStrI             =   pNmStr <|> pInteger
        pNmBase             =   Nm <$> pNmStr
{-
        pNm' pN             =   pNmSym (pN,pSymStr)
        pNm' pN             =   (Nm <$> pN) <**> pNmDot
        pNm                 =   pNm' pNmStr
-}
        pNmC                =   Nm <$> pConid
        pNmV                =   Nm <$> pVarid
{-
        pNmVw               =   pNm' pNmStrI
        pSymStr             =   pSymEscStr (keywordsOpsEsc,keywordsOpsParenEsc)
        pSymStr             =   pVarsym <|> pConsym
                            <|> pAnyKey pKey keywordsOpsEsc
                            <|> pKey "`"  *> (   (\n -> "`" ++ n ++ "`") <$> pNmStr
                                             <|> concat <$> pList1 (pAnyKey pKey keywordsOpsParenEsc)
                                             )
                                         <*  pKey "`"
-}
        pSym                =   Nm <$> pSymStr
        pRExprs             =   pFoldr (sem_RExprs_Cons,sem_RExprs_Nil) pRExpr
        pRExpr              =   pKeySPos "judge"
                                <**> ((\rn n e p -> sem_RExpr_Judge p rn n e) <$> pMbRNm <*> pNm <*> pRExprEqn
                                     <|> (flip sem_RExpr_Del) <$ pKey "-" <*> pList1 pNm
                                     )
        pRExprEqn           =   sem_RExprEqn_Attrs <$> pAttrEqns
                            <|> sem_RExprEqn_Expr  <$  pKey "=" <*> pExpr
        pExpr               =   pExprApp
                                <??> ((\es e1 -> let (op,e2) = foldr (\(op,e1) (f2,e2) -> (op,f2 e1 e2)) (const,undefined) es
                                                 in sem_Expr_AppTop (op e1 e2)
                                      )
                                      <$> pList1 ((,) <$> pOp <*> pExprApp)
                                     )
                            where pOp = (\s ss -> sem_Expr_Op s (ss (sem_Expr_Var s))) <$> pSym <*> pExprDot
        pExprExplain        =   foldl1 sem_Expr_SP <$> pList1 pW
                            where pW =   pExprAtom (Nm <$> (pNmStr <|> pSymStr <|> pAnyKey pKey (keywordsOpsExplainEsc ++ keywordsText)))
                                     <|> sem_Expr_Expr <$ pKey "|" <*> pExpr <* pKey "|"
                                     <|> sem_Expr_Paren <$> pParens pExprExplain
        pExprLF             =   pChainr (sem_Expr_LF <$ pKey "|") pExpr
        pExprLFSuffix nl    =   (\es e -> sem_Expr_LF e (foldr1 sem_Expr_LF es)) <$> pList1 (pKey nl *> pExpr)
        pExprApp            =   mkApp alg <$> pList1 pExprBase <|> pSucceed sem_Expr_Empty
        pExprAtom pN        =   sem_Expr_Var     <$> pN
                            <|> sem_Expr_Int     <$> pInteger
                            <|> sem_Expr_StrAsIs <$> pString
        pExprBase' pN       =   pParens (pExprInParen <|> pExprSpecial) <**> pExprDot
                            <|> sem_Expr_Uniq    <$  pKey "unique"
                            <|> pExprAtom pN
        pExprBase           =   pExprBase' pNm
        pExprInParen        =   pExpr <**> (   flip sem_Expr_Cnstr <$  pKey "|" <*> pECnstr
                                           <|> pExprLFSuffix "-"
                                           <|> pSucceed sem_Expr_Paren
                                           )
        pExprSpecial        =   (sem_Expr_StrAsIs . concat)     <$> pList1 (pAnyKey pKey (keywordsOpsParenEsc ++ keywordsTextEscapable))
                            <|> sem_Expr_Named                  <$> pNm <* pKey "=" <*> pExpr
                            <|> sem_Expr_StrText                <$  pKey "text" <*> pString
                            <|> sem_Expr_Retain                 <$  pKey "retain" <*> pExpr
                            <|> sem_Expr_ChildOrder             <$  pKey "node" <*> pInt <* pKey "=" <*> pExpr
        pExprDot            =   pSel (sem_Expr_SelTop,sem_Expr_Sel,sem_MbExpr_Just) (pE,pMbE)
                            where pE   = pExprBase' pNmBase
                                  pMbE = sem_MbExpr_Just <$> pE <|> pSucceed sem_MbExpr_Nothing
{-
        pNmDot              =   pNmDotSym pSymStr
        pNmDot              =   pSel (id,NmSel,Just) (pN,pMb pN)
                            where pN = pNmStrI <|> pSymStr
        pSel alg ps         =   pSel1 alg ps <|> pSucceed id
        pSel1 (top,sel,jst) (pE,pMbE)
                            =   (\ss s -> \e -> top (sel (ss e) (jst s))) <$> pDots <*> pE
                            where pSel' = flip sel <$> pMbE
                                  pDots = pChainr_ng ((\s -> \_ r -> \e -> r (s e)) <$> pSel') (id <$ pKey ".")
-}
        pECnstr             =   sem_ECnstr_Ty  <$> pList1Sep pComma pNmC
                            <|> sem_ECnstr_Var <$> pNmV
        pMbRNm              =   pMb (pNm <* pKey ":")
        pMbString           =   pMb pString
{-
        pAnyKey             =   foldr1 (<|>) . map pKey
-}
        pAttrIntros         =   pFoldrSep (sem_AttrIntros_Cons,sem_AttrIntros_Nil) pComma pAttrIntro
        pAttrIntro          =   sem_AttrIntro_Intro <$> pList pAttrProp <*> pNm <* pKey ":" <*> pNmC
        pAttrEqns           =   pFoldr (sem_AttrEqns_Cons,sem_AttrEqns_Nil) (pKey "|" *> pAttrEqn)
        pAttrEqn            =   sem_AttrEqn_Eqn <$>              pNm <* pKey "=" <*> pExpr
                            <|> sem_AttrEqn_Del <$  pKey "-" <*> pNm
        pAttrProp           =   foldr1 (<|>) [ v <$ pKey n | (n,v) <- Map.toList propsMp ]
{-
        pMb                 =   pMaybe Nothing Just
-}
        pKeySPos k          =   (\p -> (k,p)) <$> pKeyPos k
        pInt                =   read <$> pInteger
     in (pAGItf)



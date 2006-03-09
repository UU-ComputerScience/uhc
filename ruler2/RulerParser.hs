-------------------------------------------------------------------------
-- Ruler parser
-------------------------------------------------------------------------

module RulerParser
  ( pAGItf
  )
  where

import qualified Data.Set as Set
import qualified Data.Map as Map
import UU.Parsing
-- import UUTest.Parsing.Offside
import UU.Parsing.Offside
import UU.Scanner.Position( initPos, Pos, Position(..) )
import UU.Scanner.GenToken
import RulerScanner
import ParseUtils
import ScanUtils
import NmParser
import SelParser
import KeywParser
import ViewSel
import ViewSelParser
import Common
import RulerAbsSyn1

-------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------

type RulerParser ep
  = (IsParser (OffsideParser i o Token p) Token,InputState i Token p, OutputState o, Position p)
      => OffsideParser i o Token p ep

type MkConAppAlg t = (String -> t,t -> t -> t,t -> t)

mkApp :: MkConAppAlg t -> [t] -> t
mkApp (_,app,top) ts
  = case ts of
      [t]  -> t
      _    -> top t
  where t = foldl1 app ts

pAGItf :: RulerParser AGItf
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
-}
        pTopDecls'  pD      =   pLay pD
        pDecls'     pD      =   pList pD
        pDecls1'    pD      =   pList1 pD
        pDeclRule           =   (\(n,p) mn s mag d -> Decl_Rule p n mn s mag d)
                                                       <$  pKeySPos "rule"      <*> pNmSPos
                                                       <*> pMb (pKey ":" *> pNm)
                                                       <*> pMbViewSel
                                                       <*> pMbString
                                                       <*  pKey "="             <*> (pDeclRuleDflt <|> pDecls1' pDeclRulView)
        pDeclRuleDflt       =   (\(pre,post) -> [Decl_RulView nmNone pre post])
                                                       <$> pRExprsPrePost
        pDeclRulView        =   (\v (pre,post) -> Decl_RulView v pre post)
                                                       <$  pKey "view"          <*> pNmVw
                                                       <*  pKey "="             <*> pRExprsPrePost
        pRExprsPrePost      =   (,) <$> pRExprs <* sep <*> pRExprs
                            <|> pKey "judges" *> pLay2Sep pOParen (pKey "|") pCParen sep pRExprBase
                            where pRExprs    = pList pRExpr
                                  -- pRExprsLay = pLay  pRExprBase
                                  sep = pKey "-" <|> pKey "---"
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
                                                       <*  pKey "["             <*> pAttrIntros
                                                       <*  pKey "|"             <*> pAttrIntros
                                                       <*  pKey "|"             <*> pAttrIntros
                                                       <*  pKey "]"
                            <|> Decl_Explain           <$  pKey "explain" <*> pMb (pNm <* pKey "=") <*> pParens pExprExplain
                            where pShp pFmAdd pFmDel
                                    = (   (\k e p -> Decl_ShpJudge p k e) <$> pFmAdd <*> pExprLF
                                      <|> (flip Decl_ShpDel) <$ pKey "-" <*> pList1 pFmDel
                                      )
        pDeclsScmView       =   pDecls1' pDeclScmView
        pDeclGlob           =   pDeclGlobScheme
                            <|> Decl_Fmt               <$  (pKey "format" <|> pKey "rewrite")
                                                       <*> pFmKd2WithDflt
                                                       <*> pAtIO
                                                       <*> pExpr
                                                       <*  pKey "="             <*> pExpr
                            <|> Decl_Rules             <$  (pKey "rules" <|> pKey "ruleset")
                                                       <*> pNm
                                                       <*  pKey "scheme" <*> pNm <*> pMbViewSel <*> pString
                                                       <*  pKey "=" <*> pDecls' pDeclRule
                            <|> Decl_RulesGroup       <$  pKey "rulesgroup"
                                                       <*> pNm
                                                       <*  pKey "scheme" <*> pNm <*> pMbViewSel <*> pString
                                                       <*  pKey "="             <*> pList1 ((,) <$ pKey "rule" <*> pNm <*> pNm)
                            <|> Decl_ViewHierarchy     <$  pKey "viewhierarchy"
                                                       <*  pKey "="             <*> pList1Sep pComma (pList1Sep (pKey "<") pNmVw)
                            <|> Decl_Preamble          <$  pKey "preamble"      <*> pFmKd2WithDflt <*> pString
                            <|> Decl_Extern            <$  (pKey "extern" <|> pKey "external")
                                                       <*> pList1 pNm
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
        pMbViewSel          =   pMaybe ViewSel_All id (pKey "viewsel" *> pViewSel)
        pFmKd2WithDflt      =   pMaybe FmAll id pFmKd2
        pFmKd3WithDflt      =   pMaybe FmAll id pFmKd3
        pFmKd2              =   FmTeX <$ pKey "tex" <|> FmAG <$ pKey "ag"
        pFmKd3              =   FmSpec <$ pKey "spec" <|> pFmKd2
        pAtIO               =   pMaybe AtInOut id (AtIn <$ pKey "def" <|> AtOut <$ pKey "use")
        pNmBase             =   Nm <$> pNmStr
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
        pExprBase           =   pExprBase' pNm
        pExprInParen        =   pExpr <**> (   flip Expr_Cnstr <$  pKey "|" <*> pECnstr
                                           <|> pExprLFSuffix "-"
                                           <|> pSucceed Expr_Paren
                                           )
        pExprSpecial        =   (Expr_StrAsIs . concat)     <$> pList1 (pAnyKey pKey (keywordsOpsParenEsc ++ keywordsTextEscapable))
                            <|> Expr_Named                  <$> pNm <* pKey "=" <*> pExpr
                            <|> Expr_StrText                <$  pKey "text" <*> pString
                            <|> Expr_Retain                 <$  pKey "retain" <*> pExpr
                            <|> Expr_ChildOrder             <$  pKey "node" <*> pInt <* pKey "=" <*> pExpr
        pExprDot            =   pSel (Expr_SelTop,Expr_Sel,Just) (pE,pMbE)
                            where pE   = pExprBase' pNmBase
                                  pMbE = Just <$> pE <|> pSucceed Nothing
        pECnstr             =   ECnstr_Ty  <$> pList1Sep pComma pNmC
                            <|> ECnstr_Var <$> pNmV
        pMbString           =   pMb pString
        pAttrIntros         =   pListSep pComma pAttrIntro
        pAttrIntro          =   AttrIntro_Intro <$> pList pAttrProp <*> pNm <* pKey ":" <*> pNmC
        pAttrEqns           =   pList (pKey "|" *> pAttrEqn)
        pAttrEqn            =   AttrEqn_Eqn <$>              pNm <* pKey "=" <*> pExpr
                            <|> AttrEqn_Del <$  pKey "-" <*> pNm
        pAttrProp           =   foldr1 (<|>) [ v <$ pKey n | (n,v) <- Map.toList propsMp ]
        pInt                =   read <$> pInteger
     in (pAGItf)



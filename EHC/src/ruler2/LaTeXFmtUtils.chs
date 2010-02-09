%%[1 hs module (LaTeXFmtUtils)
%%]

%%[1 hs export (ppDest, atDstFillLen, atLhs2texDist, NeedParCtxt(..), exprNeedPar, ppExprMbEmpty)
%%]

%%[1 hs export (strLhs2TeXSafe, nmLhs2TeXSafe, mkMBox, mkInLhs2Tex, ensureTeXMath, switchLaTeXLhs, switchLaTeXLhs')
%%]

%%[1 hs export (mkCmdNmDef, mkCmdNmUse, ppNmLaTeX, ppSelLaTeX, ppWrapShuffle)
%%]

%%[1 hs import (EH.Util.Utils, EH.Util.Pretty, EH.Util.PrettyUtils, Common, Expr.Expr)
%%]

%%[1 hs

-------------------------------------------------------------------------
-- Formatting for destination names in AG rules
-------------------------------------------------------------------------

atLhs2texDist   = 2
atLhs2texFill   = strWhite atLhs2texDist
atDstFill1      = atLhs2texFill ++ "." ++ atLhs2texFill
atDstFill2      = atLhs2texFill
atDstFillLen    = length atDstFill1 + length atDstFill2

ppDest :: String -> Bool -> Maybe (Int,Int) -> Maybe String -> String -> String -> Nm -> PP_Doc
ppDest k isDest mbDstWd mbPrevNdStr dstPre srcPre n
  = tr $ if isDest
         then case mbDstWd of
                Just (ndW,atW)
                  -> strPad (if maybe False (==dstPre) mbPrevNdStr then "" else dstPre) ndW
                     >|< atDstFill1 >|< strPad (show n) atW >|< atDstFill2
                _ -> mkPre dstPre n
         else "@" >|< mkPre srcPre n
  where mkPre s n = if null s then pp n else s >|< "." >|< pp n
        tr x = x -- (pp k >|< ppCurlys x)

-------------------------------------------------------------------------
-- Need for parenthesis
-------------------------------------------------------------------------

data NeedParCtxt
  = ParCtxtAppL | ParCtxtAppR | ParCtxtOpL | ParCtxtOpR | ParCtxtOther
  deriving Eq

exprNeedPar :: NeedParCtxt -> Nm -> Expr -> (PP_Doc -> PP_Doc)
exprNeedPar ctxt opNm e
  = case e of
      Expr_Paren e
        -> case (t e,ctxt) of
             (Expr_Op    _ _ _ _,ParCtxtAppL) -> ppParens
             (Expr_Op    _ _ _ _,ParCtxtAppR) -> ppParens
             (Expr_Op    _ _ _ _,ParCtxtOpL ) -> ppParens
             (Expr_Op    _ _ _ _,ParCtxtOpR ) -> ppParens
             (Expr_Op    n _ _ _,_          )
               | n == nmComma -> ppParens
             (Expr_App   _ _    ,ParCtxtAppR) -> ppParens
             (Expr_Paren _      ,_          ) -> ppParens
             _ | opNm == nmSp1 -> ppParens
               | otherwise     -> id
      _ -> id
  where t = exprStrip StripBasicNoPar

-------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------

ppExprMbEmpty :: Expr -> (PP_Doc -> PP_Doc) -> PP_Doc -> PP_Doc
ppExprMbEmpty (Expr_Empty) _ p = p
ppExprMbEmpty _            f p = f p

-------------------------------------------------------------------------
-- LaTeX
-------------------------------------------------------------------------

{-
mkLaTeXNm :: String -> String
mkLaTeXNm = map (\c -> if isAlphaNum c then c else '-')
-}

strLhs2TeXSafe :: String -> String
strLhs2TeXSafe = concat . map (\c -> if c == '|' then "||" else [c])

nmLhs2TeXSafe :: Nm -> Nm
nmLhs2TeXSafe = fmap strLhs2TeXSafe

mkMBox :: PP a => a -> PP_Doc
mkMBox p = "\\;\\mbox" >|< ppCurly p

{-
mkRuleNm :: String -> String -> PP_Doc
mkRuleNm r v = "\\textsc" >|< ppCurly (mkLaTeXNm r) >|< (if null v then empty else "_" >|< ppCurly v)
-}

mkVerb :: PP_Doc -> PP_Doc
mkVerb p = ppPacked "@" "@" p

switchLaTeXLhs :: PP a => a -> PP_Doc
switchLaTeXLhs p = ppVBar (" " >|< p >|< " ")

switchLaTeXLhs' :: PP a => a -> PP_Doc
switchLaTeXLhs' = ppPacked "| " " |"

mkInLhs2Tex :: PP_Doc -> PP_Doc
mkInLhs2Tex p = ppVBar (p >|< " ")

ensureTeXMath :: PP_Doc -> PP_Doc
ensureTeXMath = mkTexCmdUse "ensuremath"

mkCmdNmDef :: (PP a, PP b) => a -> b -> PP_Doc
mkCmdNmDef = mkTexCmdDef "rulerCmdDef"

mkCmdNmUse :: PP a => a -> PP_Doc
mkCmdNmUse = mkTexCmdUse "rulerCmdUse"

ppNmLaTeX :: Nm -> PP_Doc
ppNmLaTeX n
  = ppSelLaTeX ((== strOverl),(== strOverVec)) (fromJust base) (map (fmap (\s -> (s,pp s))) sels)
  where (base:sels) = nmToMbL n

ppSelLaTeX :: (PP base,PP sel) => (sel -> Bool,sel -> Bool) -> base -> [Maybe (sel,PP_Doc)] -> PP_Doc
ppSelLaTeX (isOverl,isOverVec) base sels
  = p
  where sw = (" " >|<) . switchLaTeXLhs
        over n s
          = if isOverl n
            then sw (text "\\overline{")
            else if isOverVec n
            then sw (text "\\vec{")
            else sw ("\\stackrel{" >|< sw s >|< "}{")
        p = case (pp base,sels) of
                 (x,[Nothing,Nothing,Just (n3,s3)])
                   -> over n3 s3
                      >|< x
                      >|< sw (text "}")
                 (x,[Nothing,Just (_,s2),Just (n3,s3)])
                   -> over n3 s3
                      >|< x
                      >|< sw ("^{" >|< sw s2 >|< "}}")
                 (x,[Just (_,s1),Nothing,Just (n3,s3)])
                   -> over n3 s3
                      >|< x
                      >|< sw ("_{" >|< sw s1 >|< "}}")
                 (x,[Just (_,s1),Just (_,s2),Just (n3,s3)])
                   -> over n3 s3
                      >|< x
                      >|< sw ("_{" >|< sw s1 >|< "}^{" >|< sw s2 >|< "}}")
                 (x,(Just (_,s1):Just (_,s2):_))
                   -> x >|< sw ("_{" >|< sw s1 >|< "}^{" >|< sw s2 >|< "}")
                 (x,(Nothing:Just (_,s2):_))
                   -> x >|< sw ("^{" >|< sw s2 >|< "}")
                 (x,(Just (_,s1):_))
                   -> x >|< sw ("_{" >|< sw s1 >|< "}")
                 (x,_)
                   -> x

ppWrapShuffle :: Nm -> PP_Doc -> PP_Doc
ppWrapShuffle n x
  = "%%[" >|< n >-< x >-< "%%]"

%%]

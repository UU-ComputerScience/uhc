-- $Id: EHTyFitsIn.chs 214 2005-05-28 17:52:29Z atze $

module RulerUtils where

import IO
import Data.Maybe
import Data.Char
import Data.List
-- import Data.Graph
import qualified Data.Set as Set
import qualified Data.Map as Map
import FPath
import Utils
import Nm
import PPUtils
import Common
import UU.Pretty
import qualified UU.DData.Scc as Scc
-- import UU.Scanner.Position( noPos, Pos, Position(..) )
-- import UU.Scanner.GenToken
-- import UU.Parsing

import Debug.Trace

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
  = ppSelLaTeX (== strVec) (fromJust base) (map (fmap (\s -> (s,pp s))) sels)
  where (base:sels) = nmToMbL n

ppSelLaTeX :: (PP base,PP sel) => (sel -> Bool) -> base -> [Maybe (sel,PP_Doc)] -> PP_Doc
ppSelLaTeX isVec base sels
  = p
  where sw = (" " >|<) . switchLaTeXLhs
        over n s = if isVec n then sw (text "\\overline{") else sw ("\\stackrel{" >|< sw s >|< "}{")
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



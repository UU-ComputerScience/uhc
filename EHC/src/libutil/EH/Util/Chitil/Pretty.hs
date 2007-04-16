-------------------------------------------------------------------------
-- Subset of UU.Pretty interface to Olaf Chitil's pretty printing combinators
-------------------------------------------------------------------------

module EH.Util.Chitil.Pretty
  ( PP_Doc, PP(..)
  , disp
  
  , (>|<), (>-<)
  , (>#<)
  , hlist, vlist
  , indent

{-
  , pp_wrap, pp_quotes, pp_doubleQuotes, pp_parens, pp_brackets, pp_braces
  , ppPacked, ppParens, ppBrackets, ppBraces, ppCurlys
-}

  -- re-exported:
  , empty, text
  )
  where

import EH.Util.Chitil.FPretty

-------------------------------------------------------------------------
-- Doc structure
-------------------------------------------------------------------------

type PP_Doc = Doc

-------------------------------------------------------------------------
-- Basic combinators
-------------------------------------------------------------------------

infixr 3 >|<, >#<
infixr 2 >-< 

(>|<) :: (PP a, PP b) => a -> b -> PP_Doc
l >|< r = pp l <> pp r

(>-<) :: (PP a, PP b) => a -> b -> PP_Doc
l >-< r = pp l <$$> pp r

(>#<) :: (PP a, PP b) => a -> b -> PP_Doc
l >#< r  =  l >|< " " >|< r

indent :: PP a => Int -> a -> PP_Doc
indent i x = hang i $ pp x 

-------------------------------------------------------------------------
-- Derived combinators
-------------------------------------------------------------------------

hlist, vlist :: PP a => [a] -> PP_Doc
vlist [] = empty
vlist as = foldr  (>-<) empty as
hlist [] = empty
hlist as = foldr  (>|<) empty as

hv :: PP a => [a] -> PP_Doc
hv [] = empty
hv as = fillCat $ map pp as

-------------------------------------------------------------------------
-- Printing open/close pairs
-------------------------------------------------------------------------

{-
pp_wrap :: PP a =>  a -> a -> PP_Doc -> PP_Doc
pp_wrap op cl p = op >|< (p >|< cl)

pp_quotes       = pp_wrap '`' '\''
pp_doubleQuotes = pp_wrap '"' '"'
pp_parens       = pp_wrap '(' ')'
pp_brackets     = pp_wrap '[' ']'
pp_braces       = pp_wrap '{' '}'

ppPacked :: (PP a,PP b) =>  a -> a -> b -> PP_Doc
ppPacked o c x = pp_wrap o c $ pp x

ppParens, ppBrackets, ppBraces, ppCurlys :: PP b => b -> PP_Doc
ppParens   x = pp_parens $ pp x
ppBrackets x = pp_brackets $ pp x
ppBraces   x = pp_braces $ pp x
ppCurlys     = ppBraces
-}

-------------------------------------------------------------------------
-- PP class
-------------------------------------------------------------------------

class Show a => PP a where
  pp     :: a   -> PP_Doc
  pp       = text . show

  ppList :: [a] -> PP_Doc
  ppList as = hlist as

instance PP PP_Doc where
  pp     = id

instance PP Char where
  pp c   = text [c]
  ppList = text

instance PP a => PP [a] where
  pp = ppList

instance Show PP_Doc where
  show p = disp p 200 ""

instance PP Int where
  pp = text . show

instance PP Float where
  pp = text . show

-------------------------------------------------------------------------
-- Rendering
-------------------------------------------------------------------------

disp  ::  PP_Doc -> Int -> ShowS
disp d w s = pretty w d ++ s

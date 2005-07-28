module Utils where

import UU.Pretty
import Data.Char

-------------------------------------------------------------------------
-- Utils for tools
-------------------------------------------------------------------------

mkTexCmdDef :: (PP a, PP b) => String -> a -> b -> PP_Doc
mkTexCmdDef cmd nm def = "\\" >|< cmd >|< "{" >|< pp nm >|< "}{%" >-< pp def >-< "}"

mkTexCmdUse :: PP a => String -> a -> PP_Doc
mkTexCmdUse cmd nm = "\\" >|< cmd >|< "{" >|< pp nm >|< "}"

mkTexCmdUse' :: PP a => String -> a -> PP_Doc
mkTexCmdUse' cmd nm = mkTexCmdUse cmd nm >|< "%"

-------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------

hdAndTl :: [a] -> (a,[a])
hdAndTl (x:xs) = (x,xs)

maybeHd :: r -> (a -> r) -> [a] -> r
maybeHd n f l = if null l then n else f (head l)

strWhite :: Int -> String
strWhite sz = replicate sz ' '

strPad :: String -> Int -> String
strPad s sz = s ++ strWhite (sz - length s)

strCapitalize :: String -> String
strCapitalize s
  = case s of
      (c:cs) -> toUpper c : cs
      _      -> s

panic m = error ("panic: " ++ m)


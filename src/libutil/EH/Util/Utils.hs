module EH.Util.Utils where

import UU.Pretty
import Data.Char
import Data.List
import Debug.Trace

-------------------------------------------------------------------------
-- Utils for tools
-------------------------------------------------------------------------

mkTexCmdDef :: (PP cmd, PP a, PP b) => cmd -> a -> b -> PP_Doc
mkTexCmdDef cmd nm def = "\\" >|< pp cmd >|< "{" >|< pp nm >|< "}{%" >-< pp def >-< "}"

mkTexCmdUse :: (PP cmd, PP a) => cmd -> a -> PP_Doc
mkTexCmdUse cmd nm = "\\" >|< pp cmd >|< "{" >|< pp nm >|< "}"

mkTexCmdUse' :: (PP cmd, PP a) => cmd -> a -> PP_Doc
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

wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy p l
  = w l
  where w [] = []
        w l  = let (l',ls') = break p l
               in  l' : case ls' of []       -> []
                                    (_:ls'') -> w ls''

panic m = error ("panic: " ++ m)

-------------------------------------------------------------------------
-- group/sort combi's
-------------------------------------------------------------------------

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = sortByOn compare

sortByOn :: (b -> b -> Ordering) -> (a -> b) -> [a] -> [a]
sortByOn cmp sel = sortBy (\e1 e2 -> sel e1 `cmp` sel e2)

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn sel = groupBy (\e1 e2 -> sel e1 == sel e2)

groupSortOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortOn sel = groupOn sel . sortOn sel

groupByOn :: (b -> b -> Bool) -> (a -> b) -> [a] -> [[a]]
groupByOn eq sel = groupBy (\e1 e2 -> sel e1 `eq` sel e2)

groupSortByOn :: (b -> b -> Ordering) -> (a -> b) -> [a] -> [[a]]
groupSortByOn cmp sel = groupByOn (\e1 e2 -> cmp e1 e2 == EQ) sel . sortByOn cmp sel

-------------------------------------------------------------------------
-- Maybe
-------------------------------------------------------------------------

panicJust :: String -> Maybe a -> a
panicJust m = maybe (panic m) id

-------------------------------------------------------------------------
-- Tracing
-------------------------------------------------------------------------

tr m s v = trace (m ++ show s) v
trp m s v = trace (m ++ "\n" ++ disp (m >|< ":" >#< s) 1000 "") v


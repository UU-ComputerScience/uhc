{-| Module      :  OneLiner
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Utils.OneLiner(OneLineTree(..), showOneLine) where

import List

data OneLineTree 
    = OneLineNode [OneLineTree]
    | OneLineText String

collapseString :: String
collapseString = "..."

collapseWidth :: Int
collapseWidth  = length collapseString

showOneLine :: Int -> OneLineTree -> String
showOneLine width tree = 
    case tree of
        OneLineText s -> s
        OneLineNode ts -> oneLine True width ts
        
oneLine :: Bool -> Int -> [OneLineTree] -> String
oneLine toplevel width trees
    | not toplevel &&  -- do not collapse at toplevel
        thisLevel > width -- collapse if not even texts can be displayed
          = collapseString
    | not toplevel &&
        minSize trees > collapseWidth && 
            minSize trees > width -- only collapse if that makes things better
          = collapseString
    | otherwise = concatMap processTree (zip childWidths trees)
    where
        thisLevel = countThisLevel trees
        childSizes = map (\t -> case t of { OneLineText _ -> 0; OneLineNode _ -> maxSize [t]} ) trees
        numberedChildren = zip [0..] childSizes
        childWidths = map snd (sort (distribute (width - thisLevel) numberedChildren))
        
        processTree (_         , OneLineText s) = s
        processTree (childWidth, OneLineNode ts) = oneLine False childWidth ts

maxSize :: [OneLineTree] -> Int
maxSize ts =
    let
        sizeOne :: OneLineTree -> Int
        sizeOne (OneLineText s)     = length s
        sizeOne (OneLineNode subTs) = maxSize subTs
    in
        sum (map sizeOne ts)

minSize :: [OneLineTree] -> Int
minSize ts =
    let
        sizeOne :: OneLineTree -> Int
        sizeOne (OneLineText s) = length s
        sizeOne (OneLineNode subTs) = min (minSize subTs) collapseWidth
    in
        sum (map sizeOne ts)

countThisLevel :: [OneLineTree] -> Int
countThisLevel ts = 
    sum [ length s | OneLineText s <- ts ]


distribute :: Int -> [(Int, Int)] -> [(Int, Int)]
distribute width children 
    | null smallChildren = [ (nr, widthPerChild) | (nr, _) <- children ]
    | otherwise =
        smallChildren ++ distribute leftOvers bigChildren
    where
        widthPerChild = width `div` length children
        (smallChildren, bigChildren) =
            partition (\(_, need) -> need <= widthPerChild) children
        leftOvers = width - sum (map snd smallChildren)
        

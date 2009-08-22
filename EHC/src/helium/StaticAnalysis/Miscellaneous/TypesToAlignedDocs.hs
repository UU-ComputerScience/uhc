{-| Module      :  TypesToAlignedDocs
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
    
    Functions to align and show types.
-}

module Helium.StaticAnalysis.Miscellaneous.TypesToAlignedDocs (qualifiedTypesToAlignedDocs, typesToAlignedDocs) where

import List     ( (\\), union, transpose )
import Top.Types
import Lvm.Common.PPrint as PPrint

qualifiedTypesToAlignedDocs :: [QType] -> [PPrint.Doc]
qualifiedTypesToAlignedDocs qtps = 
   let (contexts, types) = unzip (map split qtps)
       docContexts = map text . sameLengthRight . map showContext $ contexts
       docTypes    = typesToAlignedDocs types
   in if null (concat contexts)
         then docTypes
         else zipWith (<>) docContexts docTypes

typesToAlignedDocs :: Tps -> [PPrint.Doc]
typesToAlignedDocs []  = []
typesToAlignedDocs tps

   | allFunctionType
     = let functionSpines = map functionSpine tps
           shortestSpine  = minimum (map (length . fst) functionSpines)
           tupleSpines    = map partOfSpine functionSpines
           partOfSpine (ts, t) = let (xs, ys) = splitAt shortestSpine ts 
                                 in (xs, foldr (.->.) t ys)
           (left, right)  = unzip tupleSpines
           docsLeft       = recs (<1) left
           docsRight      = rec  (const False) right
       in map funDocs (zipWith (\xs x -> xs++[x]) docsLeft docsRight)
  
   | allVariable
     = map PPrint.text (sameLength [ 'v' : show i | (TVar i, _) <- spines])

   | allConstant
     = map PPrint.text (sameLength [ s | (TCon s, _) <- spines])
   
   | allListType
     = map PPrint.squares (rec (const False) (map (head . snd) spines))

   | allSameTuple
     = map tupleDocs (recs (const False) (map snd spines))   
     
   | allSameConstructor 
     = map appDocs (recs (<2) [ x:xs | (x, xs) <- spines ])
           
   | otherwise 
     = map PPrint.text $ sameLength $ map show tps
   
   where spines = map leftSpine tps
         allSameConstructor = all isTCon (map fst spines)
                           && allEqual [ s | (TCon s, _) <- spines ]
                           && allEqual [ length xs | (_, xs) <- spines ]
         allSameTuple       = all isTCon (map fst spines)
                           && all isTupleConstructor [ s | (TCon s, _) <- spines ]
                           && allEqual [ s | (TCon s, _) <- spines ]
                           && allEqual [ length xs | (_, xs) <- spines ]      
         allListType        = all isTCon (map fst spines)
                           && all ("[]"==) [ s | (TCon s, _) <- spines ]
                           && all (1==) [length xs | (_, xs) <- spines ]
         allConstant        = all isTCon (map fst spines)
                           && all null (map snd spines)
         allVariable        = all isTVar (map fst spines)
                           && all null (map snd spines)
         allFunctionType    = all isTCon (map fst spines)
                           && all ("->"==) [ s | (TCon s, _) <- spines ]
                           && all (2==) [length xs | (_, xs) <- spines ]

recs :: (Int -> Bool) -> [Tps] -> [[PPrint.Doc]]
recs predicate = transpose . map (rec predicate) . transpose 

rec :: (Int -> Bool) -> Tps -> [PPrint.Doc]    
rec predicate tps = 
   let docs  = typesToAlignedDocs tps     
       bools = map (predicate . priorityOfType) tps
       maybeParenthesize (b, doc) 
          | b         = PPrint.parens doc
          | or bools  = doc <> PPrint.text "  "
          | otherwise = doc
   in map maybeParenthesize (zip bools docs)

showTwoTypesSpecial (t1,t2) = 
   let [d1,d2] = typesToAlignedDocs [t1,t2]
   in (d1,d2)

showTwoTypes = showTwoTypesSpecial 
   
allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = all (x==) xs
 
sameLength :: [String] -> [String]
sameLength xs = 
   let n = maximum (0 : map length xs)  
       f = take n . (++repeat ' ') 
   in map f xs

sameLengthRight :: [String] -> [String]
sameLengthRight = 
   map reverse . sameLength . map reverse

appDocs :: [Doc] -> Doc
appDocs = foldl1 (\d1 d2 -> PPrint.group $ d1 <> line <> d2)

tupleDocs :: [Doc] -> Doc
tupleDocs [] = PPrint.text "()"
tupleDocs ds = PPrint.hang 0 $ PPrint.group  (PPrint.text "(" <> 
          foldl1 (\d1 d2 -> d1 <> line <> PPrint.text "," <+> d2) ds) 
          <> PPrint.text ")"   

funDocs :: [Doc] -> Doc
funDocs = PPrint.group . foldl1 (\d1 d2 -> d1 <> line <> text "->" <+> d2)          

{-| Module      :  HeliumMessages
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
    
    Defines how the (error) messages should be reported by the Helium compiler.
	(For instance, one could define another layout, or produce XML-like output).
-}

module Helium.StaticAnalysis.Messages.HeliumMessages where

import Helium.StaticAnalysis.Messages.Messages 
import Top.Types
import qualified Lvm.Common.PPrint as PPrint
import qualified Helium.Utils.OneLiner as OneLiner
import List                (intersperse, zipWith4)
import Helium.StaticAnalysis.Miscellaneous.TypesToAlignedDocs  (qualifiedTypesToAlignedDocs)
import Helium.Syntax.UHA_Range           (isImportRange, showRanges)
import Char                (isSpace)

----------------------------------------------------------
-- message parameters

lineLength :: Int
lineLength = 80

tableWidthLeft :: Int
tableWidthLeft = 16

tablePrefix :: String
tablePrefix = " "

tableSeparator :: String
tableSeparator = " : "

splitStringMargin :: Int
splitStringMargin = 15

----------------------------------------------------------

tableWidthRight :: Int
tableWidthRight = lineLength - tableWidthLeft - length (tablePrefix ++ tableSeparator)

----------------------------------------------------------

instance Show MessageLine where
   show messageLine = 
      case prepareTypesAndTypeSchemes messageLine of
         MessageOneLiner m   -> show m++"\n"
         MessageTable tab    -> showTable tab
         MessageHints pre ms -> showHints pre ms

instance Show MessageBlock where
   show (MessageString s     ) = s
   show (MessageRange r      ) = show r
   show (MessageType tp      ) = show tp
   show (MessagePredicate p  ) = show p
   show (MessageOneLineTree t) = OneLiner.showOneLine tableWidthRight t
   show (MessageCompose ms   ) = concatMap show ms

sortAndShowMessages :: HasMessage a => [a] -> String
sortAndShowMessages = concatMap showMessage . sortMessages  
   
showMessage :: HasMessage message => message -> String
showMessage x =
    let rangePart = 
           case filter (not . isImportRange) (getRanges x) of
              [] -> MessageString ""
              xs -> MessageString (showRanges xs ++ ": ")
        messageWithRange = 
	   case getMessage x of
              MessageOneLiner m:rest -> MessageOneLiner (MessageCompose [rangePart, m]) : rest
              xs                     -> MessageOneLiner rangePart : xs
    in concatMap show messageWithRange

showHints :: String -> MessageBlocks -> String
showHints pre ms =
   let firstPrefix = "  " ++ pre ++ ": "
       restPrefix  = replicate (4 + length pre) ' '
       prefixes    = firstPrefix : repeat restPrefix
       width       = lineLength - length firstPrefix
       combine     = concat . intersperse ("\n" ++ restPrefix)
   in unlines . zipWith (++) prefixes . map (combine . splitString width . show) $ ms

showTable :: [(Bool, MessageBlock, MessageBlock)] -> String
showTable = 
   let showTuple (indentBlock, leftBlock, rightBlock) =
          let -- some helper functions
	      leftWidth = tableWidthLeft - (if indentBlock then 2 else 0)
              concatFour a b c d = a ++ b ++ c ++ d
	      makeOfLength i s   = take i (s ++ repeat ' ')
	      linesOfLength i    = repeat (replicate i ' ')
	      -- lines
              leftLines  = splitString leftWidth       (show leftBlock)
              rightLines = splitString tableWidthRight (show rightBlock)
              nrOfLines  = length leftLines `max` length rightLines
	      -- the four columns
              indentColumn    = if indentBlock
	                          then               linesOfLength (length tablePrefix + 2)
                                  else tablePrefix : linesOfLength (length tablePrefix)
              leftColumn      = map (makeOfLength leftWidth) leftLines ++ linesOfLength leftWidth 
	      seperatorColumn = tableSeparator : linesOfLength (length tableSeparator)
              rightColumn     = rightLines ++ linesOfLength tableWidthRight
          in unlines (take nrOfLines (zipWith4 concatFour indentColumn leftColumn seperatorColumn rightColumn))
   in concatMap showTuple . renderTypesInRight
  
-- if two types or type schemes follow each other in a table (on the right-hand side)
-- then the two types are rendered in a special way.
renderTypesInRight :: [(Bool, MessageBlock, MessageBlock)] -> [(Bool, MessageBlock, MessageBlock)]
renderTypesInRight table =
   case table of
      hd@(q1, l1, r1) : tl@((q2, l2, r2) : rest)
        -> case (maybeQType r1, maybeQType r2) of
              (Just tp1, Just tp2) -> 
	         let [doc1, doc2] = qualifiedTypesToAlignedDocs [tp1, tp2]
                     render = flip PPrint.displayS [] . PPrint.renderPretty 1.0 tableWidthRight
                 in (q1, l1, MessageType (toTpScheme (TCon (render doc1))))
                  : (q2, l2, MessageType (toTpScheme (TCon (render doc2))))
                  : renderTypesInRight rest
              _ -> hd : renderTypesInRight tl
      _ -> table

  where maybeQType :: MessageBlock -> Maybe QType
        maybeQType (MessageType qtype) = Just (unquantify qtype) -- unsafe?
        maybeQType _                   = Nothing

-- make sure that a string does not exceed a certain width.
-- Two extra features:
--   - treat '\n' in the proper way.
--     (Be careful here: an enter in a string or a character does not
--      make a new line)
--   - try not to break words.
splitString :: Int -> String -> [String]
splitString width = concatMap f . lines
   where f string | length string <= width
                    = [string]
                  | otherwise
                    = let lastSpace     = last . (width:) . map fst . filter predicate
                                               . zip [0..] . take width $ string
                          predicate (pos, char) = isSpace char && pos >= width - splitStringMargin
                          (begin, rest) = splitAt lastSpace string
                      in begin : f (dropWhile isSpace rest)
                    
-- Prepare the types and type schemes in a messageline to be shown.
--
-- type schemes:
--   * responsible for their own type variables
--   * monomorphic type variables are frozen, that is, replaced by _1, _2, etc.
-- types: 
--   * use a, b, c for type variables
--   * use the type variables consistent over all types 
--       (for instance, all v5 are mapped to a 'c')
prepareTypesAndTypeSchemes :: MessageLine -> MessageLine
prepareTypesAndTypeSchemes messageLine = newMessageLine
   where 
    (result, _, names) = replaceTypeSchemes messageLine
    newMessageLine     = giveTypeVariableIdentifiers result
   
     --step 1
    replaceTypeSchemes :: MessageLine -> (MessageLine, Int, [String])
    replaceTypeSchemes messageLine = 
       let unique = nextFTV messageLine
       in case messageLine of
             MessageOneLiner mb -> let (r, i, ns) = f_MessageBlock unique mb
                                   in (MessageOneLiner r, i, ns)
             MessageTable tab   -> let (r, i, ns) = f_Table unique tab
                                   in (MessageTable r, i, ns)
             MessageHints s mbs -> let (r, i, ns) = f_MessageBlocks unique mbs
                                   in (MessageHints s r, i, ns)

    --step 2
    giveTypeVariableIdentifiers :: MessageLine -> MessageLine
    giveTypeVariableIdentifiers ml = 
       let sub = listToSubstitution (zip (ftv ml) [ TCon s | s <- variableList, s `notElem` names])
       in sub |-> ml
   
    f_Table :: Int -> [(Bool, MessageBlock, MessageBlock)] -> ([(Bool, MessageBlock, MessageBlock)], Int, [String])
    f_Table i [] = ([], i, [])
    f_Table i ((q, a, b):xs) = let (r1, i1, ns1) = f_MessageBlock i  a
                                   (r2, i2, ns2) = f_MessageBlock i1 b
                                   (r3, i3, ns3) = f_Table        i2 xs
                               in ((q, r1, r2):r3, i3, ns1++ns2++ns3)    
    
    f_MessageBlocks :: Int -> [MessageBlock] -> ([MessageBlock], Int, [String])
    f_MessageBlocks i []     = ([], i, [])
    f_MessageBlocks i (x:xs) = let (r1, i1, ns1) = f_MessageBlock  i  x
                                   (r2, i2, ns2) = f_MessageBlocks i1 xs
                               in (r1:r2, i2, ns1++ns2)

    f_MessageBlock :: Int -> MessageBlock -> (MessageBlock, Int, [String])
    f_MessageBlock unique messageBlock = 
        case messageBlock of
           MessageCompose mbs -> let (r, i, ns) = f_MessageBlocks unique mbs
                                 in (MessageCompose r, i, ns)
           MessageType ts     -> let (unique', ps, its) = instantiateWithNameMap unique ts
                                 in (MessageType (toTpScheme (ps .=>. its)), unique', constantsInType its)				   
           _                  -> (messageBlock, unique, [])

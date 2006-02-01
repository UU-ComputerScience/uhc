module ParseErrPrettyPrint
  ( ppPos, ppErr
  )
  where

import Data.List
import UU.Pretty
import UU.Parsing
import UU.Scanner.Position( noPos, Pos, Position(..) )
-- import UU.Scanner.GenToken
import PPUtils

-------------------------------------------------------------------------
-- PP of parse errors
-------------------------------------------------------------------------

ppPos :: Position p => p -> PP_Doc
ppPos p
  = if l < 0 then empty else pp f >|< ppListSep "(" ")" "," [pp l,pp c]
  where l = line p
        c = column p
        f = file p

ppErr :: Position pos => (String,pos) -> PP_Doc -> PP_Doc
ppErr (sym,pos) p
  = "*** ERROR ***"
    >-< ppPos pos >|< (if null sym then empty else ", at symbol '" >|< pp sym >|< "'") >|< ":"
    >-< indent 4 p

instance (Eq s, Show s, Show p, Position p) => PP (Message s p) where
  pp (Msg expecting position action)  
    = ppErr ("",position)
            (   "Expecting  :" >#< (fillblock 120 . intersperse (pp " ") . map pp $ showExp)
                               >#< (if null omitExp then empty else pp "...")
            >-< "Repaired by:" >#< show action
            )
    where (showExp,omitExp) = splitAt 20 . words $ show expecting

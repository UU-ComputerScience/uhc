module ParseErrPrettyPrint
  ( ppPos, ppErr, ppWarn
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

ppMsg :: Position pos => String -> (String,pos) -> PP_Doc -> PP_Doc
ppMsg what (sym,pos) p
  = "***" >#< what >#< "***"
    >-< ppPos pos >|< (if null sym then empty else ", at symbol '" >|< pp sym >|< "'") >|< ":"
    >-< indent 4 p

ppErr, ppWarn :: Position pos => (String,pos) -> PP_Doc -> PP_Doc
ppErr  = ppMsg "ERROR"
ppWarn = ppMsg "WARNING"

instance (Eq s, Show s, Show p, Position p) => PP (Message s p) where
  pp (Msg expecting position action)  
    = ppErr ("",position)
            (   "Expecting  :" >#< (fillblock 120 . intersperse (pp " ") . map pp $ showExp)
                               >#< (if null omitExp then empty else pp "...")
            >-< "Repaired by:" >#< show action
            )
    where (showExp,omitExp) = splitAt 20 . words $ show expecting

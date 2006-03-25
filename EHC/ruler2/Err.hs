-------------------------------------------------------------------------
-- Error
-------------------------------------------------------------------------

module Err
  ( Err(..), mkPPErr, ppErrPPL
  , errLIsFatal
  , errFirst
  )
  where

import UU.Pretty
import PPUtils
import RulerScanner( SPos, emptySPos )
import ParseErrPrettyPrint
import Nm
import Utils( maybeHd )

-------------------------------------------------------------------------
-- Errors
-------------------------------------------------------------------------

data Err
  = Err_UndefNm      SPos String String [Nm]
  | Err_NoJdSpec       SPos String [Nm]
  | Err_Dups         SPos String String [PP_Doc]
  | Err_MutDpds      SPos String String [PP_Doc]
  | Err_NoXXFor      SPos String String [Nm]
  | Err_Match        SPos String PP_Doc PP_Doc
  | Err_RlPost       SPos String Nm
  | Err_NotAEqnForm  SPos PP_Doc
  | Err_FileNotFound SPos String [String]
  | Err_PP                PP_Doc
  deriving Show

-------------------------------------------------------------------------
-- Pretty printing
-------------------------------------------------------------------------

ppErrPPL :: PP a => [a] -> PP_Doc
ppErrPPL = vlist . map pp

mkPPErr :: PP a => a -> Err
mkPPErr = Err_PP . pp

instance PP Err where
  pp (Err_UndefNm pos cx knd nmL)
    = ppErr pos ("In" >#< cx >#< knd >|< "(s) are undefined:" >#< ppCommas nmL)
  pp (Err_Dups pos cx knd nmL)
    = ppErr pos (cx >#< "has duplicate" >#< knd >|< "s:" >#< ppCommas nmL)
  pp (Err_MutDpds pos cx knd nmL)
    = ppErr pos (cx >#< "has mutually dependent" >#< knd >|< "s:" >#< ppCommas nmL)
  pp (Err_NoXXFor pos cx knd nmL)
    = ppErr pos ("In" >#< cx >#< "a" >#< knd >#< "lacks for:" >#< ppCommas nmL)
  pp (Err_NoJdSpec pos cx nmL)
    = ppErr pos ("In" >#< cx >#< "no judgespec for:" >#< ppCommas nmL)
  pp (Err_Match pos cx given reqd)
    = ppErr pos ("In" >#< cx >#< "could not match"
                 >-< indent 2
                       (    "scheme judgement expr:" >#< reqd
                        >-< "given view expr      :" >#< given
                       )
                )
  pp (Err_RlPost pos cx nm)
    = ppErr pos ("In" >#< cx >#< "conclusion lacks judgement for ruleset's scheme:" >#< pp nm)
  pp (Err_FileNotFound pos fp sp)
    = ppErr pos ("File not found"
                 >-< indent 2
                       (    "file name         :" >#< fp
                        >-< "searched locations:" >#< vlist (map (text.show) sp)
                       )
                )
  pp (Err_NotAEqnForm pos e)
    = ppWarn pos ("expr not of (AG rule) form ... = ...:" >#< e)
  pp (Err_PP e)
    = e

-------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------

errIsFatal :: Err -> Bool
errIsFatal (Err_NotAEqnForm _ _) = False
errIsFatal _                     = True

errLIsFatal :: [Err] -> Bool
errLIsFatal es = not (null es) && any errIsFatal es

errFirst :: [[Err]] -> [Err]
errFirst = maybeHd [] id . filter (not . null)
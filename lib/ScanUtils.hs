module ScanUtils
  ( ScanOpts(..), defaultScanOpts
  )
  where

import IO
import Data.List
import UU.Pretty
import UU.Parsing
import UU.Scanner.Position( noPos, Pos(..), Position(..) )
import UU.Scanner.GenToken
import PPUtils

-------------------------------------------------------------------------
-- PP of parse errors
-------------------------------------------------------------------------

{-
instance (Eq s, Show s, Show p, Position p) => PP (Message s p) where
  pp (Msg expecting position action)  
    = ppErr ("",position)
            (   "Expecting  :" >#< (fillblock 120 . intersperse (pp " ") . map pp $ showExp)
                               >#< (if null omitExp then empty else pp "...")
            >-< "Repaired by:" >#< show action
            )
    where (showExp,omitExp) = splitAt 20 . words $ show expecting
-}

instance Position p => Position (Maybe p) where
  line   = maybe (line   noPos) line
  column = maybe (column noPos) column
  file   = maybe (file   noPos) file

instance Position (GenToken k t v) where
  line   = line   . position
  column = column . position
  file   = file   . position

instance PP Pos where
  pp (Pos l c f) = (if null f then empty else pp f >|< ":" ) >|< l >|< ":" >|< c

-------------------------------------------------------------------------
-- ScanOpts
-------------------------------------------------------------------------

data ScanOpts
  =  ScanOpts
        {   scoKeywordsTxt      ::  [String]
        ,   scoKeywordsOps      ::  [String]
        ,   scoSpecChars        ::  String
        ,   scoOpChars          ::  String
        ,   scoSpecPairs        ::  [String]
        ,   scoDollarIdent      ::  Bool
        ,   scoOffsideTrigs     ::  [String]
        ,   scoOffsideModule    ::  String
        ,   scoOffsideOpen      ::  String
        ,   scoOffsideClose     ::  String
        }

defaultScanOpts :: ScanOpts
defaultScanOpts
  =  ScanOpts
        {   scoKeywordsTxt      =   []
        ,   scoKeywordsOps      =   []
        ,   scoSpecChars        =   ""
        ,   scoOpChars          =   ""
        ,   scoSpecPairs        =   []
        ,   scoDollarIdent      =   False
        ,   scoOffsideTrigs     =   []
        ,   scoOffsideModule    =   ""
        ,   scoOffsideOpen      =   ""
        ,   scoOffsideClose     =   ""
        }


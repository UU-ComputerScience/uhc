module EH.Util.ScanUtils
  ( ScanOpts(..), defaultScanOpts
  , isNoPos, posIs1stColumn
  , genTokVal, genTokTp, genTokMap
  )
  where

import IO
import Data.List
import EH.Util.Pretty
import UU.Parsing
import UU.Scanner.Position( noPos, Pos(..), Position(..) )
import UU.Scanner.GenToken

-------------------------------------------------------------------------
-- Utils for GenToken
-------------------------------------------------------------------------

genTokVal :: GenToken v t v -> v
genTokVal (ValToken _ v _) = v
genTokVal (Reserved   v _) = v

genTokTp :: GenToken k t v -> Maybe t
genTokTp (ValToken t _ _) = Just t
genTokTp _                = Nothing

genTokMap :: (a->b) -> GenToken a t a -> GenToken b t b
genTokMap f (ValToken t v p) = ValToken t (f v) p
genTokMap f (Reserved   k p) = Reserved   (f k) p

-------------------------------------------------------------------------
-- Utils for Pos
-------------------------------------------------------------------------

isNoPos :: Pos -> Bool
isNoPos (Pos l c f) = l < 0 || c < 0

posIs1stColumn :: Pos -> Bool
posIs1stColumn p = column p == 1

-------------------------------------------------------------------------
-- PP of parse errors
-------------------------------------------------------------------------

instance Position p => Position (Maybe p) where
  line   = maybe (line   noPos) line
  column = maybe (column noPos) column
  file   = maybe (file   noPos) file

instance Position (GenToken k t v) where
  line   = line   . position
  column = column . position
  file   = file   . position

instance PP Pos where
  pp (Pos l c f) = ppParens $ (if null f then empty else pp f >|< ":" ) >|< l >|< "," >|< c

-------------------------------------------------------------------------
-- ScanOpts
-------------------------------------------------------------------------

data ScanOpts
  =  ScanOpts
        {   scoKeywordsTxt      ::  ![String]
        ,   scoKeywordsOps      ::  ![String]
        ,   scoSpecChars        ::  !String
        ,   scoOpChars          ::  !String
        ,   scoSpecPairs        ::  ![String]
        ,   scoDollarIdent      ::  !Bool
        ,   scoOffsideTrigs     ::  ![String]
        ,   scoOffsideModule    ::  !String
        ,   scoOffsideOpen      ::  !String
        ,   scoOffsideClose     ::  !String
        ,   scoLitmode          ::  !Bool
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
        ,   scoLitmode          =   False
        }


module EH.Util.ScanUtils
  ( ScanOpts(..), defaultScanOpts
  
  , isNoPos, posIs1stColumn
  
  , InFilePos(..), infpStart, infpNone
  , infpAdvCol, infpAdvLine, infpAdv1Line, infpAdvStr
  
  , genTokVal, genTokTp, genTokMap
  
  , isLF, isStr, isStrQuote
  , isWhite, isBlack
  , isVarStart, isVarRest
  )
  where

import IO
import Data.Char
import Data.List
import qualified Data.Set as Set

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
-- InFilePos: Simplified Pos for inside a file only
-------------------------------------------------------------------------

data InFilePos
  = InFilePos { infpLine, infpColumn :: Int }
  deriving (Eq,Ord)

instance Show InFilePos where
  show (InFilePos l c) = if l < 0 || c < 0 then "" else "(" ++ show l ++ ":" ++ show c ++ ")"

infpStart :: InFilePos
infpStart = InFilePos 1 1

infpNone :: InFilePos
infpNone = InFilePos (-1) (-1)

infpAdvCol :: Int -> InFilePos -> InFilePos
infpAdvCol i p = p {infpColumn = i + infpColumn p}

infpAdvStr :: String -> InFilePos -> InFilePos
infpAdvStr s p = infpAdvCol (length s) p

infpAdvLine :: Int -> InFilePos -> InFilePos
infpAdvLine i p = p {infpLine = i + infpLine p, infpColumn = 1}

infpAdv1Line :: InFilePos -> InFilePos
infpAdv1Line = infpAdvLine 1

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

{-
ScanOpts encode all possible options we ever might want to pass to a scanner used inside the EHC project.
Hence not all options are used by all scanners.
-}

data ScanOpts
  =  ScanOpts
        {   scoKeywordsTxt      ::  !(Set.Set String)       -- identifiers which are keywords
        ,   scoCommandsTxt      ::  !(Set.Set String)       -- identifiers which are commands
        ,   scoKeywordsOps      ::  !(Set.Set String)       -- operators which are keywords
        ,   scoSpecChars        ::  !(Set.Set Char)         -- 1 char keywords
        ,   scoOpChars          ::  !(Set.Set Char)         -- chars used for operators
        ,   scoSpecPairs        ::  !(Set.Set String)       -- pairs of chars which form keywords
        ,   scoDollarIdent      ::  !Bool                   -- allow $ encoded identifiers
        ,   scoOffsideTrigs     ::  ![String]               -- offside triggers
        ,   scoOffsideModule    ::  !String                 -- offside start of module
        ,   scoOffsideOpen      ::  !String                 -- offside open symbol
        ,   scoOffsideClose     ::  !String                 -- offside close symbol
        ,   scoLitmode          ::  !Bool                   -- do literal scanning
        ,   scoVerbOpenClose    ::  ![(String,String)]      -- open/close pairs used for verbatim text
        }

defaultScanOpts :: ScanOpts
defaultScanOpts
  =  ScanOpts
        {   scoKeywordsTxt      =   Set.empty
        ,   scoCommandsTxt      =   Set.empty
        ,   scoKeywordsOps      =   Set.empty
        ,   scoSpecChars        =   Set.empty
        ,   scoOpChars          =   Set.empty
        ,   scoSpecPairs        =   Set.empty
        ,   scoDollarIdent      =   False
        ,   scoOffsideTrigs     =   []
        ,   scoOffsideModule    =   ""
        ,   scoOffsideOpen      =   ""
        ,   scoOffsideClose     =   ""
        ,   scoLitmode          =   False
        ,   scoVerbOpenClose    =   []
        }

-------------------------------------------------------------------------
-- Char predicates
-------------------------------------------------------------------------

isLF :: Char -> Bool
isLF = (`elem` "\n\r")

isStrQuote :: Char -> Bool
isStrQuote c = c == '"'

isStr :: Char -> Bool
isStr c = not (isStrQuote c || isLF c)

isVarStart :: Char -> Bool
isVarStart c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'

isVarRest :: Char -> Bool
isVarRest c = isVarStart c || isDigit c || c `elem` "'_"

isWhite :: Char -> Bool
isWhite = (`elem` " \t")

{-
isDig :: Char -> Bool
isDig c = c >= '0' && c <= '9'
-}

isBlack :: Char -> Bool
isBlack c = not (isWhite c || isLF c)


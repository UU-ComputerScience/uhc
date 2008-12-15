-------------------------------------------------------------------------
-- Common stuff
-------------------------------------------------------------------------

module Common
  ( Opts(..), defaultOpts

  , TextType(..)
  , texttypeMp

  , Err(..)

  , OutDoc, emptyout
  , Out(..)
  , (+++), outList, outListSep
  , outToString
  
  , putOut, putOutLn
  , hPutOutLn
  
  , cmbMb
  )
  where

import IO
import Data.List
import qualified Data.Map as Map

import qualified EH.Util.FastSeq as Seq

-------------------------------------------------------------------------
-- Input/output types
-------------------------------------------------------------------------

-- when extending TextType, also update texttypeMp
data TextType
  = TextType_DocLaTeX
  | TextType_TWiki
  | TextType_None
  deriving (Eq,Ord)

instance Show TextType where
  show TextType_DocLaTeX = "doclatex"
  show TextType_TWiki    = "twiki"
  show TextType_None     = "none"

texttypeMp :: Map.Map String TextType
texttypeMp = Map.fromList [ (show t, t) | t <- [TextType_DocLaTeX,TextType_TWiki] ]

-------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------

data Opts 
  = Opts
      { optGenFor           	::	TextType
      , optHelp					::	Bool
      , optGenHeaderNumbering	::	Bool
      }

defaultOpts 
  = Opts
      { optGenFor           	= 	TextType_DocLaTeX
      , optHelp					=	False
      , optGenHeaderNumbering	=	False
      }

-------------------------------------------------------------------------
-- Errors
-------------------------------------------------------------------------

data Err
  = Err_Out 		OutDoc
  | Err_NoPlugin 	TextType

instance Show Err where
  show _ = "Err"

instance Out Err where
  out (Err_Out 			o) = o
  out (Err_NoPlugin 	t) = "no plugin for " +++ show t

-------------------------------------------------------------------------
-- Combine maybe's
-------------------------------------------------------------------------

cmbMb :: Maybe a -> Maybe a -> Maybe a
cmbMb x@(Just _) _ = x
cmbMb _ x@(Just _) = x
cmbMb _ _          = Nothing

-------------------------------------------------------------------------
-- Output document
-------------------------------------------------------------------------

infixr 3 +++

type OutDoc = Seq.Seq String

emptyout :: OutDoc
emptyout = Seq.empty

class Out a where
  out :: a -> OutDoc

instance Out OutDoc where
  out = id

instance Out Int where
  out = Seq.singleton . show

instance Out Char where
  out c = Seq.singleton [c]

instance Out String where
  out = Seq.singleton

outToString :: OutDoc -> String
outToString = concat . Seq.toList

(+++) :: (Out a, Out b) => a -> b -> OutDoc
a +++ b = out a `Seq.union` out b

outList :: Out x => [x] -> OutDoc
outList = Seq.unions . map out

outListSep :: (Out s, Out c, Out o, Out a) => o -> c -> s -> [a] -> OutDoc
outListSep o c s outs = o +++ outList (intersperse (out s) (map out outs)) +++ c

hPutOut :: Handle -> OutDoc -> IO ()
hPutOut h ou = hPutStr h (outToString ou)

putOut :: OutDoc -> IO ()
putOut = hPutOut stdout

hPutOutLn :: Handle -> OutDoc -> IO ()
hPutOutLn h ou = hPutStrLn h (outToString ou)

putOutLn :: OutDoc -> IO ()
putOutLn = hPutOutLn stdout

hPutOutFile :: Handle -> OutDoc -> Int -> IO ()
hPutOutFile h ou wid = hPutOutLn h ou
    
putOutFile :: String -> OutDoc -> Int -> IO ()
putOutFile fn ou wid
  =  do  {  h <- openFile fn WriteMode
         ;  hPutOutFile h ou wid
         ;  hClose h
         }


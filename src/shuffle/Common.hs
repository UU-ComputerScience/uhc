-------------------------------------------------------------------------
-- Common stuff
-------------------------------------------------------------------------

module Common
  ( module Data.Maybe
  , module Data.Char
  -- , module UU.Pretty
  , module EH.Util.Nm
  , module EH.Util.FPath
  , module EH.Util.PPUtils
  , Err(..), ErrM, ppErr, showUndef
  , openURI
  , Opts(..), defaultOpts, optsHasNoVerOrder
  , URef
  , CRef, CPos(..)
  , ChKind(..), ChDest(..), ChWrap(..)
  , Version(..), VersionOrder
  , KVMap
  )
  where

import Data.Maybe
import Data.Char
import qualified Data.Map as Map
import Network.URI
import IO
import System.Directory
import System.Console.GetOpt
import UU.Pretty
import EH.Util.FPath
import EH.Util.PPUtils
import EH.Util.Nm

-------------------------------------------------------------------------
-- Errors
-------------------------------------------------------------------------

data Err
  = Err_UndefNm     CPos String [Nm]
  | Err_UndefURI    CPos String
  | Err_Exec        CPos String String
  deriving Show

type ErrM = Map.Map CPos Err

ppErr :: CPos -> PP_Doc -> PP_Doc
ppErr pos p
  = "*** ERROR ***"
    >-< show pos >|< ":"
    >-< indent 4 p

instance PP Err where
  pp (Err_UndefNm pos knd nmL)
    = ppErr pos (knd >|< "(s) are undefined:" >#< ppCommas' nmL)
  pp (Err_UndefURI pos u)
    = ppErr pos ("could not open:" >#< u)
  pp (Err_Exec pos f e)
    = ppErr pos (   "execution of:" >#< f
                >-< "failed      :" >#< e
                )

showUndef :: Show r => r -> String
showUndef r = "<<<<" ++ show r ++ ">>>>"

-------------------------------------------------------------------------
-- URI handling
-------------------------------------------------------------------------

openURI :: URI -> IO (Maybe Handle)
openURI u
  = case uriScheme u of
      "file:" -> do { ex <- doesFileExist p
                    ; if ex
                      then do { h <- openFile p ReadMode
                              ; return (Just h)
                              }
                      else return Nothing
                    }
      _       -> return Nothing
  where p = uriPath u

-------------------------------------------------------------------------
-- Key/value pair handling
-------------------------------------------------------------------------

type KVMap = Map.Map String String

-------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------

data Opts 
  = Opts
      { optAG           :: Bool
      , optHS           :: Bool
      , optPlain        :: Bool
      , optLaTeX        :: Bool
      , optPreamble     :: Bool
      , optLinePragmas  :: Bool
      , optIndex        :: Bool
      , optHelp         :: Bool
      , optChDest       :: (ChDest,String)
      , optGenVersion   :: Version
      , optBaseName     :: Maybe String
      , optBaseFPath    :: FPath
      , optWrapLhs2tex  :: ChWrap
      , optMbXRefExcept :: Maybe String
      , optVerOrder     :: VersionOrder
      , optDefs			:: KVMap
      }

defaultOpts
  = Opts
      { optAG           =  False
      , optHS           =  False
      , optLaTeX        =  False
      , optPreamble     =  True
      , optLinePragmas  =  False
      , optPlain        =  False
      , optIndex        =  False
      , optHelp         =  False
      , optChDest       =  (ChHere,"")
      , optGenVersion   =  VNone
      , optBaseName     =  Nothing
      , optBaseFPath    =  emptyFPath
      , optWrapLhs2tex  =  ChWrapCode
      , optMbXRefExcept =  Nothing
      , optVerOrder     =  [[]]
      , optDefs			=  Map.empty
      }

optsHasNoVerOrder :: Opts -> Bool
optsHasNoVerOrder = null . head . optVerOrder

-------------------------------------------------------------------------
-- URI ref
-------------------------------------------------------------------------

type URef = String

-------------------------------------------------------------------------
-- Chunk ref, position (in file)
-------------------------------------------------------------------------

type CRef = Nm
data CPos = CPos FPath Int
          deriving (Eq,Ord)

instance Show CPos where
  show (CPos fp l) = fpathToStr fp ++ ":" ++ show l

-------------------------------------------------------------------------
-- Chunk kind, purpose/destination
-------------------------------------------------------------------------

data ChKind
  = ChAG | ChHS | ChPlain
  deriving (Show,Eq,Ord)

data ChDest
  = ChHere | ChHide
  deriving (Show,Eq,Ord)

data ChWrap
  = ChWrapCode | ChWrapBoxCode | ChWrapBeamerBlockCode String | ChWrapTT | ChWrapTTtiny | ChWrapPlain
  deriving (Show,Eq,Ord)

-------------------------------------------------------------------------
-- Version
-------------------------------------------------------------------------

data Version    = VAll
                | VPre
                | VNone
                | VRef {verRef :: Int}
                | VNest {verNest :: Version, verRef :: Int}
                deriving (Show,Eq,Ord)

type VersionOrder = [[Version]]





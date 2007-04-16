-------------------------------------------------------------------------
-- Common stuff
-------------------------------------------------------------------------

module Common
  ( module Data.Maybe
  , module Data.Char
  , module EH.Util.Nm
  , module EH.Util.FPath
  , module EH.Util.Pretty
  , Err(..), ErrM, ppErr, showUndef
  , openURI
  , Opts(..), defaultOpts, optsHasNoVerOrder
  , URef
  , CRef, CPos(..)
  , ChKind(..), ChDest(..), ChWrap(..)
  , Version(..), VersionOrder
  , verMember
  , VOMp, sortOnVOMp
  , KVMap
  , CompilerRestriction(..)
  )
  where

import Data.Maybe
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map(Map)
import Data.Set(Set)
import Network.URI
import IO
import System.Directory
import System.Console.GetOpt
import EH.Util.Pretty
import EH.Util.FPath
import EH.Util.Utils
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
      , optCompiler     :: [Int]
      , optHelp         :: Bool
      , optGenDeps      :: Bool
      , optChDest       :: (ChDest,String)
      , optGenVersion   :: Version
      , optBaseName     :: Maybe String
      , optBaseFPath    :: FPath
      , optWrapLhs2tex  :: ChWrap
      , optMbXRefExcept :: Maybe String
      , optVerOrder     :: VersionOrder
      , optDefs         :: KVMap
      , optDepNamePrefix :: String
      , optDepSrcVar     :: String
      , optDepDstVar     :: String
      , optDepMainVar    :: String
      , optDepDpdsVar    :: String
      , optDepOrigDpdsVar :: String
      , optDepBaseDir     :: String
      , optDepTerm        :: Map String [String]
      , optDepIgn         :: Set String
      } deriving (Show)

defaultOpts
  = Opts
      { optAG           =  False
      , optHS           =  False
      , optLaTeX        =  False
      , optPreamble     =  True
      , optLinePragmas  =  False
      , optPlain        =  False
      , optIndex        =  False
      , optCompiler     = []
      , optHelp         =  False
      , optGenDeps      =  False
      , optChDest       =  (ChHere,"")
      , optGenVersion   =  VNone
      , optBaseName     =  Nothing
      , optBaseFPath    =  emptyFPath
      , optWrapLhs2tex  =  ChWrapCode
      , optMbXRefExcept =  Nothing
      , optVerOrder     =  [[]]
      , optDefs			=  Map.empty
      , optDepNamePrefix = error "optDepNamePrefix not set"
      , optDepSrcVar     = error "optDepSrcVar not set"
      , optDepDstVar     = error "optDepDstVar not set"
      , optDepMainVar    = error "optDepMainVar not set"
      , optDepDpdsVar    = error "optDepDpdsVar not set"
      , optDepOrigDpdsVar = error "optDepOrigDpdsVar not set"
      , optDepBaseDir     = error "optDepBaseDir not set"
      , optDepTerm        = Map.empty
      , optDepIgn         = Set.empty
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
  = ChAG | ChHS | ChPlain | ChHaddock
  deriving (Show,Eq,Ord)

data ChDest
  = ChHere | ChHide
  deriving (Show,Eq,Ord)

data ChWrap
  = ChWrapCode
  | ChWrapBoxCode 			(Maybe String)
  | ChWrapBeamerBlockCode 	String
  | ChWrapTT
  | ChWrapTTtiny
  | ChWrapPlain
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
type VOMp = Map.Map Version Int

verMember :: Version -> VOMp -> Bool
verMember VAll _ = True
verMember v    s = Map.member v s

sortOnVOMp :: VOMp -> [(Version,x)] -> [x]
sortOnVOMp m = map snd . sortOn fst . map (\(v,x) -> (Map.findWithDefault 0 v m,x))

-------------------------------------------------------------------------
-- Compiler restrictions
-------------------------------------------------------------------------

data CompilerRestriction
  = Restricted (Maybe [Int]) (Maybe [Int])
  deriving Show

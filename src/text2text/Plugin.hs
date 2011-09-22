{-
A Plugin holds all info required for parsing from and formatting to a particular text format.
-}

-------------------------------------------------------------------------
-- Plugin structure
-------------------------------------------------------------------------

module Plugin
  ( Plugin(..), defaultPlugin
  , PluginMp
  )
  where

import qualified Data.Map as Map

import UU.Parsing
import qualified EH.Util.FastSeq as Seq

import Common
import Text
import Text.Parser.Common

-------------------------------------------------------------------------
-- Plugin
-------------------------------------------------------------------------

data Plugin
  = Plugin
      { plgHasParserTextItems 	:: Bool									-- has a parser (Maybe cannot be used because parser type cannot be type parameter in recent GHC versions (>=7))
      , plgParseTextItems 		:: Maybe (T2TPr (Seq.Seq TextItem))		-- parse text items
      , plgParseTextItems2 		:: T2TPr ( (Seq.Seq TextItem))		-- parse text items
      , plgScanOptsMp 			:: ScanOptsMp							-- scanner configuration
      , plgScanInitState		:: ScState								-- initial scanning state
      , plgToOutDoc				:: Maybe (Opts -> AGItf -> OutDoc)		-- generate output for format
      }

defaultPlugin :: Plugin
defaultPlugin
  = Plugin
      { plgHasParserTextItems 	= False
      , plgParseTextItems 		= Nothing
      , plgParseTextItems2 		= pSucceed Seq.empty
      , plgScanOptsMp 			= Map.empty
      , plgScanInitState		= defaultScState
      , plgToOutDoc				= Nothing
      }

type PluginMp = Map.Map TextType Plugin

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
      { plgParseTextItems 	:: T2TPr (Seq.Seq TextItem)
      , plgScanOptsMp 		:: ScanOptsMp
      , plgScanInitState	:: ScState
      }

defaultPlugin :: Plugin
defaultPlugin
  = Plugin
      { plgParseTextItems 	= undefined
      , plgScanOptsMp 		= Map.empty
      , plgScanInitState	= defaultScState
      }

type PluginMp = Map.Map TextType Plugin

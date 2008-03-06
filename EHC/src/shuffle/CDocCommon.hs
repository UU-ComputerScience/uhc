-------------------------------------------------------------------------
-- Common stuff based on CDoc
-------------------------------------------------------------------------

module CDocCommon
  ( module Common
  , module CDoc
  , NmChInfo(..), NmChMp
  )
  where

import Common
import CDoc
import qualified Data.Map as Map

-------------------------------------------------------------------------
-- Named chunks
-------------------------------------------------------------------------

data NmChInfo
  = NmChInfo
      { nciNm       :: CRef
      , nciChDest   :: ChDest
      , nciMbCDoc   :: Maybe CDoc
      , nciMkCDoc   :: MkCDoc
      }

type NmChMp = Map.Map CRef NmChInfo


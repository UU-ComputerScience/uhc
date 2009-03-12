{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Prelude
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The Prelude: a standard module imported by default into all Haskell
-- modules.  For more documentation, see the Haskell 98 Report
-- <http://www.haskell.org/onlinereport/>.
--
-----------------------------------------------------------------------------

module Prelude
  ( module EHC.Prelude
  , module EHC.Eq
  , module EHC.Ord
  , module EHC.Enum
  , module EHC.Bounded
  , module EHC.Ix
  , module EHC.Show
  , module EHC.Read
  )
  where

import EHC.Prelude hiding
  ( absReal, signumReal
  -- , primEqInt
  )
import EHC.Eq
import EHC.Ord
import EHC.Enum
import EHC.Bounded
import EHC.Ix
import EHC.Show
import EHC.Read

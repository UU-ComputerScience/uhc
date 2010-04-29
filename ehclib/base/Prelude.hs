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
  ( module UHC.Base
  , module UHC.Eq
  , module UHC.Ord
  , module UHC.Enum
  , module UHC.Bounded
  -- , module UHC.Ix
  , module UHC.Show
  , module UHC.Read
  , module UHC.Run
#ifdef __UHC_TARGET_C__
  , module UHC.OldIO
#else
  , module System.IO
#endif
  , module UHC.IOBase
  )
  where

import UHC.Base hiding
  ( absReal, signumReal
  -- , primEqInt
  , State
  -- , IOWorld, RealWorld
  , ByteArray
  , exitWithIntCode
  )
import UHC.Eq
import UHC.Ord
import UHC.Enum
import UHC.Bounded
-- import UHC.Ix
import UHC.Show
import UHC.Read
import UHC.IOBase
  ( IOError, ioError, userError, catch, unsafePerformIO
#ifdef __UHC_TARGET_C__
  , FilePath
#endif
  )
import UHC.Run

#ifdef __UHC_TARGET_C__
import UHC.OldIO
#else
import System.IO
  ( IO, IOMode(..),
    -- *** Output functions
    putChar,
    putStr, putStrLn, print,
    -- *** Input functions
    getChar,
    getLine, getContents, interact,
    -- *** Files
    FilePath,
    readFile, writeFile, appendFile, readIO, readLn,
    openFile,
    hClose, hGetLine, hPutStrLn, hPutStr, hPutChar, hFlush,
    stdout, stdin, stderr
  )
#endif


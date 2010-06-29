{-# LANGUAGE CPP #-}
#define MODULE_NAME     Posix

#ifndef IS_WINDOWS
#define IS_WINDOWS      False
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.FilePath.Posix
-- Copyright   :  (c) Neil Mitchell 2005-2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- A library for FilePath manipulations, using Posix style paths on
-- all platforms. Importing "System.FilePath" is usually better.

-- Unfortunately, this #include breaks when haddocking with Cabal
#ifdef __HADDOCK__
module System.FilePath.Posix where
#else
#include "Internal.hs"
#endif


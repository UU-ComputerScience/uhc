{-# LANGUAGE NoImplicitPrelude, CPP #-}
{-# OPTIONS_GHC -XNoImplicitPrelude -#include "HsBase.h" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.IO.Fix
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- fixIO
--
-----------------------------------------------------------------------------


module System.IO.Fix (
    -- * The IO monad
    fixIO,                     -- :: (a -> IO a) -> IO a

  ) where

import UHC.Base
import Data.IORef
import System.IO.Unsafe

-- ---------------------------------------------------------------------------
-- fixIO

data Lazy a = Lazy a

-- functie aangepast met Lazy data type als wrapper voor newIORef (strict in zijn argument).
fixIO :: (a -> IO a) -> IO a
fixIO k = do
    ref <- newIORef (Lazy (throw NonTermination))
    ~(Lazy ans) <- unsafeInterleaveIO (readIORef ref)
    result <- k ans
    writeIORef ref (Lazy result)
    return result


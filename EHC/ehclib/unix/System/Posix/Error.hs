-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Error
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX error support
--
-----------------------------------------------------------------------------

module System.Posix.Error (
	throwErrnoPath,
	throwErrnoPathIf, 
	throwErrnoPathIf_,
	throwErrnoPathIfNull,
	throwErrnoPathIfMinus1,
	throwErrnoPathIfMinus1_
  ) where

import Foreign.C.Error


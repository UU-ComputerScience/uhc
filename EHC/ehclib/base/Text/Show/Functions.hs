{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans, CPP #-}
{-# OPTIONS_UHC "--optP=-traditional-cpp" #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Functions
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Optional instance of 'Text.Show.Show' for functions:
--
-- > instance Show (a -> b) where
-- > 	showsPrec _ _ = showString \"\<function\>\"
--
-----------------------------------------------------------------------------

module Text.Show.Functions () where

import Prelude

#ifndef __NHC__
instance Show (a -> b) where
	showsPrec _ _ = showString "<function>"
#else
instance (Show a,Show b) => Show (a->b) where
  showsPrec d a = showString "<<function>>"

  showsType a = showChar '(' . showsType value  . showString " -> " .
                               showsType result . showChar ')'
                where (value,result) = getTypes undefined
                      getTypes x = (x,a x)
#endif

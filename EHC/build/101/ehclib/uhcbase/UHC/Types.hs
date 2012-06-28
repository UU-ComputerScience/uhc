{-# LANGUAGE NoImplicitPrelude, CPP #-}

module UHC.Types
  -- export all
  where

import UHC.Base

#include "HsBaseConfig.h"
#include "CTypes.h"


data Int8
data Int16
data Int32
data Int64

data Word
data Word8
data Word16
data Word32
data Word64

data Addr


-- | Haskell type representing the C @char@ type.
NEWTYPE_TYPE_NODERIVING(CChar,HTYPE_CHAR)

-- | Haskell type representing the C @int@ type.
NEWTYPE_TYPE_NODERIVING(CInt,HTYPE_INT)

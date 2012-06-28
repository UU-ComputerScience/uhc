{-# LANGUAGE NoImplicitPrelude, CPP #-}

module UHC.Bits
  -- export everything
  where

import UHC.Base

#include "IntLikeInstance.h"


PRIMS2_BITLOGIC(Int,primAndInt,"primAndWord",primOrInt,"primOrWord",primXorInt,"primXorWord")
PRIMS_BITSHIFT(Int,primComplementInt,primShiftLeftInt,primShiftRightInt,primRotateLeftInt,primRotateRightInt)

PRIMS_BITLOGIC(Integer,primAndInteger,primOrInteger,primXorInteger)
PRIMS_BITSHIFT(Integer,primComplementInteger,primShiftLeftInteger,primShiftRightInteger,primRotateLeftInteger,primRotateRightInteger)


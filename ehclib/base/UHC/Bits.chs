%%[99
module UHC.Bits
  -- export everything
  where

import UHC.Base

#include "IntLikeInstance.h"

%%]

%%[99
PRIMS2_BITS(Int,primAndInt,"primAndWord",primOrInt,"primOrWord",primXorInt,"primXorWord")
PRIMS_BITS_SIZEDPD(Int,primComplementInt,primShiftLeftInt,primShiftRightInt,primRotateLeftInt,primRotateRightInt)
%%]

%%[99
PRIMS_BITS(Integer,primAndInteger,primOrInteger,primXorInteger)
PRIMS_BITS_SIZEDPD(Integer,primComplementInteger,primShiftLeftInteger,primShiftRightInteger,primRotateLeftInteger,primRotateRightInteger)
%%]


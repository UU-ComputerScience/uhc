%%[99
{-# LANGUAGE NoImplicitPrelude, CPP #-}
{-# OPTIONS_UHC "--optP=-traditional-cpp" #-}

module UHC.Bits
  -- export everything
  where

import UHC.Base

#include "IntLikeInstance.h"

%%]

%%[99
PRIMS2_BITLOGIC(Int,primAndInt,"primAndWord",primOrInt,"primOrWord",primXorInt,"primXorWord")
PRIMS_BITSHIFT(Int,primComplementInt,primShiftLeftInt,primShiftRightInt,primRotateLeftInt,primRotateRightInt)
%%]

%%[99
PRIMS_BITLOGIC(Integer,primAndInteger,primOrInteger,primXorInteger)
PRIMS_BITSHIFT(Integer,primComplementInteger,primShiftLeftInteger,primShiftRightInteger,primRotateLeftInteger,primRotateRightInteger)
%%]


%%[99
-- Compiler knows the Int types of this module are defined here

module UHC.Int
  ( Int8, Int16, Int32, Int64
  )
  where

import UHC.Base
import UHC.Types
import UHC.Prims
import UHC.Bits
import UHC.Show
import Data.Bits

#include "MachDeps.h"
#include "IntLikeInstance.h"

%%]

%%[99
INSTANCE_EQ(Int64,primEqInt64,primNeInt64)
INSTANCE_ORD(Int64,primCmpInt64,primLtInt64,primGtInt64,primLeInt64,primGeInt64)
INSTANCE_BOUNDED(Int64,primMinInt64,primMaxInt64)
INSTANCE_NUM(Int64,primAddInt64,primSubInt64,primMulInt64,primNegInt64,primIntegerToInt64,primIntToInt64)
INSTANCE_ENUM(Int64,primIntToInt64,primInt64ToInt)
INSTANCE_REAL(Int64)
INSTANCE_INTEGRAL1(Int64,primDivInt64,primModInt64,primQuotInt64,primRemInt64,primInt64ToInteger,primInt64ToInt)
INSTANCE_SHOW(Int64)
INSTANCE_READ(Int64)

INSTANCE_BITS1(Int64,64,True,primAndInt64,primOrInt64,primXorInt64,primComplementInt64,primShiftLeftInt64,primShiftRightInt64,primRotateLeftInt64,primRotateRightInt64)
%%]

If possible (when 32 bits fit into Int), use Int stuff, otherwise boxed with additional primitives.

%%[99
INSTANCE_EQ(Int32,primEqInt32,primNeInt32)
INSTANCE_ORD(Int32,primCmpInt32,primLtInt32,primGtInt32,primLeInt32,primGeInt32)
INSTANCE_BOUNDED(Int32,primMinInt32,primMaxInt32)
INSTANCE_NUM(Int32,primAddInt32,primSubInt32,primMulInt32,primNegInt32,primIntegerToInt32,primIntToInt32)
INSTANCE_ENUM(Int32,primIntToInt32,primInt32ToInt)
INSTANCE_REAL(Int32)
INSTANCE_INTEGRAL1(Int32,primDivInt32,primModInt32,primQuotInt32,primRemInt32,primInt32ToInteger,primInt32ToInt)
INSTANCE_SHOW(Int32)
INSTANCE_READ(Int32)

INSTANCE_BITS1(Int32,32,True,primAndInt32,primOrInt32,primXorInt32,primComplementInt32,primShiftLeftInt32,primShiftRightInt32,primRotateLeftInt32,primRotateRightInt32)
%%]

%%[99
INSTANCE_EQ(Int16,primEqInt16,primNeInt16)
INSTANCE_ORD(Int16,primCmpInt16,primLtInt16,primGtInt16,primLeInt16,primGeInt16)
INSTANCE_BOUNDED(Int16,primMinInt16,primMaxInt16)
INSTANCE_NUM(Int16,primAddInt16,primSubInt16,primMulInt16,primNegInt16,primIntegerToInt16,primIntToInt16)
INSTANCE_ENUM(Int16,primIntToInt16,primInt16ToInt)
INSTANCE_REAL(Int16)
INSTANCE_INTEGRAL1(Int16,primDivInt16,primModInt16,primQuotInt16,primRemInt16,primInt16ToInteger,primInt16ToInt)
INSTANCE_SHOW(Int16)
INSTANCE_READ(Int16)

INSTANCE_BITS1(Int16,16,True,primAndInt16,primOrInt16,primXorInt16,primComplementInt16,primShiftLeftInt16,primShiftRightInt16,primRotateLeftInt16,primRotateRightInt16)

%%]

%%[99
INSTANCE_EQ(Int8,primEqInt8,primNeInt8)
INSTANCE_ORD(Int8,primCmpInt8,primLtInt8,primGtInt8,primLeInt8,primGeInt8)
INSTANCE_BOUNDED(Int8,primMinInt8,primMaxInt8)
INSTANCE_NUM(Int8,primAddInt8,primSubInt8,primMulInt8,primNegInt8,primIntegerToInt8,primIntToInt8)
INSTANCE_ENUM(Int8,primIntToInt8,primInt8ToInt)
INSTANCE_REAL(Int8)
INSTANCE_INTEGRAL1(Int8,primDivInt8,primModInt8,primQuotInt8,primRemInt8,primInt8ToInteger,primInt8ToInt)
INSTANCE_SHOW(Int8)
INSTANCE_READ(Int8)

INSTANCE_BITS1(Int8,8,True,primAndInt8,primOrInt8,primXorInt8,primComplementInt8,primShiftLeftInt8,primShiftRightInt8,primRotateLeftInt8,primRotateRightInt8)

%%]

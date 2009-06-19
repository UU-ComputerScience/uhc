%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitives used by UHC.XX modules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Definitions for primitives which
\begin{itemize}
\item are used in >1 UHC.XX module
\item are used to avoid overloaded equivalents
\end{itemize}

In due time all (or most) primitives may move to here
%%]

%%[99
module UHC.Prims
  -- export all
  where

import UHC.Base
import UHC.Types

#include "MachDeps.h"
#include "IntLikeInstance.h"

%%]

Word

%%[99
PRIMS_BOUNDED(Word,primMinWord,primMaxWord)
PRIMS_CONVERSION_INTEGER(Word,primIntegerToWord,primWordToInteger)
PRIMS_CONVERSION_INT(Word,primIntToWord,primWordToInt)

PRIMS_EQ(Word,primEqWord,primNeWord)
PRIMS_ORD(Word,primCmpWord,primLtWord,primGtWord,primLeWord,primGeWord)
PRIMS_NUM(Word,primAddWord,primSubWord,primMulWord,primNegWord)
PRIMS_INTEGRAL1(Word,primDivWord,primModWord,primQuotWord,primRemWord)

PRIMS_BITLOGIC(Word,primAndWord,primOrWord,primXorWord)
PRIMS_BITSHIFT(Word,primComplementWord,primShiftLeftWord,primShiftRightWord,primRotateLeftWord,primRotateRightWord)

%%]

Word64

%%[99
PRIMS_BOUNDED(Word64,primMinWord64,primMaxWord64)
PRIMS_CONVERSION_INTEGER(Word64,primIntegerToWord64,primWord64ToInteger)
PRIMS_CONVERSION_INT(Word64,primIntToWord64,primWord64ToInt)

PRIMS_EQ(Word64,primEqWord64,primNeWord64)
PRIMS_ORD(Word64,primCmpWord64,primLtWord64,primGtWord64,primLeWord64,primGeWord64)
PRIMS_NUM(Word64,primAddWord64,primSubWord64,primMulWord64,primNegWord64)
PRIMS_INTEGRAL1(Word64,primDivWord64,primModWord64,primQuotWord64,primRemWord64)

PRIMS_BITLOGIC(Word64,primAndWord64,primOrWord64,primXorWord64)
PRIMS_BITSHIFT(Word64,primComplementWord64,primShiftLeftWord64,primShiftRightWord64,primRotateLeftWord64,primRotateRightWord64)

%%]

Word32

%%[99
PRIMS_BOUNDED(Word32,primMinWord32,primMaxWord32)
PRIMS_CONVERSION_INTEGER(Word32,primIntegerToWord32,primWord32ToInteger)
PRIMS_CONVERSION_INT(Word32,primIntToWord32,primWord32ToInt)

#if USE_32_BITS
PRIMS_EQ(Word32,primEqWord32,primNeWord32)
PRIMS_ORD(Word32,primCmpWord32,primLtWord32,primGtWord32,primLeWord32,primGeWord32)
PRIMS_NUM(Word32,primAddWord32,primSubWord32,primMulWord32,primNegWord32)
PRIMS_INTEGRAL1(Word32,primDivWord32,primModWord32,primQuotWord32,primRemWord32)
#else
PRIMS2_EQ(Word32,primEqWord32,"primEqWord",primNeWord32,"primNeWord")
PRIMS2_ORD(Word32,primCmpWord32,"primCmpWord",primLtWord32,"primLtWord",primGtWord32,"primGtWord",primLeWord32,"primLeWord",primGeWord32,"primGeWord")
PRIMS2_NUM(Word32,primAddWord32,"primAddWord",primSubWord32,"primSubWord",primMulWord32,"primMulWord",primNegWord32,"primNegWord")
PRIMS2_INTEGRAL1(Word32,primDivWord32,"primDivWord",primModWord32,"primModWord",primQuotWord32,"primQuotWord",primRemWord32,"primRemWord")
#endif

PRIMS2_BITLOGIC(Word32,primAndWord32,"primAndWord",primOrWord32,"primOrWord",primXorWord32,"primXorWord")
PRIMS_BITSHIFT(Word32,primComplementWord32,primShiftLeftWord32,primShiftRightWord32,primRotateLeftWord32,primRotateRightWord32)

%%]

Word16

%%[99
PRIMS_BOUNDED(Word16,primMinWord16,primMaxWord16)
PRIMS2_CONVERSION_INTEGER(Word16,primIntegerToWord16,"primIntegerToInt",primWord16ToInteger,"primIntToInteger")
PRIMS_CONVERSION_INT(Word16,primIntToWord16,primWord16ToInt)

PRIMS2_EQ(Word16,primEqWord16,"primEqInt",primNeWord16,"primNeInt")
PRIMS2_ORD(Word16,primCmpWord16,"primCmpInt",primLtWord16,"primLtInt",primGtWord16,"primGtInt",primLeWord16,"primLeInt",primGeWord16,"primGeInt")
PRIMS2_NUM(Word16,primAddWord16,"primAddInt",primSubWord16,"primSubInt",primMulWord16,"primMulInt",primNegWord16,"primNegInt")
PRIMS2_INTEGRAL1(Word16,primDivWord16,"primDivInt",primModWord16,"primModInt",primQuotWord16,"primQuotInt",primRemWord16,"primRemInt")

PRIMS2_BITLOGIC(Word16,primAndWord16,"primAndWord",primOrWord16,"primOrWord",primXorWord16,"primXorWord")
PRIMS_BITSHIFT(Word16,primComplementWord16,primShiftLeftWord16,primShiftRightWord16,primRotateLeftWord16,primRotateRightWord16)

%%]

Word8

%%[99
PRIMS_BOUNDED(Word8,primMinWord8,primMaxWord8)
PRIMS2_CONVERSION_INTEGER(Word8,primIntegerToWord8,"primIntegerToInt",primWord8ToInteger,"primIntToInteger")
PRIMS_CONVERSION_INT(Word8,primIntToWord8,primWord8ToInt)

PRIMS2_EQ(Word8,primEqWord8,"primEqInt",primNeWord8,"primNeInt")
PRIMS2_ORD(Word8,primCmpWord8,"primCmpInt",primLtWord8,"primLtInt",primGtWord8,"primGtInt",primLeWord8,"primLeInt",primGeWord8,"primGeInt")
PRIMS2_NUM(Word8,primAddWord8,"primAddInt",primSubWord8,"primSubInt",primMulWord8,"primMulInt",primNegWord8,"primNegInt")
PRIMS2_INTEGRAL1(Word8,primDivWord8,"primDivInt",primModWord8,"primModInt",primQuotWord8,"primQuotInt",primRemWord8,"primRemInt")

PRIMS2_BITLOGIC(Word8,primAndWord8,"primAndWord",primOrWord8,"primOrWord",primXorWord8,"primXorWord")
PRIMS_BITSHIFT(Word8,primComplementWord8,primShiftLeftWord8,primShiftRightWord8,primRotateLeftWord8,primRotateRightWord8)

%%]

Int64

%%[99
PRIMS_BOUNDED(Int64,primMinInt64,primMaxInt64)
PRIMS_CONVERSION_INTEGER(Int64,primIntegerToInt64,primInt64ToInteger)
PRIMS_CONVERSION_INT(Int64,primIntToInt64,primInt64ToInt)

PRIMS_EQ(Int64,primEqInt64,primNeInt64)
PRIMS_ORD(Int64,primCmpInt64,primLtInt64,primGtInt64,primLeInt64,primGeInt64)
PRIMS_NUM(Int64,primAddInt64,primSubInt64,primMulInt64,primNegInt64)
PRIMS_INTEGRAL1(Int64,primDivInt64,primModInt64,primQuotInt64,primRemInt64)

PRIMS_BITLOGIC(Int64,primAndInt64,primOrInt64,primXorInt64)
PRIMS_BITSHIFT(Int64,primComplementInt64,primShiftLeftInt64,primShiftRightInt64,primRotateLeftInt64,primRotateRightInt64)

%%]

Int32

%%[99
PRIMS_BOUNDED(Int32,primMinInt32,primMaxInt32)
PRIMS_CONVERSION_INTEGER(Int32,primIntegerToInt32,primInt32ToInteger)
PRIMS_CONVERSION_INT(Int32,primIntToInt32,primInt32ToInt)

#if USE_32_BITS
PRIMS_EQ(Int32,primEqInt32,primNeInt32)
PRIMS_ORD(Int32,primCmpInt32,primLtInt32,primGtInt32,primLeInt32,primGeInt32)
PRIMS_NUM(Int32,primAddInt32,primSubInt32,primMulInt32,primNegInt32)
PRIMS_INTEGRAL1(Int32,primDivInt32,primModInt32,primQuotInt32,primRemInt32)
#else
PRIMS2_EQ(Int32,primEqInt32,"primEqInt",primNeInt32,"primNeInt")
PRIMS2_ORD(Int32,primCmpInt32,"primCmpInt",primLtInt32,"primLtInt",primGtInt32,"primGtInt",primLeInt32,"primLeInt",primGeInt32,"primGeInt")
PRIMS2_NUM(Int32,primAddInt32,"primAddInt",primSubInt32,"primSubInt",primMulInt32,"primMulInt",primNegInt32,"primNegInt")
PRIMS2_INTEGRAL1(Int32,primDivInt32,"primDivInt",primModInt32,"primModInt",primQuotInt32,"primQuotInt",primRemInt32,"primRemInt")
#endif

PRIMS2_BITLOGIC(Int32,primAndInt32,"primAndWord",primOrInt32,"primOrWord",primXorInt32,"primXorWord")
PRIMS_BITSHIFT(Int32,primComplementInt32,primShiftLeftInt32,primShiftRightInt32,primRotateLeftInt32,primRotateRightInt32)

%%]

Int16

%%[99
PRIMS_BOUNDED(Int16,primMinInt16,primMaxInt16)
PRIMS2_CONVERSION_INTEGER(Int16,primIntegerToInt16,"primIntegerToInt",primInt16ToInteger,"primIntToInteger")
PRIMS_CONVERSION_INT(Int16,primIntToInt16,primInt16ToInt)

PRIMS2_EQ(Int16,primEqInt16,"primEqInt",primNeInt16,"primNeInt")
PRIMS2_ORD(Int16,primCmpInt16,"primCmpInt",primLtInt16,"primLtInt",primGtInt16,"primGtInt",primLeInt16,"primLeInt",primGeInt16,"primGeInt")
PRIMS2_NUM(Int16,primAddInt16,"primAddInt",primSubInt16,"primSubInt",primMulInt16,"primMulInt",primNegInt16,"primNegInt")
PRIMS2_INTEGRAL1(Int16,primDivInt16,"primDivInt",primModInt16,"primModInt",primQuotInt16,"primQuotInt",primRemInt16,"primRemInt")

PRIMS2_BITLOGIC(Int16,primAndInt16,"primAndWord",primOrInt16,"primOrWord",primXorInt16,"primXorWord")
PRIMS_BITSHIFT(Int16,primComplementInt16,primShiftLeftInt16,primShiftRightInt16,primRotateLeftInt16,primRotateRightInt16)

%%]

Int8

%%[99
PRIMS_BOUNDED(Int8,primMinInt8,primMaxInt8)
PRIMS2_CONVERSION_INTEGER(Int8,primIntegerToInt8,"primIntegerToInt",primInt8ToInteger,"primIntToInteger")
PRIMS_CONVERSION_INT(Int8,primIntToInt8,primInt8ToInt)

PRIMS2_EQ(Int8,primEqInt8,"primEqInt",primNeInt8,"primNeInt")
PRIMS2_ORD(Int8,primCmpInt8,"primCmpInt",primLtInt8,"primLtInt",primGtInt8,"primGtInt",primLeInt8,"primLeInt",primGeInt8,"primGeInt")
PRIMS2_NUM(Int8,primAddInt8,"primAddInt",primSubInt8,"primSubInt",primMulInt8,"primMulInt",primNegInt8,"primNegInt")
PRIMS2_INTEGRAL1(Int8,primDivInt8,"primDivInt",primModInt8,"primModInt",primQuotInt8,"primQuotInt",primRemInt8,"primRemInt")

PRIMS2_BITLOGIC(Int8,primAndInt8,"primAndWord",primOrInt8,"primOrWord",primXorInt8,"primXorWord")
PRIMS_BITSHIFT(Int8,primComplementInt8,primShiftLeftInt8,primShiftRightInt8,primRotateLeftInt8,primRotateRightInt8)

%%]

Addr

%%[99
#if USE_32_BITS
PRIMS2_EQ(Addr,primEqAddr,"primEqInt32",primNeAddr,"primNeInt32")
PRIMS2_ORD(Addr,primCmpAddr,"primCmpInt32",primLtAddr,"primLtInt32",primGtAddr,"primGtInt32",primLeAddr,"primLeInt32",primGeAddr,"primGeInt32")
#else
PRIMS2_EQ(Addr,primEqAddr,"primEqInt64",primNeAddr,"primNeInt64")
PRIMS2_ORD(Addr,primCmpAddr,"primCmpInt64",primLtAddr,"primLtInt64",primGtAddr,"primGtInt64",primLeAddr,"primLeInt64",primGeAddr,"primGeInt64")
#endif

%%]


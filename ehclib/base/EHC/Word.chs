%%[99
-- Compiler knows the Word types of this module are defined here

module EHC.Word
  ( Word, Word8, Word16, Word32, Word64
  )
  where

import EHC.Prelude
import EHC.Bits
import Data.Bits

#include "MachDeps.h"
#include "IntLikeInstance.h"

data Word
data Word8
data Word16
data Word32
data Word64
%%]

%%[99
PRIMS_BOUNDED(Word,primMinWord,primMaxWord)
PRIMS_CONVERSION_INTEGER(Word,primIntegerToWord,primWordToInteger)
PRIMS_CONVERSION_INT(Word,primIntToWord,primWordToInt)

PRIMS_EQ(Word,primEqWord)
PRIMS_ORD2(Word,primCmpWord,primLtWord,primGtWord)
PRIMS_NUM(Word,primAddWord,primSubWord,primMulWord,primNegWord)
PRIMS_INTEGRAL1(Word,primDivWord,primModWord,primQuotWord,primRemWord)

PRIMS_BITS(Word,primAndWord,primOrWord,primXorWord)
PRIMS_BITS_SIZEDPD(Word,primComplementWord,primShiftLeftWord,primShiftRightWord,primRotateLeftWord,primRotateRightWord)

INSTANCE_EQ(Word,primEqWord)
INSTANCE_ORD2(Word,primCmpWord,primLtWord,primGtWord)
INSTANCE_BOUNDED(Word,primMinWord,primMaxWord)
INSTANCE_NUM(Word,primAddWord,primSubWord,primMulWord,primNegWord,primIntegerToWord,primIntToWord)
INSTANCE_ENUM(Word,primIntToWord,primWordToInt)
INSTANCE_REAL(Word)
INSTANCE_INTEGRAL1(Word,primDivWord,primModWord,primQuotWord,primRemWord,primWordToInteger,primWordToInt)
INSTANCE_SHOW(Word)

INSTANCE_BITS1(Word,SIZEOF_HSWORD*8-BITSIZEOF_WORDTAG,False,primAndWord,primOrWord,primXorWord,primComplementWord,primShiftLeftWord,primShiftRightWord,primRotateLeftWord,primRotateRightWord)
%%]

%%[99
PRIMS_BOUNDED(Word64,primMinWord64,primMaxWord64)
PRIMS_CONVERSION_INTEGER(Word64,primIntegerToWord64,primWord64ToInteger)
PRIMS_CONVERSION_INT(Word64,primIntToWord64,primWord64ToInt)

PRIMS_EQ(Word64,primEqWord64)
PRIMS_ORD2(Word64,primCmpWord64,primLtWord64,primGtWord64)
PRIMS_NUM(Word64,primAddWord64,primSubWord64,primMulWord64,primNegWord64)
PRIMS_INTEGRAL1(Word64,primDivWord64,primModWord64,primQuotWord64,primRemWord64)

PRIMS_BITS(Word64,primAndWord64,primOrWord64,primXorWord64)
PRIMS_BITS_SIZEDPD(Word64,primComplementWord64,primShiftLeftWord64,primShiftRightWord64,primRotateLeftWord64,primRotateRightWord64)

INSTANCE_EQ(Word64,primEqWord64)
INSTANCE_ORD2(Word64,primCmpWord64,primLtWord64,primGtWord64)
INSTANCE_BOUNDED(Word64,primMinWord64,primMaxWord64)
INSTANCE_NUM(Word64,primAddWord64,primSubWord64,primMulWord64,primNegWord64,primIntegerToWord64,primIntToWord64)
INSTANCE_ENUM(Word64,primIntToWord64,primWord64ToInt)
INSTANCE_REAL(Word64)
INSTANCE_INTEGRAL1(Word64,primDivWord64,primModWord64,primQuotWord64,primRemWord64,primWord64ToInteger,primWord64ToInt)
INSTANCE_SHOW(Word64)

INSTANCE_BITS1(Word64,64,False,primAndWord64,primOrWord64,primXorWord64,primComplementWord64,primShiftLeftWord64,primShiftRightWord64,primRotateLeftWord64,primRotateRightWord64)
%%]

If possible (when 32 bits fit into Int), use Int stuff, otherwise boxed with additional primitives.

%%[99
PRIMS_BOUNDED(Word32,primMinWord32,primMaxWord32)
PRIMS_CONVERSION_INTEGER(Word32,primIntegerToWord32,primWord32ToInteger)
PRIMS_CONVERSION_INT(Word32,primIntToWord32,primWord32ToInt)

#if USE_32_BITS
PRIMS_EQ(Word32,primEqWord32)
PRIMS_ORD2(Word32,primCmpWord32,primLtWord32,primGtWord32)
PRIMS_NUM(Word32,primAddWord32,primSubWord32,primMulWord32,primNegWord32)
PRIMS_INTEGRAL1(Word32,primDivWord32,primModWord32,primQuotWord32,primRemWord32)
#else
PRIMS2_EQ(Word32,primEqWord32,"primEqWord")
PRIMS2_ORD2(Word32,primCmpWord32,"primCmpWord",primLtWord32,"primLtWord",primGtWord32,"primGtWord")
PRIMS2_NUM(Word32,primAddWord32,"primAddWord",primSubWord32,"primSubWord",primMulWord32,"primMulWord",primNegWord32,"primNegWord")
PRIMS2_INTEGRAL1(Word32,primDivWord32,"primDivWord",primModWord32,"primModWord",primQuotWord32,"primQuotWord",primRemWord32,"primRemWord")
#endif

PRIMS2_BITS(Word32,primAndWord32,"primAndWord",primOrWord32,"primOrWord",primXorWord32,"primXorWord")
PRIMS_BITS_SIZEDPD(Word32,primComplementWord32,primShiftLeftWord32,primShiftRightWord32,primRotateLeftWord32,primRotateRightWord32)

INSTANCE_EQ(Word32,primEqWord32)
INSTANCE_ORD2(Word32,primCmpWord32,primLtWord32,primGtWord32)
INSTANCE_BOUNDED(Word32,primMinWord32,primMaxWord32)
INSTANCE_NUM(Word32,primAddWord32,primSubWord32,primMulWord32,primNegWord32,primIntegerToWord32,primIntToWord32)
INSTANCE_ENUM(Word32,primIntToWord32,primWord32ToInt)
INSTANCE_REAL(Word32)
INSTANCE_INTEGRAL1(Word32,primDivWord32,primModWord32,primQuotWord32,primRemWord32,primWord32ToInteger,primWord32ToInt)
INSTANCE_SHOW(Word32)

INSTANCE_BITS1(Word32,32,False,primAndWord32,primOrWord32,primXorWord32,primComplementWord32,primShiftLeftWord32,primShiftRightWord32,primRotateLeftWord32,primRotateRightWord32)
%%]

%%[99
PRIMS_BOUNDED(Word16,primMinWord16,primMaxWord16)
PRIMS2_CONVERSION_INTEGER(Word16,primIntegerToWord16,"primIntegerToInt",primWord16ToInteger,"primIntToInteger")
PRIMS_CONVERSION_INT(Word16,primIntToWord16,primWord16ToInt)

PRIMS2_EQ(Word16,primEqWord16,"primEqInt")
PRIMS2_ORD2(Word16,primCmpWord16,"primCmpInt",primLtWord16,"primLtInt",primGtWord16,"primGtInt")
PRIMS2_NUM(Word16,primAddWord16,"primAddInt",primSubWord16,"primSubInt",primMulWord16,"primMulInt",primNegWord16,"primNegInt")
PRIMS2_INTEGRAL1(Word16,primDivWord16,"primDivInt",primModWord16,"primModInt",primQuotWord16,"primQuotInt",primRemWord16,"primRemInt")

PRIMS2_BITS(Word16,primAndWord16,"primAndWord",primOrWord16,"primOrWord",primXorWord16,"primXorWord")
PRIMS_BITS_SIZEDPD(Word16,primComplementWord16,primShiftLeftWord16,primShiftRightWord16,primRotateLeftWord16,primRotateRightWord16)

INSTANCE_EQ(Word16,primEqWord16)
INSTANCE_ORD2(Word16,primCmpWord16,primLtWord16,primGtWord16)
INSTANCE_BOUNDED(Word16,primMinWord16,primMaxWord16)
INSTANCE_NUM(Word16,primAddWord16,primSubWord16,primMulWord16,primNegWord16,primIntegerToWord16,primIntToWord16)
INSTANCE_ENUM(Word16,primIntToWord16,primWord16ToInt)
INSTANCE_REAL(Word16)
INSTANCE_INTEGRAL1(Word16,primDivWord16,primModWord16,primQuotWord16,primRemWord16,primWord16ToInteger,primWord16ToInt)
INSTANCE_SHOW(Word16)

INSTANCE_BITS1(Word16,16,False,primAndWord16,primOrWord16,primXorWord16,primComplementWord16,primShiftLeftWord16,primShiftRightWord16,primRotateLeftWord16,primRotateRightWord16)
%%]

%%[99
PRIMS_BOUNDED(Word8,primMinWord8,primMaxWord8)
PRIMS2_CONVERSION_INTEGER(Word8,primIntegerToWord8,"primIntegerToInt",primWord8ToInteger,"primIntToInteger")
PRIMS_CONVERSION_INT(Word8,primIntToWord8,primWord8ToInt)

PRIMS2_EQ(Word8,primEqWord8,"primEqInt")
PRIMS2_ORD2(Word8,primCmpWord8,"primCmpInt",primLtWord8,"primLtInt",primGtWord8,"primGtInt")
PRIMS2_NUM(Word8,primAddWord8,"primAddInt",primSubWord8,"primSubInt",primMulWord8,"primMulInt",primNegWord8,"primNegInt")
PRIMS2_INTEGRAL1(Word8,primDivWord8,"primDivInt",primModWord8,"primModInt",primQuotWord8,"primQuotInt",primRemWord8,"primRemInt")

PRIMS2_BITS(Word8,primAndWord8,"primAndWord",primOrWord8,"primOrWord",primXorWord8,"primXorWord")
PRIMS_BITS_SIZEDPD(Word8,primComplementWord8,primShiftLeftWord8,primShiftRightWord8,primRotateLeftWord8,primRotateRightWord8)

INSTANCE_EQ(Word8,primEqWord8)
INSTANCE_ORD2(Word8,primCmpWord8,primLtWord8,primGtWord8)
INSTANCE_BOUNDED(Word8,primMinWord8,primMaxWord8)
INSTANCE_NUM(Word8,primAddWord8,primSubWord8,primMulWord8,primNegWord8,primIntegerToWord8,primIntToWord8)
INSTANCE_ENUM(Word8,primIntToWord8,primWord8ToInt)
INSTANCE_REAL(Word8)
INSTANCE_INTEGRAL1(Word8,primDivWord8,primModWord8,primQuotWord8,primRemWord8,primWord8ToInteger,primWord8ToInt)
INSTANCE_SHOW(Word8)

INSTANCE_BITS1(Word8,8,False,primAndWord8,primOrWord8,primXorWord8,primComplementWord8,primShiftLeftWord8,primShiftRightWord8,primRotateLeftWord8,primRotateRightWord8)
%%]

%%[99
-- Compiler knows the Word types of this module are defined here

module UHC.Word
  ( Word, Word8, Word16, Word32, Word64
  )
  where

import UHC.Base
import UHC.Bits
import UHC.Types
import UHC.Prims
import UHC.Show
import Data.Bits

#include "MachDeps.h"
#include "IntLikeInstance.h"

%%]

%%[99
INSTANCE_EQ(Word,primEqWord,primNeWord)
INSTANCE_ORD(Word,primCmpWord,primLtWord,primGtWord,primLeWord,primGeWord)
INSTANCE_BOUNDED(Word,primMinWord,primMaxWord)
INSTANCE_NUM(Word,primAddWord,primSubWord,primMulWord,primNegWord,primIntegerToWord,primIntToWord)
INSTANCE_ENUM(Word,primIntToWord,primWordToInt)
INSTANCE_REAL(Word)
INSTANCE_INTEGRAL1(Word,primDivWord,primModWord,primQuotWord,primRemWord,primWordToInteger,primWordToInt)
INSTANCE_SHOW(Word)
INSTANCE_READ(Word)

INSTANCE_BITS1(Word,SIZEOF_HSWORD*8-BITSIZEOF_WORDTAG,False,primAndWord,primOrWord,primXorWord,primComplementWord,primShiftLeftWord,primShiftRightWord,primRotateLeftWord,primRotateRightWord)
%%]

%%[99
INSTANCE_EQ(Word64,primEqWord64,primNeWord64)
INSTANCE_ORD(Word64,primCmpWord64,primLtWord64,primGtWord64,primLeWord64,primGeWord64)
INSTANCE_BOUNDED(Word64,primMinWord64,primMaxWord64)
INSTANCE_NUM(Word64,primAddWord64,primSubWord64,primMulWord64,primNegWord64,primIntegerToWord64,primIntToWord64)
INSTANCE_ENUM(Word64,primIntToWord64,primWord64ToInt)
INSTANCE_REAL(Word64)
INSTANCE_INTEGRAL1(Word64,primDivWord64,primModWord64,primQuotWord64,primRemWord64,primWord64ToInteger,primWord64ToInt)
INSTANCE_SHOW(Word64)
INSTANCE_READ(Word64)

INSTANCE_BITS1(Word64,64,False,primAndWord64,primOrWord64,primXorWord64,primComplementWord64,primShiftLeftWord64,primShiftRightWord64,primRotateLeftWord64,primRotateRightWord64)
%%]

If possible (when 32 bits fit into Int), use Int stuff, otherwise boxed with additional primitives.

%%[99
INSTANCE_EQ(Word32,primEqWord32,primNeWord32)
INSTANCE_ORD(Word32,primCmpWord32,primLtWord32,primGtWord32,primLeWord32,primGeWord32)
INSTANCE_BOUNDED(Word32,primMinWord32,primMaxWord32)
INSTANCE_NUM(Word32,primAddWord32,primSubWord32,primMulWord32,primNegWord32,primIntegerToWord32,primIntToWord32)
INSTANCE_ENUM(Word32,primIntToWord32,primWord32ToInt)
INSTANCE_REAL(Word32)
INSTANCE_INTEGRAL1(Word32,primDivWord32,primModWord32,primQuotWord32,primRemWord32,primWord32ToInteger,primWord32ToInt)
INSTANCE_SHOW(Word32)
INSTANCE_READ(Word32)

INSTANCE_BITS1(Word32,32,False,primAndWord32,primOrWord32,primXorWord32,primComplementWord32,primShiftLeftWord32,primShiftRightWord32,primRotateLeftWord32,primRotateRightWord32)
%%]

%%[99
INSTANCE_EQ(Word16,primEqWord16,primNeWord16)
INSTANCE_ORD(Word16,primCmpWord16,primLtWord16,primGtWord16,primLeWord16,primGeWord16)
INSTANCE_BOUNDED(Word16,primMinWord16,primMaxWord16)
INSTANCE_NUM(Word16,primAddWord16,primSubWord16,primMulWord16,primNegWord16,primIntegerToWord16,primIntToWord16)
INSTANCE_ENUM(Word16,primIntToWord16,primWord16ToInt)
INSTANCE_REAL(Word16)
INSTANCE_INTEGRAL1(Word16,primDivWord16,primModWord16,primQuotWord16,primRemWord16,primWord16ToInteger,primWord16ToInt)
INSTANCE_SHOW(Word16)
INSTANCE_READ(Word16)

INSTANCE_BITS1(Word16,16,False,primAndWord16,primOrWord16,primXorWord16,primComplementWord16,primShiftLeftWord16,primShiftRightWord16,primRotateLeftWord16,primRotateRightWord16)
%%]

%%[99
INSTANCE_EQ(Word8,primEqWord8,primNeWord8)
INSTANCE_ORD(Word8,primCmpWord8,primLtWord8,primGtWord8,primLeWord8,primGeWord8)
INSTANCE_BOUNDED(Word8,primMinWord8,primMaxWord8)
INSTANCE_NUM(Word8,primAddWord8,primSubWord8,primMulWord8,primNegWord8,primIntegerToWord8,primIntToWord8)
INSTANCE_ENUM(Word8,primIntToWord8,primWord8ToInt)
INSTANCE_REAL(Word8)
INSTANCE_INTEGRAL1(Word8,primDivWord8,primModWord8,primQuotWord8,primRemWord8,primWord8ToInteger,primWord8ToInt)
INSTANCE_SHOW(Word8)
INSTANCE_READ(Word8)

INSTANCE_BITS1(Word8,8,False,primAndWord8,primOrWord8,primXorWord8,primComplementWord8,primShiftLeftWord8,primShiftRightWord8,primRotateLeftWord8,primRotateRightWord8)
%%]

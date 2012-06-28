{-# OPTIONS_GHC -cpp #-}

module EH101.ConfigDefines
( mkB
, GCVariant (..), gcVariant
, useBoehmGC
, sizeofWord, sizeofWordInBits, sizeofWordAsInteger, sizeofWordInLog
, nodeNeedsForwarding
, sizeofPointer
, sizeofGrWord, sizeofGrWordAsInteger
, gbLabelOffsetSize
, use64Bits, use32Bits
, machineIsBigEndian
, rtsGlobalVarPrefix, rtsUseGC
, magicNumberHI
, mpLib, MPLib (..)
, sizeofFloat, sizeofDouble, sizeofCInt
, isSameSizeForIntAndWord )
where
import EH101.Opts
import Data.Word
import Data.Char


{-# LINE 18 "src/ehc/ConfigDefines.chs" #-}
magicNumberHI :: [Word8]
magicNumberHI = map (fromInteger . toInteger . fromEnum) "UHI1"

{-# LINE 27 "src/ehc/ConfigDefines.chs" #-}
mkB :: Int -> Bool
mkB x = if x /= 0 then True else False

{-# LINE 39 "src/ehc/ConfigDefines.chs" #-}
#define USE_BOEHM_GC 0

data GCVariant
  = GCVariant_Boehm | GCVariant_Uhc
  deriving Eq

gcVariant :: EHCOpts -> GCVariant
gcVariant opts | ehcOptEmitExecBytecode opts = GCVariant_Uhc
               | mkB USE_BOEHM_GC            = GCVariant_Boehm
               | otherwise                   = GCVariant_Uhc

{-# LINE 56 "src/ehc/ConfigDefines.chs" #-}
useBoehmGC :: EHCOpts -> Bool
useBoehmGC opts = gcVariant opts == GCVariant_Boehm

{-# LINE 61 "src/ehc/ConfigDefines.chs" #-}
#define USE_LTM 1
#define USE_GMP 0

data MPLib
  = MPLib_LTM | MPLib_GMP
  deriving (Show,Eq)

mpLib :: MPLib
mpLib | mkB USE_GMP = MPLib_GMP
      | otherwise   = MPLib_LTM

{-# LINE 78 "src/ehc/ConfigDefines.chs" #-}
#define SIZEOF_INTPTR_T 8

{-# LINE 82 "src/ehc/ConfigDefines.chs" #-}
sizeofWord :: Int
sizeofWord = SIZEOF_INTPTR_T

sizeofWordInBits :: Int
sizeofWordInBits = sizeofWord * 8

sizeofWordInLog :: Int
sizeofWordInLog = if sizeofWord == 8 then 3 else 2

sizeofWordAsInteger :: Integer
sizeofWordAsInteger = toInteger sizeofWord

{-# LINE 100 "src/ehc/ConfigDefines.chs" #-}
nodeNeedsForwarding :: EHCOpts -> Bool
nodeNeedsForwarding opts
  = case gcVariant opts of
      GCVariant_Boehm -> False -- header only
      GCVariant_Uhc   -> True  -- header + 1 fld for forwarding

{-# LINE 112 "src/ehc/ConfigDefines.chs" #-}
#define SIZEOF_FLOAT 4
#define SIZEOF_DOUBLE 8
#define SIZEOF_INT 4

{-# LINE 118 "src/ehc/ConfigDefines.chs" #-}
sizeofFloat :: Int
sizeofFloat = SIZEOF_FLOAT

sizeofDouble :: Int
sizeofDouble = SIZEOF_DOUBLE

sizeofCInt :: Int
sizeofCInt = SIZEOF_INT

{-# LINE 133 "src/ehc/ConfigDefines.chs" #-}
sizeofPointer :: Int
sizeofPointer = sizeofWord

{-# LINE 138 "src/ehc/ConfigDefines.chs" #-}
sizeofGrWord :: Int
sizeofGrWord = sizeofWord

sizeofGrWordAsInteger :: Integer
sizeofGrWordAsInteger = sizeofWordAsInteger


{-# LINE 147 "src/ehc/ConfigDefines.chs" #-}
gbLabelOffsetSize :: Int
gbLabelOffsetSize = sizeofGrWord

{-# LINE 156 "src/ehc/ConfigDefines.chs" #-}
-- which size is used for word?
use64Bits, use32Bits :: Bool
(use64Bits,use32Bits)
  = if sizeofGrWord == 8
    then (True,False)
    else (False,True)

{-# LINE 165 "src/ehc/ConfigDefines.chs" #-}
-- are sizes of machine int and word same?
isSameSizeForIntAndWord :: Bool
isSameSizeForIntAndWord = sizeofCInt == sizeofWord

{-# LINE 175 "src/ehc/ConfigDefines.chs" #-}
#define BIGENDIAN 0
#define LITTLEENDIAN 1

machineIsBigEndian :: Bool
machineIsBigEndian = mkB BIGENDIAN

{-# LINE 187 "src/ehc/ConfigDefines.chs" #-}
#define RTS_GLOBAL_VAR_PREFIX "global_"
#define USE_BOEHM_GC 0

rtsGlobalVarPrefix :: String
rtsGlobalVarPrefix = RTS_GLOBAL_VAR_PREFIX

rtsUseGC :: Bool
rtsUseGC = mkB USE_BOEHM_GC


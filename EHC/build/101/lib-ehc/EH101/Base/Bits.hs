module EH101.Base.Bits
( Seq.empty
, entierLogUpShrBy, entierLogUpBy
, entierUpShrBy, entierUpBy
, pow2
, pow2_7, pow2_7neg, pow2_7sub1, pow2_8, pow2_8neg, pow2_8sub1
, pow2_15, pow2_15neg, pow2_15sub1, pow2_16, pow2_16neg, pow2_16sub1
, pow2_31, pow2_31neg, pow2_31sub1, pow2_32, pow2_32neg, pow2_32sub1
, pow2_63, pow2_63neg, pow2_63sub1, pow2_64, pow2_64neg, pow2_64sub1
, mask2_8, mask2_16, mask2_32, mask2_64
, posFits8
, ensurePositive
, signedFitsInBits
, ChooseBitSize, chooseOnBitsize
, Byte, Bytes
, bytesSize
, Byteable (..)
, (##), bytesUnions
, u1, u2, u4, u8
, bytesToString
, stringToBytes )
where
import Data.Word
import Data.Int
import Data.Bits
import Data.Char
import qualified EH.Util.FastSeq as Seq

{-# LINE 18 "src/ehc/Base/Bits.chs" #-}
entierLogUpShrBy :: Bits x => Int -> x -> x
entierLogUpShrBy by x = ((x - 1) `shiftR` by) + 1

entierLogUpBy :: Bits x => Int -> x -> x
entierLogUpBy by x = entierLogUpShrBy by x `shiftL` by

{-# LINE 26 "src/ehc/Base/Bits.chs" #-}
entierUpShrBy :: Integral x => x -> x -> x
entierUpShrBy by x = ((x - 1) `div` by) + 1

entierUpBy :: Integral x => x -> x -> x
entierUpBy by x = entierUpShrBy by x * by

{-# LINE 42 "src/ehc/Base/Bits.chs" #-}
pow2 :: Bits x => Int -> x
pow2 x = 1 `shiftL` x

pow2' :: Int -> (Integer,Integer,Integer)
pow2' x
  = (p,-p,p-1)
  where p = pow2 x

{-# LINE 52 "src/ehc/Base/Bits.chs" #-}
(pow2_7 ,pow2_7neg ,pow2_7sub1 ) = pow2' 7
(pow2_8 ,pow2_8neg ,pow2_8sub1 ) = pow2' 8

{-# LINE 57 "src/ehc/Base/Bits.chs" #-}
(pow2_15,pow2_15neg,pow2_15sub1) = pow2' 15
(pow2_16,pow2_16neg,pow2_16sub1) = pow2' 16

{-# LINE 62 "src/ehc/Base/Bits.chs" #-}
(pow2_31,pow2_31neg,pow2_31sub1) = pow2' 31
(pow2_32,pow2_32neg,pow2_32sub1) = pow2' 32

{-# LINE 67 "src/ehc/Base/Bits.chs" #-}
(pow2_63,pow2_63neg,pow2_63sub1) = pow2' 63
(pow2_64,pow2_64neg,pow2_64sub1) = pow2' 64

{-# LINE 72 "src/ehc/Base/Bits.chs" #-}
mask2 :: Bits x => Int -> x
mask2 x = pow2 x - 1

{-# LINE 77 "src/ehc/Base/Bits.chs" #-}
shift2 :: [Int]
shift2 = [8,16,32,64]

{-# LINE 82 "src/ehc/Base/Bits.chs" #-}
[mask2_8,mask2_16,mask2_32,mask2_64] = map mask2 shift2

{-# LINE 86 "src/ehc/Base/Bits.chs" #-}
posFits8 :: Integral c => c -> Bool
posFits8 x = toInteger x < pow2_8

{-# LINE 95 "src/ehc/Base/Bits.chs" #-}
ensurePositive :: Int -> Integer -> Integer
ensurePositive nrBits x | x < 0     = x + pow2 nrBits
                        | otherwise = x

{-# LINE 105 "src/ehc/Base/Bits.chs" #-}
signedFitsInBits :: Int -> Integer -> Bool
signedFitsInBits nrBits x = x >= neg && x < sub1
  where (_,neg,sub1) = pow2' (nrBits-1)

{-# LINE 115 "src/ehc/Base/Bits.chs" #-}
type ChooseBitSize r = Integer -> r

chooseOnBitsize
  :: Integral c
       => ( ChooseBitSize r
          , ChooseBitSize r
          , ChooseBitSize r
          , ChooseBitSize r
          , ChooseBitSize r
          , ChooseBitSize r
          , ChooseBitSize r
          , ChooseBitSize r
          ) -> c -> r
chooseOnBitsize (neg8,neg16,neg32,neg64,pos8,pos16,pos32,pos64) c
  = case toInteger c of
      i | i <  0 -> s (i .&. m)
                 where (s,m) = case i of
                                 i | i >= pow2_7neg  -> (neg8 ,mask2_8 )
                                   | i >= pow2_15neg -> (neg16,mask2_16)
                                   | i >= pow2_31neg -> (neg32,mask2_32)
                                   | otherwise       -> (neg64,mask2_64)
      i | i >= 0 -> s (i .&. mask2_64)
                 where s = case i of
                             i | i < pow2_7sub1  -> pos8
                               | i < pow2_15sub1 -> pos16
                               | i < pow2_31sub1 -> pos32
                               | otherwise       -> pos64

{-# LINE 149 "src/ehc/Base/Bits.chs" #-}
type Byte = Char
type Bytes = Seq.Seq Byte

{-# LINE 158 "src/ehc/Base/Bits.chs" #-}
bytesSize :: Bytes -> Int
bytesSize = Seq.size

{-# LINE 167 "src/ehc/Base/Bits.chs" #-}
class Byteable x where
  bytes :: x -> Bytes

instance Byteable Byte where
  bytes = Seq.singleton

instance Byteable Bytes where
  bytes = id

instance Byteable x => Byteable [x] where
  bytes = bytesUnions . map bytes

{-# LINE 185 "src/ehc/Base/Bits.chs" #-}
infixr 3 ##

(##) :: (Byteable x, Byteable y) => x -> y -> Bytes
x ## y = bytes x `Seq.union` bytes y

bytesUnions :: [Bytes] -> Bytes
bytesUnions = Seq.unions

{-# LINE 199 "src/ehc/Base/Bits.chs" #-}
-- ux 0 x = Seq.singleton $ chr $ fromIntegral x .&. mask2_8

{-# LINE 203 "src/ehc/Base/Bits.chs" #-}
u1 :: Integral x => x -> Bytes
u1 x = Seq.singleton $ chr $ fromIntegral x .&. 0xFF

u2 :: (Integral x, Bits x) => x -> Bytes
u2 x = u1 x1 ## u1 x2
     where (x1,x2) = (x .&. 0xFFFF) `divMod` 0x100

u4 :: (Integral x, Bits x) => x -> Bytes
u4 x = u2 x1 ## u2 x2
     where (x1,x2) = (x .&. 0xFFFFFFFF) `divMod` 0x10000

u8 :: (Integral x, Bits x) => x -> Bytes
u8 x = u4 x1 ## u4 x2
     where (x1,x2) = x `divMod` 0x100000000

{-# LINE 224 "src/ehc/Base/Bits.chs" #-}
bytesToString :: Bytes -> String
bytesToString = Seq.toList

{-# LINE 229 "src/ehc/Base/Bits.chs" #-}
stringToBytes :: String -> Bytes
stringToBytes = Seq.unions . map (u1 . ord)


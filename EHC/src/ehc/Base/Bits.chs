%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface to binary file by compositionally glueing bytes (and words, ...)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) module {%{EH}Base.Bits}
%%]

%%[(8 codegen) import(Data.Word,Data.Int,Data.Bits,Data.Char)
%%]
%%[(8 codegen) import(qualified EH.Util.FastSeq as Seq) export(Seq.empty)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bit fiddling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(entierLogUpShrBy,entierLogUpBy)
entierLogUpShrBy :: Bits x => Int -> x -> x
entierLogUpShrBy by x = ((x - 1) `shiftR` by) + 1

entierLogUpBy :: Bits x => Int -> x -> x
entierLogUpBy by x = entierLogUpShrBy by x `shiftL` by
%%]

%%[(8 codegen) export(entierUpShrBy,entierUpBy)
entierUpShrBy :: Integral x => x -> x -> x
entierUpShrBy by x = ((x - 1) `div` by) + 1

entierUpBy :: Integral x => x -> x -> x
entierUpBy by x = entierUpShrBy by x * by
%%]

#define EntierLogUpShrBy(x,m)			((((x)-1)>>(m))+1)
#define EntierLogUpBy(x,m)				(EntierLogUpShrBy(x,m)<<(m))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(pow2)
pow2 :: Bits x => Int -> x
pow2 x = 1 `shiftL` x

pow2' :: Int -> (Integer,Integer,Integer)
pow2' x
  = (p,-p,p-1)
  where p = pow2 x
%%]

%%[(8 codegen) export(pow2_7 ,pow2_7neg ,pow2_7sub1, pow2_8 ,pow2_8neg ,pow2_8sub1 )
(pow2_7 ,pow2_7neg ,pow2_7sub1 ) = pow2' 7
(pow2_8 ,pow2_8neg ,pow2_8sub1 ) = pow2' 8
%%]

%%[(8 codegen) export(pow2_15,pow2_15neg,pow2_15sub1, pow2_16,pow2_16neg,pow2_16sub1)
(pow2_15,pow2_15neg,pow2_15sub1) = pow2' 15
(pow2_16,pow2_16neg,pow2_16sub1) = pow2' 16
%%]

%%[(8 codegen) export(pow2_31,pow2_31neg,pow2_31sub1, pow2_32,pow2_32neg,pow2_32sub1)
(pow2_31,pow2_31neg,pow2_31sub1) = pow2' 31
(pow2_32,pow2_32neg,pow2_32sub1) = pow2' 32
%%]

%%[(8 codegen) export(pow2_63,pow2_63neg,pow2_63sub1, pow2_64,pow2_64neg,pow2_64sub1)
(pow2_63,pow2_63neg,pow2_63sub1) = pow2' 63
(pow2_64,pow2_64neg,pow2_64sub1) = pow2' 64
%%]

%%[(8 codegen)
mask2 :: Bits x => Int -> x
mask2 x = pow2 x - 1
%%]

%%[(8 codegen)
shift2 :: [Int]
shift2 = [8,16,32,64]
%%]

%%[(8 codegen) export(mask2_8,mask2_16,mask2_32,mask2_64)
[mask2_8,mask2_16,mask2_32,mask2_64] = map mask2 shift2
%%]

%%[(8 codegen) export(posFits8)
posFits8 :: Integral c => c -> Bool
posFits8 x = toInteger x < pow2_8
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Wrap integer w.r.t. sign, i.e. make >=0 representation of <0 int
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(ensurePositive)
ensurePositive :: Int -> Integer -> Integer
ensurePositive nrBits x | x < 0     = x + pow2 nrBits
                        | otherwise = x
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Check whether fits in given nr of bits
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(signedFitsInBits)
signedFitsInBits :: Int -> Integer -> Bool
signedFitsInBits nrBits x = x >= neg && x < sub1
  where (_,neg,sub1) = pow2' (nrBits-1)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constructing: immediate operands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(ChooseBitSize,chooseOnBitsize)
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
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Type
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(Byte,Bytes)
type Byte = Char
type Bytes = Seq.Seq Byte
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Observation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(bytesSize)
bytesSize :: Bytes -> Int
bytesSize = Seq.size
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Class for constructing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(Byteable(..))
class Byteable x where
  bytes :: x -> Bytes

instance Byteable Byte where
  bytes = Seq.singleton

instance Byteable Bytes where
  bytes = id

instance Byteable x => Byteable [x] where
  bytes = bytesUnions . map bytes
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Combination
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export((##),bytesUnions)
infixr 3 ##

(##) :: (Byteable x, Byteable y) => x -> y -> Bytes
x ## y = bytes x `Seq.union` bytes y

bytesUnions :: [Bytes] -> Bytes
bytesUnions = Seq.unions
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Basic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen)
-- ux 0 x = Seq.singleton $ chr $ fromIntegral x .&. mask2_8
%%]

%%[(8 codegen) export(u1,u2,u4,u8)
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
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conversion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(bytesToString)
bytesToString :: Bytes -> String
bytesToString = Seq.toList
%%]

%%[(8 codegen) export(stringToBytes)
stringToBytes :: String -> Bytes
stringToBytes = Seq.unions . map (u1 . ord)
%%]


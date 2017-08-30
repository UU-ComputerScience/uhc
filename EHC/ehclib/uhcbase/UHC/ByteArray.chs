%%[doesWhat doclatex
Interface to ByteArray, in several varieties: mutable and immutable.
Offering the ByteArray related primitives from GHC.Prim.

Content is not garbage collected, just bytes.
For arrays with GC content see UHC.BoxArray.
%%]

%%[99
{-# LANGUAGE NoImplicitPrelude, CPP #-}
{-# EXCLUDE_IF_TARGET cr #-}
{-# LANGUAGE BangPatterns #-}

module UHC.ByteArray
  ( MutableByteArray
  
  , newByteArray, newPinnedByteArray
  , sizeofByteArray, sizeofMutableByteArray
  , byteArrayContents
  , mutableByteArrayContents, mutableByteArrayPtr
  , unsafeFreezeByteArray
  
  , indexCharArray, indexWideCharArray, indexUnicodeCharArray
  , indexIntArray, indexWordArray, indexAddrArray
#if !defined( __UHC_TARGET_JS__ )
  , indexStablePtrArray
#endif
  , indexFloatArray, indexDoubleArray
  , indexInt8Array, indexInt16Array, indexInt32Array, indexInt64Array
  , indexWord8Array, indexWord16Array, indexWord32Array, indexWord64Array

  , readCharArray, readWideCharArray, readUnicodeCharArray
  , readIntArray, readWordArray, readAddrArray
#if !defined( __UHC_TARGET_JS__ )
  , readStablePtrArray
#endif
  , readFloatArray, readDoubleArray
  , readInt8Array, readInt16Array, readInt32Array, readInt64Array
  , readWord8Array, readWord16Array, readWord32Array, readWord64Array

  , writeCharArray, writeWideCharArray, writeUnicodeCharArray
  , writeIntArray, writeWordArray, writeAddrArray
#if !defined( __UHC_TARGET_JS__ )
  , writeStablePtrArray
#endif
  , writeFloatArray, writeDoubleArray
  , writeInt8Array, writeInt16Array, writeInt32Array, writeInt64Array
  , writeWord8Array, writeWord16Array, writeWord32Array, writeWord64Array
  -- , traceMBA
  
  )
  where

import UHC.Base
import UHC.Ptr
#if !defined( __UHC_TARGET_JS__ )
import UHC.StablePtr
#endif
import UHC.Types

#include "MachDeps.h"

%%]

%%[99
newtype MutableByteArray s = MutableByteArray ByteArray

{-
-- This function is here for debugging purpose. It prints to the stderr n positions of the given array.
traceMBA :: Int -> MutableByteArray s -> IO ()
traceMBA n (MutableByteArray arr) = primByteArrayTrace n arr

foreign import prim primByteArrayTrace :: Int -> ByteArray -> IO ()
-}
%%]

%%[99
foreign import prim primNewByteArray :: Int -> ByteArray
foreign import prim primNewPinnedByteArray :: Int -> ByteArray
foreign import prim primSizeofByteArray :: ByteArray -> Int
foreign import prim primByteArrayContents :: ByteArray -> Addr
%%]

%%[99
-- |Create a new mutable byte array of specified size (in bytes), in
--     the specified state thread.

newByteArray :: Int -> State s -> (State s, MutableByteArray s)
newByteArray sz s = (s, MutableByteArray (primNewByteArray sz))

-- |Create a mutable byte array that the GC guarantees not to move.

newPinnedByteArray :: Int -> State s -> (State s, MutableByteArray s)
newPinnedByteArray sz s = (s, MutableByteArray (primNewPinnedByteArray sz))

-- |Intended for use with pinned arrays; otherwise very unsafe!

byteArrayContents :: ByteArray -> Addr
byteArrayContents = primByteArrayContents

mutableByteArrayContents :: MutableByteArray s -> Addr
mutableByteArrayContents (MutableByteArray a) = byteArrayContents a

mutableByteArrayPtr :: MutableByteArray s -> Ptr a
mutableByteArrayPtr a = Ptr (mutableByteArrayContents a)

-- |Make a mutable byte array immutable, without copying.

unsafeFreezeByteArray :: MutableByteArray s -> State s -> (State s, ByteArray)
unsafeFreezeByteArray (MutableByteArray a) s = (s,a)

sizeofByteArray :: ByteArray -> Int
sizeofByteArray = primSizeofByteArray

sizeofMutableByteArray :: MutableByteArray s -> Int
sizeofMutableByteArray (MutableByteArray a) = primSizeofByteArray a
%%]

%%[99
-- |Read 8-bit character; offset in bytes.

foreign import prim "primIndexWord8Array" indexCharArray :: ByteArray -> Int -> Char

-- |Read 31-bit character; offset in 4-byte words.

foreign import prim "primIndexWord32Array" indexWideCharArray :: ByteArray -> Int -> Char
foreign import prim "primIndexUnicodeArray" indexUnicodeCharArray :: ByteArray -> Int -> (Char,Int)

#if USE_64_BITS
foreign import prim "primIndexWord64Array" 	indexIntArray 		:: ByteArray -> Int -> Int
foreign import prim "primIndexWord64Array" 	indexWordArray 		:: ByteArray -> Int -> Word
foreign import prim "primIndexWord64Array" 	indexAddrArray 		:: ByteArray -> Int -> Addr
#else
foreign import prim "primIndexWord32Array" 	indexIntArray 		:: ByteArray -> Int -> Int
foreign import prim "primIndexWord32Array" 	indexWordArray 		:: ByteArray -> Int -> Word
foreign import prim "primIndexWord32Array" 	indexAddrArray 		:: ByteArray -> Int -> Addr
#endif

foreign import prim "primIndexFloatArray" 	indexFloatArray 	:: ByteArray -> Int -> Float
foreign import prim "primIndexDoubleArray" 	indexDoubleArray 	:: ByteArray -> Int -> Double

foreign import prim "primIndexWord8Array" 	indexInt8Array 		:: ByteArray -> Int -> Int8
foreign import prim "primIndexWord16Array" 	indexInt16Array 	:: ByteArray -> Int -> Int16
foreign import prim "primIndexWord32Array" 	indexInt32Array 	:: ByteArray -> Int -> Int32
foreign import prim "primIndexWord64Array" 	indexInt64Array 	:: ByteArray -> Int -> Int64

foreign import prim "primIndexWord8Array" 	indexWord8Array 	:: ByteArray -> Int -> Word8
foreign import prim "primIndexWord16Array" 	indexWord16Array 	:: ByteArray -> Int -> Word16
foreign import prim "primIndexWord32Array" 	indexWord32Array 	:: ByteArray -> Int -> Word32
foreign import prim "primIndexWord64Array" 	indexWord64Array 	:: ByteArray -> Int -> Word64

%%]

%%[99
#if !defined( __UHC_TARGET_JS__ )
indexStablePtrArray :: forall s . ByteArray -> Int -> StablePtr s
indexStablePtrArray a i = let !x = indexAddrArray a i in StablePtr x
#endif
%%]

%%[99
-- | Read 8-bit character; offset in bytes.
readCharArray :: MutableByteArray s -> Int -> State s -> ( State s,Char )
readCharArray (MutableByteArray a) i s = let !x = indexCharArray a i in (s, x)

-- | Read 31-bit character; offset in 4-byte words.
readWideCharArray :: MutableByteArray s -> Int -> State s -> ( State s,Char )
readWideCharArray (MutableByteArray a) i s = let !x = indexWideCharArray a i in (s, x)

-- | Read unicode character; offset in bytes.
readUnicodeCharArray :: MutableByteArray s -> Int -> State s -> ( State s, (Char, Int) )
readUnicodeCharArray (MutableByteArray a) i s = let !x = indexUnicodeCharArray a i in (s, x)

readIntArray :: MutableByteArray s -> Int -> State s -> ( State s,Int )
readIntArray (MutableByteArray a) i s = let !x = indexIntArray a i in (s, x)

readWordArray :: MutableByteArray s -> Int -> State s -> ( State s,Word )
readWordArray (MutableByteArray a) i s = let !x = indexWordArray a i in (s, x)

readAddrArray :: MutableByteArray s -> Int -> State s -> ( State s,Addr )
readAddrArray (MutableByteArray a) i s = let !x = indexAddrArray a i in (s, x)

readFloatArray :: MutableByteArray s -> Int -> State s -> ( State s,Float )
readFloatArray (MutableByteArray a) i s = let !x = indexFloatArray a i in (s, x)

readDoubleArray :: MutableByteArray s -> Int -> State s -> ( State s,Double )
readDoubleArray (MutableByteArray a) i s = let !x = indexDoubleArray a i in (s, x)

#if !defined( __UHC_TARGET_JS__ )
readStablePtrArray :: MutableByteArray s -> Int -> State s -> ( State s,StablePtr s )
readStablePtrArray (MutableByteArray a) i s = let !x = indexStablePtrArray a i in (s, x)
#endif

readInt8Array :: MutableByteArray s -> Int -> State s -> ( State s,Int8 )
readInt8Array (MutableByteArray a) i s = let !x = indexInt8Array a i in (s, x)

readInt16Array :: MutableByteArray s -> Int -> State s -> ( State s,Int16 )
readInt16Array (MutableByteArray a) i s = let !x = indexInt16Array a i in (s, x)

readInt32Array :: MutableByteArray s -> Int -> State s -> ( State s,Int32 )
readInt32Array (MutableByteArray a) i s = let !x = indexInt32Array a i in (s, x)

readInt64Array :: MutableByteArray s -> Int -> State s -> ( State s,Int64 )
readInt64Array (MutableByteArray a) i s = let !x = indexInt64Array a i in (s, x)

readWord8Array :: MutableByteArray s -> Int -> State s -> ( State s,Word8 )
readWord8Array (MutableByteArray a) i s = let !x = indexWord8Array a i in (s, x)

readWord16Array :: MutableByteArray s -> Int -> State s -> ( State s,Word16 )
readWord16Array (MutableByteArray a) i s = let !x = indexWord16Array a i in (s, x)

readWord32Array :: MutableByteArray s -> Int -> State s -> ( State s,Word32 )
readWord32Array (MutableByteArray a) i s = let !x = indexWord32Array a i in (s, x)

readWord64Array :: MutableByteArray s -> Int -> State s -> ( State s,Word64 )
readWord64Array (MutableByteArray a) i s = let !x = indexWord64Array a i in (s, x)

%%]

%%[99
foreign import prim "primWriteWord8Array"  primWriteCharArray 		:: ByteArray -> Int -> Char -> ()

foreign import prim "primWriteWord32Array" primWriteWideCharArray 	:: ByteArray -> Int -> Char -> ()

#if USE_64_BITS
foreign import prim "primWriteWord64Array" primWriteIntArray 		:: ByteArray -> Int -> Int -> ()
foreign import prim "primWriteWord64Array" primWriteWordArray 		:: ByteArray -> Int -> Word -> ()
foreign import prim "primWriteWord64Array" primWriteAddrArray 		:: ByteArray -> Int -> Addr -> ()
#else
foreign import prim "primWriteWord32Array" primWriteIntArray 		:: ByteArray -> Int -> Int -> ()
foreign import prim "primWriteWord32Array" primWriteWordArray 		:: ByteArray -> Int -> Word -> ()
foreign import prim "primWriteWord32Array" primWriteAddrArray 		:: ByteArray -> Int -> Addr -> ()
#endif

foreign import prim "primWriteFloatArray"  primWriteFloatArray 		:: ByteArray -> Int -> Float -> ()
foreign import prim "primWriteDoubleArray" primWriteDoubleArray 	:: ByteArray -> Int -> Double -> ()

foreign import prim "primWriteWord8Array"  primWriteInt8Array 		:: ByteArray -> Int -> Int8 -> ()
foreign import prim "primWriteWord16Array" primWriteInt16Array 		:: ByteArray -> Int -> Int16 -> ()
foreign import prim "primWriteWord32Array" primWriteInt32Array 		:: ByteArray -> Int -> Int32 -> ()
foreign import prim "primWriteWord64Array" primWriteInt64Array 		:: ByteArray -> Int -> Int64 -> ()

foreign import prim "primWriteWord8Array"  primWriteWord8Array 		:: ByteArray -> Int -> Word8 -> ()
foreign import prim "primWriteWord16Array" primWriteWord16Array		:: ByteArray -> Int -> Word16 -> ()
foreign import prim "primWriteWord32Array" primWriteWord32Array 	:: ByteArray -> Int -> Word32 -> ()
foreign import prim "primWriteWord64Array" primWriteWord64Array 	:: ByteArray -> Int -> Word64 -> ()

foreign import prim "primWriteUnicodeArray" primWriteUnicodeArray 	:: ByteArray -> Int -> Char -> Int
%%]

%%[99
-- |Write 8-bit character; offset in bytes.

writeCharArray :: MutableByteArray s -> Int -> Char -> State s -> State s
writeCharArray (MutableByteArray a) i x s = let !_ = primWriteCharArray a i x in s

-- |Write 31-bit character; offset in 4-byte words.

writeUnicodeCharArray :: MutableByteArray s -> Int -> Char -> State s -> (State s,Int)
writeUnicodeCharArray (MutableByteArray a) i x s = let !n = primWriteUnicodeArray a i x in (s,n)

writeWideCharArray :: MutableByteArray s -> Int -> Char -> State s -> State s
writeWideCharArray (MutableByteArray a) i x s = let !_ = primWriteWideCharArray a i x in s

writeIntArray :: MutableByteArray s -> Int -> Int -> State s -> State s
writeIntArray (MutableByteArray a) i x s = let !_ = primWriteIntArray a i x in s

writeWordArray :: MutableByteArray s -> Int -> Word -> State s -> State s
writeWordArray (MutableByteArray a) i x s = let !_ = primWriteWordArray a i x in s

writeAddrArray :: MutableByteArray s -> Int -> Addr -> State s -> State s
writeAddrArray (MutableByteArray a) i x s = let !_ = primWriteAddrArray a i x in s

writeFloatArray :: MutableByteArray s -> Int -> Float -> State s -> State s
writeFloatArray (MutableByteArray a) i x s = let !_ = primWriteFloatArray a i x in s

writeDoubleArray :: MutableByteArray s -> Int -> Double -> State s -> State s
writeDoubleArray (MutableByteArray a) i x s = let !_ = primWriteDoubleArray a i x in s

#if !defined( __UHC_TARGET_JS__ )
writeStablePtrArray :: MutableByteArray s -> Int -> StablePtr s -> State s -> State s
writeStablePtrArray (MutableByteArray a) i (StablePtr x) s = let !_ = primWriteAddrArray a i x in s
#endif

writeInt8Array :: MutableByteArray s -> Int -> Int8 -> State s -> State s
writeInt8Array (MutableByteArray a) i x s = let !_ = primWriteInt8Array a i x in s

writeInt16Array :: MutableByteArray s -> Int -> Int16 -> State s -> State s
writeInt16Array (MutableByteArray a) i x s = let !_ = primWriteInt16Array a i x in s

writeInt32Array :: MutableByteArray s -> Int -> Int32 -> State s -> State s
writeInt32Array (MutableByteArray a) i x s = let !_ = primWriteInt32Array a i x in s

writeInt64Array :: MutableByteArray s -> Int -> Int64 -> State s -> State s
writeInt64Array (MutableByteArray a) i x s = let !_ = primWriteInt64Array a i x in s

writeWord8Array :: MutableByteArray s -> Int -> Word8 -> State s -> State s
writeWord8Array (MutableByteArray a) i x s = let !_ = primWriteWord8Array a i x in s

writeWord16Array :: MutableByteArray s -> Int -> Word16 -> State s -> State s
writeWord16Array (MutableByteArray a) i x s = let !_ = primWriteWord16Array a i x in s

writeWord32Array :: MutableByteArray s -> Int -> Word32 -> State s -> State s
writeWord32Array (MutableByteArray a) i x s = let !_ = primWriteWord32Array a i x in s

writeWord64Array :: MutableByteArray s -> Int -> Word64 -> State s -> State s
writeWord64Array (MutableByteArray a) i x s = let !_ = primWriteWord64Array a i x in s

%%]



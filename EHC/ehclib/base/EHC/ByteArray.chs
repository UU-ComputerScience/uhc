%%[doesWhat doclatex
Interface to ByteArray, in several varieties: mutable and immutable.
Offering the ByteArray related primitives from GHC.Prim.
%%]

%%[99
module EHC.ByteArray
  ( MutableByteArray
  
  , newByteArray, sizeofByteArray
  
  , sizeofMutableByteArray
  
  , indexCharArray, indexWideCharArray
  , indexIntArray, indexWordArray, indexAddrArray, indexStablePtrArray
  , indexFloatArray, indexDoubleArray
  , indexInt8Array, indexInt16Array, indexInt32Array, indexInt64Array
  , indexWord8Array, indexWord16Array, indexWord32Array, indexWord64Array

  , readCharArray, readWideCharArray
  , readIntArray, readWordArray, readAddrArray, readStablePtrArray
  , readFloatArray, readDoubleArray
  , readInt8Array, readInt16Array, readInt32Array, readInt64Array
  , readWord8Array, readWord16Array, readWord32Array, readWord64Array

  )
  where

import EHC.Prelude
import EHC.Ptr
import EHC.StablePtr
import EHC.Word
import EHC.Int

#include "MachDeps.h"

%%]

%%[99
newtype MutableByteArray s = MutableByteArray ByteArray
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

#if USE_64_BITS
foreign import prim "primIndexWord64Array" indexIntArray :: ByteArray -> Int -> Int
foreign import prim "primIndexWord64Array" indexWordArray :: ByteArray -> Int -> Word
foreign import prim "primIndexWord64Array" indexAddrArray :: ByteArray -> Int -> Addr
foreign import prim "primIndexWord64Array" indexStablePtrArray :: ByteArray -> Int -> Addr
#else
foreign import prim "primIndexWord32Array" indexIntArray :: ByteArray -> Int -> Int
foreign import prim "primIndexWord32Array" indexWordArray :: ByteArray -> Int -> Word
foreign import prim "primIndexWord32Array" indexAddrArray :: ByteArray -> Int -> Addr
foreign import prim "primIndexWord32Array" indexStablePtrArray :: ByteArray -> Int -> Addr
#endif

foreign import prim "primIndexFloatArray" indexFloatArray :: ByteArray -> Int -> Float
foreign import prim "primIndexDoubleArray" indexDoubleArray :: ByteArray -> Int -> Double

foreign import prim "primIndexWord8Array" indexInt8Array :: ByteArray -> Int -> Int8
foreign import prim "primIndexWord16Array" indexInt16Array :: ByteArray -> Int -> Int16
foreign import prim "primIndexWord32Array" indexInt32Array :: ByteArray -> Int -> Int32
foreign import prim "primIndexWord64Array" indexInt64Array :: ByteArray -> Int -> Int64

foreign import prim "primIndexWord8Array" indexWord8Array :: ByteArray -> Int -> Word8
foreign import prim "primIndexWord16Array" indexWord16Array :: ByteArray -> Int -> Word16
foreign import prim "primIndexWord32Array" indexWord32Array :: ByteArray -> Int -> Word32
foreign import prim "primIndexWord64Array" indexWord64Array :: ByteArray -> Int -> Word64

%%]

%%[99
-- |Read 8-bit character; offset in bytes.

readCharArray :: MutableByteArray s -> Int -> State s -> ( State s,Char )
readCharArray (MutableByteArray a) i s = (s, indexCharArray a i)

-- |Read 31-bit character; offset in 4-byte words.

readWideCharArray :: MutableByteArray s -> Int -> State s -> ( State s,Char )
readWideCharArray (MutableByteArray a) i s = (s, indexWideCharArray a i)

readIntArray :: MutableByteArray s -> Int -> State s -> ( State s,Int )
readIntArray (MutableByteArray a) i s = (s, indexIntArray a i)

readWordArray :: MutableByteArray s -> Int -> State s -> ( State s,Word )
readWordArray (MutableByteArray a) i s = (s, indexWordArray a i)

readAddrArray :: MutableByteArray s -> Int -> State s -> ( State s,Addr )
readAddrArray (MutableByteArray a) i s = (s, indexAddrArray a i)

readFloatArray :: MutableByteArray s -> Int -> State s -> ( State s,Float )
readFloatArray (MutableByteArray a) i s = (s, indexFloatArray a i)

readDoubleArray :: MutableByteArray s -> Int -> State s -> ( State s,Double )
readDoubleArray (MutableByteArray a) i s = (s, indexDoubleArray a i)

readStablePtrArray :: MutableByteArray s -> Int -> State s -> ( State s,StablePtr s )
readStablePtrArray (MutableByteArray a) i s = (s, StablePtr (indexStablePtrArray a i))

readInt8Array :: MutableByteArray s -> Int -> State s -> ( State s,Int8 )
readInt8Array (MutableByteArray a) i s = (s, indexInt8Array a i)

readInt16Array :: MutableByteArray s -> Int -> State s -> ( State s,Int16 )
readInt16Array (MutableByteArray a) i s = (s, indexInt16Array a i)

readInt32Array :: MutableByteArray s -> Int -> State s -> ( State s,Int32 )
readInt32Array (MutableByteArray a) i s = (s, indexInt32Array a i)

readInt64Array :: MutableByteArray s -> Int -> State s -> ( State s,Int64 )
readInt64Array (MutableByteArray a) i s = (s, indexInt64Array a i)

readWord8Array :: MutableByteArray s -> Int -> State s -> ( State s,Word8 )
readWord8Array (MutableByteArray a) i s = (s, indexWord8Array a i)

readWord16Array :: MutableByteArray s -> Int -> State s -> ( State s,Word16 )
readWord16Array (MutableByteArray a) i s = (s, indexWord16Array a i)

readWord32Array :: MutableByteArray s -> Int -> State s -> ( State s,Word32 )
readWord32Array (MutableByteArray a) i s = (s, indexWord32Array a i)

readWord64Array :: MutableByteArray s -> Int -> State s -> ( State s,Word64 )
readWord64Array (MutableByteArray a) i s = (s, indexWord64Array a i)

%%]

%%[99
%%]
-- |Write 8-bit character; offset in bytes.

writeCharArray :: MutableByteArray s -> Int -> Char -> State s -> State s
writeCharArray (MutableByteArray a) i x s = 

-- |Write 31-bit character; offset in 4-byte words.



%%[99
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Ptr
-- Copyright   :  (c) The FFI Task Force, 2000-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The 'Ptr' and 'FunPtr' types and operations.
--
-- Adapted for use in EHC
--
-----------------------------------------------------------------------------

-- #hide
module UHC.Ptr
  ( Addr
  , nullAddr

  , Ptr
  , nullPtr, castPtr, plusPtr, alignPtr, minusPtr
  
  , FunPtr
  , nullFunPtr, castFunPtr, castPtrToFunPtr, castFunPtrToPtr
  
  , freeHaskellFunPtr	-- for now
  )
  where

import UHC.Base
import UHC.Types
import UHC.Prims
import UHC.Show          ( showHex )

#include "MachDeps.h"

------------------------------------------------------------------------
-- Address

#if USE_64_BITS
foreign import prim "primAddWord64" primAddAddr :: Addr -> Int  -> Addr
foreign import prim "primSubWord64" primSubAddr :: Addr -> Addr -> Int
foreign import prim "primRemWord64" primRemAddr :: Addr -> Int  -> Int
foreign import prim "primUnsafeId"  primAddrToWord :: Addr -> Word64

addrToInteger :: Addr -> Integer
addrToInteger a = primWord64ToInteger (primAddrToWord a)
#else
foreign import prim "primAddWord32" primAddAddr :: Addr -> Int  -> Addr
foreign import prim "primSubWord32" primSubAddr :: Addr -> Addr -> Int
foreign import prim "primRemWord32" primRemAddr :: Addr -> Int  -> Int
foreign import prim "primUnsafeId"  primAddrToWord :: Addr -> Word32

addrToInteger :: Addr -> Integer
addrToInteger a = primWord32ToInteger (primAddrToWord a)
#endif

foreign import prim "primNullAddr" nullAddr :: Addr

------------------------------------------------------------------------
-- Data pointers.

newtype Ptr a		= Ptr Addr

-- ^ A value of type @'Ptr' a@ represents a pointer to an object, or an
-- array of objects, which may be marshalled to or from Haskell values
-- of type @a@.
--
-- The type @a@ will often be an instance of class
-- 'Foreign.Storable.Storable' which provides the marshalling operations.
-- However this is not essential, and you can provide your own operations
-- to access the pointer.  For example you might write small foreign
-- functions to get or set the fields of a C @struct@.

-- |The constant 'nullPtr' contains a distinguished value of 'Ptr'
-- that is not associated with a valid memory location.
nullPtr :: Ptr a
nullPtr = Ptr nullAddr

-- |The 'castPtr' function casts a pointer from one type to another.
castPtr :: forall a b . Ptr a -> Ptr b
castPtr (Ptr addr) = Ptr addr

-- |Advances the given address by the given offset in bytes.
plusPtr :: forall a b . Ptr a -> Int -> Ptr b
plusPtr (Ptr addr) d = Ptr (addr `primAddAddr` d)

-- |Given an arbitrary address and an alignment constraint,
-- 'alignPtr' yields the next higher address that fulfills the
-- alignment constraint.  An alignment constraint @x@ is fulfilled by
-- any address divisible by @x@.  This operation is idempotent.
alignPtr :: Ptr a -> Int -> Ptr a
alignPtr addr@(Ptr a) i
  = case a `primRemAddr` i of 
      0 -> addr
      n -> Ptr (a `primAddAddr` (i - n))

-- |Computes the offset required to get from the second to the first
-- argument.  We have 
--
-- > p2 == p1 `plusPtr` (p2 `minusPtr` p1)
minusPtr :: forall a b . Ptr a -> Ptr b -> Int
minusPtr (Ptr a1) (Ptr a2) = a1 `primSubAddr` a2

------------------------------------------------------------------------
-- Function pointers for the default calling convention.

newtype FunPtr a		= FunPtr Addr

-- ^ A value of type @'FunPtr' a@ is a pointer to a function callable
-- from foreign code.  The type @a@ will normally be a /foreign type/,
-- a function type with zero or more arguments where
--
-- * the argument types are /marshallable foreign types/,
--   i.e. 'Char', 'Int', 'Prelude.Double', 'Prelude.Float',
--   'Bool', 'Data.Int.Int8', 'Data.Int.Int16', 'Data.Int.Int32',
--   'Data.Int.Int64', 'Data.Word.Word8', 'Data.Word.Word16',
--   'Data.Word.Word32', 'Data.Word.Word64', @'Ptr' a@, @'FunPtr' a@,
--   @'Foreign.StablePtr.StablePtr' a@ or a renaming of any of these
--   using @newtype@.
-- 
-- * the return type is either a marshallable foreign type or has the form
--   @'Prelude.IO' t@ where @t@ is a marshallable foreign type or @()@.
--
-- A value of type @'FunPtr' a@ may be a pointer to a foreign function,
-- either returned by another foreign function or imported with a
-- a static address import like
--
-- > foreign import ccall "stdlib.h &free"
-- >   p_free :: FunPtr (Ptr a -> IO ())
--
-- or a pointer to a Haskell function created using a /wrapper/ stub
-- declared to produce a 'FunPtr' of the correct type.  For example:
--
-- > type Compare = Int -> Int -> Bool
-- > foreign import ccall "wrapper"
-- >   mkCompare :: Compare -> IO (FunPtr Compare)
--
-- Calls to wrapper stubs like @mkCompare@ allocate storage, which
-- should be released with 'Foreign.Ptr.freeHaskellFunPtr' when no
-- longer required.
--
-- To convert 'FunPtr' values to corresponding Haskell functions, one
-- can define a /dynamic/ stub for the specific foreign type, e.g.
--
-- > type IntFunction = CInt -> IO ()
-- > foreign import ccall "dynamic" 
-- >   mkFun :: FunPtr IntFunction -> IntFunction

-- |The constant 'nullFunPtr' contains a
-- distinguished value of 'FunPtr' that is not
-- associated with a valid memory location.
nullFunPtr :: FunPtr a
nullFunPtr = FunPtr nullAddr

-- |Casts a 'FunPtr' to a 'FunPtr' of a different type.
castFunPtr :: forall a b . FunPtr a -> FunPtr b
castFunPtr (FunPtr addr) = FunPtr addr

-- |Casts a 'FunPtr' to a 'Ptr'.
--
-- /Note:/ this is valid only on architectures where data and function
-- pointers range over the same set of addresses, and should only be used
-- for bindings to external libraries whose interface already relies on
-- this assumption.
castFunPtrToPtr :: forall a b . FunPtr a -> Ptr b
castFunPtrToPtr (FunPtr addr) = Ptr addr

-- |Casts a 'Ptr' to a 'FunPtr'.
--
-- /Note:/ this is valid only on architectures where data and function
-- pointers range over the same set of addresses, and should only be used
-- for bindings to external libraries whose interface already relies on
-- this assumption.
castPtrToFunPtr :: forall a b . Ptr a -> FunPtr b
castPtrToFunPtr (Ptr addr) = FunPtr addr


------------------------------------------------------------------------
-- Eq, Ord instances for Ptr.

instance Eq (Ptr a) where
  (Ptr a1) == (Ptr a2) = a1 `primEqAddr` a2

instance Ord (Ptr a) where
  (Ptr a1) `compare` (Ptr a2) = a1 `primCmpAddr` a2

------------------------------------------------------------------------
-- Show instances for Ptr and FunPtr

instance Show (Ptr a) where
   showsPrec _ (Ptr a) rs = pad_out (showHex (addrToInteger a) "")
     where
        -- want 0s prefixed to pad it out to a fixed length.
       pad_out ls = 
          '0':'x':(replicate (2*SIZEOF_HSPTR - length ls) '0') ++ ls ++ rs

instance Show (FunPtr a) where
   showsPrec p = showsPrec p . castFunPtrToPtr
%%]

%%[99
-- For now, we put this here, the function doesn't do anything useful because no export is yet allowed
freeHaskellFunPtr :: FunPtr a -> IO ()
freeHaskellFunPtr (FunPtr a) = ioFromPrim $ \_ -> ()
%%]

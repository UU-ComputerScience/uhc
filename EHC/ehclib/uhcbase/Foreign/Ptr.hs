{-# LANGUAGE NoImplicitPrelude, CPP #-}
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-# OPTIONS_UHC "--optP=-traditional-cpp" #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Ptr
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides typed pointers to foreign data.  It is part
-- of the Foreign Function Interface (FFI) and will normally be
-- imported via the "Foreign" module.
--
-----------------------------------------------------------------------------

module Foreign.Ptr (

    -- * Data pointers

#if ! defined(__UHC_TARGET_JS__)
    Ptr,      -- data Ptr a
    nullPtr,      -- :: Ptr a
    castPtr,      -- :: Ptr a -> Ptr b
    plusPtr,      -- :: Ptr a -> Int -> Ptr b
    alignPtr,     -- :: Ptr a -> Int -> Ptr a
    minusPtr,     -- :: Ptr a -> Ptr b -> Int
#endif
    -- * Function pointers

    FunPtr,      -- data FunPtr a
#if ! defined(__UHC_TARGET_JS__)
    nullFunPtr,      -- :: FunPtr a
    castFunPtr,      -- :: FunPtr a -> FunPtr b
    castFunPtrToPtr, -- :: FunPtr a -> Ptr b
    castPtrToFunPtr, -- :: Ptr a -> FunPtr b
#endif

    freeHaskellFunPtr, -- :: FunPtr a -> IO ()
    -- Free the function pointer created by foreign export dynamic.

#if ! ( defined(__UHC_TARGET_JS__) || defined(__NHC__) )
    -- * Integral types with lossless conversion to and from pointers
    IntPtr,
    ptrToIntPtr,
    intPtrToPtr,
    WordPtr,
    ptrToWordPtr,
    wordPtrToPtr
#endif
 ) where

#ifdef __GLASGOW_HASKELL__

import GHC.Ptr
import GHC.IOBase
import GHC.Base
import GHC.Num
import GHC.Read
import GHC.Real
import GHC.Show
import GHC.Enum
import GHC.Word         ( Word(..) )

import Data.Int
import Data.Word

#elif __UHC__

import Control.Monad    ( liftM )
#if ! defined(__UHC_TARGET_JS__)
import Foreign.C.Types
#endif

#else
import Control.Monad    ( liftM )
import Foreign.C.Types
#endif

import Data.Bits
import Data.Typeable

#if ! defined(__UHC_TARGET_JS__)
import Foreign.Storable ( Storable(..) )
#endif

#ifdef __NHC__
import NHC.FFI
  ( Ptr
  , nullPtr
  , castPtr
  , plusPtr
  , alignPtr
  , minusPtr
  , FunPtr
  , nullFunPtr
  , castFunPtr
  , castFunPtrToPtr
  , castPtrToFunPtr
  , freeHaskellFunPtr
  )
#endif

#ifdef __HUGS__
import Hugs.Ptr
#endif

#ifdef __UHC__
import UHC.Ptr
import UHC.IOBase
import UHC.Base
#if ! defined(__UHC_TARGET_JS__)
import UHC.Read
import UHC.Real
import UHC.Show
import UHC.Enum
#endif
#endif

#ifdef __GLASGOW_HASKELL__
-- | Release the storage associated with the given 'FunPtr', which
-- must have been obtained from a wrapper stub.  This should be called
-- whenever the return value from a foreign import wrapper function is
-- no longer required; otherwise, the storage it uses will leak.
foreign import ccall unsafe "freeHaskellFunctionPtr"
    freeHaskellFunPtr :: FunPtr a -> IO ()
#endif

#ifndef __NHC__
# include "HsBaseConfig.h"
# include "CTypes.h"

# ifdef __GLASGOW_HASKELL__
-- | An unsigned integral type that can be losslessly converted to and from
-- @Ptr@.
INTEGRAL_TYPE(WordPtr,tyConWordPtr,"WordPtr",Word)
        -- Word and Int are guaranteed pointer-sized in GHC

-- | A signed integral type that can be losslessly converted to and from
-- @Ptr@.
INTEGRAL_TYPE(IntPtr,tyConIntPtr,"IntPtr",Int)
        -- Word and Int are guaranteed pointer-sized in GHC

-- | casts a @Ptr@ to a @WordPtr@
ptrToWordPtr :: Ptr a -> WordPtr
ptrToWordPtr (Ptr a#) = WordPtr (W# (int2Word# (addr2Int# a#)))

-- | casts a @WordPtr@ to a @Ptr@
wordPtrToPtr :: WordPtr -> Ptr a
wordPtrToPtr (WordPtr (W# w#)) = Ptr (int2Addr# (word2Int# w#))

-- | casts a @Ptr@ to an @IntPtr@
ptrToIntPtr :: Ptr a -> IntPtr
ptrToIntPtr (Ptr a#) = IntPtr (I# (addr2Int# a#))

-- | casts an @IntPtr@ to a @Ptr@
intPtrToPtr :: IntPtr -> Ptr a
intPtrToPtr (IntPtr (I# i#)) = Ptr (int2Addr# i#)

# else /* !__GLASGOW_HASKELL__ */

#if ! defined(__UHC_TARGET_JS__)
INTEGRAL_TYPE(WordPtr,tyConWordPtr,"WordPtr",CUIntPtr)
INTEGRAL_TYPE(IntPtr,tyConIntPtr,"IntPtr",CIntPtr)
#endif

{-# CFILES cbits/PrelIOUtils.c #-}

#  ifdef __UHC__

#if ! defined(__UHC_TARGET_JS__)
foreign import prim "primUnsafeId"
    ptrToWordPtr :: Ptr a -> WordPtr

foreign import prim "primUnsafeId"
    wordPtrToPtr :: WordPtr -> Ptr a

foreign import prim "primUnsafeId"
    ptrToIntPtr :: Ptr a -> IntPtr

foreign import prim "primUnsafeId"
    intPtrToPtr :: IntPtr -> Ptr a
#endif

#  else /* !__UHC__ */

foreign import ccall unsafe "__hscore_to_uintptr"
    ptrToWordPtr :: Ptr a -> WordPtr

foreign import ccall unsafe "__hscore_from_uintptr"
    wordPtrToPtr :: WordPtr -> Ptr a

foreign import ccall unsafe "__hscore_to_intptr"
    ptrToIntPtr :: Ptr a -> IntPtr

foreign import ccall unsafe "__hscore_from_intptr"
    intPtrToPtr :: IntPtr -> Ptr a

#  endif /* !__UHC__ */

# endif /* !__GLASGOW_HASKELL__ */
#endif /* !__NHC_ */

{-# LANGUAGE NoImplicitPrelude, CPP #-}
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# EXCLUDE_IF_TARGET js #-}
{-# EXCLUDE_IF_TARGET cr #-}
{-# LANGUAGE GenericDeriving #-}

-- XXX -fno-warn-unused-binds stops us warning about unused constructors,
-- but really we should just remove them if we don't want them
-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.C.Types
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Mapping of C types to corresponding Haskell types.
--
-----------------------------------------------------------------------------

module Foreign.C.TypesZero
        ( -- * Representations of C types
#ifndef __NHC__
          -- ** Floating types
          -- | These types are are represented as @newtype@s of
          -- 'Prelude.Float' and 'Prelude.Double', and are instances of
          -- 'Prelude.Eq', 'Prelude.Ord', 'Prelude.Num', 'Prelude.Read',
          -- 'Prelude.Show', 'Prelude.Enum', 'Typeable', 'Storable',
          -- 'Prelude.Real', 'Prelude.Fractional', 'Prelude.Floating',
          -- 'Prelude.RealFrac' and 'Prelude.RealFloat'.
         CFloat,  CDouble, CLDouble
#else
         CFloat(..),   CDouble(..), CLDouble(..)
#endif

        ) where

#ifndef __NHC__

import {-# SOURCE #-} Foreign.Storable
import Data.Bits        ( Bits(..) )
import Data.Int         ( Int8,  Int16,  Int32,  Int64  )
import Data.Word        ( Word8, Word16, Word32, Word64 )
import {-# SOURCE #-} Data.Typeable

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.Float
import GHC.Enum
import GHC.Real
import GHC.Show
import GHC.Read
import GHC.Num
#else
import Control.Monad    ( liftM )
#endif

#ifdef __HUGS__
import Hugs.Ptr         ( castPtr )
#endif

#ifdef __UHC__
import UHC.Ptr
import UHC.Types
import UHC.Base
import UHC.Float
import UHC.Enum
import UHC.Real
import UHC.Show
import UHC.Read
#endif
#include "HsBaseConfig.h"
#include "CTypes.h"

-- A slightly different def of properFraction is required to avoid problems with absence of monomorphism restriction.
-- It should be a temporary solution.
#ifdef __UHC__
#define INSTANCE_EHC_REALFRAC(T) \
instance RealFrac T where { \
   properFraction (T x) :: (mt,...) = let (m::mt,y) = properFraction x in (m::mt, T y) ; \
   truncate (T x) = truncate x ; \
   round    (T x) = round x ; \
   ceiling  (T x) = ceiling x ; \
   floor    (T x) = floor x }

#define FLOATING_EHC_TYPE(T,C,S,B) \
ARITHMETIC_TYPE(T,C,S,B) ; \
INSTANCE_FRACTIONAL(T) ; \
INSTANCE_FLOATING(T) ; \
INSTANCE_EHC_REALFRAC(T) ; \
INSTANCE_REALFLOAT(T)

#endif

-- | Haskell type representing the C @float@ type.
#ifdef __UHC__
FLOATING_EHC_TYPE(CFloat,tyConCFloat,"CFloat",HTYPE_FLOAT)
#else
FLOATING_TYPE(CFloat,tyConCFloat,"CFloat",HTYPE_FLOAT)
#endif
-- | Haskell type representing the C @double@ type.
#ifdef __UHC__
FLOATING_EHC_TYPE(CDouble,tyConCDouble,"CDouble",HTYPE_DOUBLE)
#else
FLOATING_TYPE(CDouble,tyConCDouble,"CDouble",HTYPE_DOUBLE)
#endif
-- HACK: Currently no long double in the FFI, so we simply re-use double
-- | Haskell type representing the C @long double@ type.
#ifdef __UHC__
FLOATING_EHC_TYPE(CLDouble,tyConCLDouble,"CLDouble",HTYPE_DOUBLE)
#else
FLOATING_TYPE(CLDouble,tyConCLDouble,"CLDouble",HTYPE_DOUBLE)
#endif

#endif

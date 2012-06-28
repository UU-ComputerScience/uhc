{-# LANGUAGE NoImplicitPrelude #-}

module UHC.WeakPtr
  ( WeakPtr

  , mkWeakPtr, mkWeakPtrWOFinalizer
  , deRefWeakPtr
  , finalizeWeakPtr

  , mkWeakPtrForeignEnv
  )
  where

import UHC.Base
import UHC.Ptr


data WeakPtr val		-- opaque, internals known by RTS

foreign import prim "primMakeWeakPtr" mkWeakPtr :: k -> v -> IO () -> WeakPtr v
foreign import prim "primMakeWeakPtrWOFinalizer" mkWeakPtrWOFinalizer :: k -> v -> WeakPtr v
foreign import prim "primDeRefWeakPtr" deRefWeakPtr :: WeakPtr v -> Maybe v
foreign import prim "primFinalizeWeakPtr" finalizeWeakPtr :: WeakPtr v -> Maybe (IO ())

mkWeakPtrForeignEnv :: k -> v -> Addr -> Addr -> Int -> Addr -> WeakPtr v
mkWeakPtrForeignEnv k v finalizer pointer hasEnv env = mkWeakPtr k v (return ())



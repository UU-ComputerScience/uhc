%%[doesWhat doclatex
Interface to WeakPtr.
No useful functionality is provided however, just the interface.
%%]

%%[99
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

%%]

%%[99
data WeakPtr val		-- opaque, internals known by RTS
%%]

%%[99
foreign import prim "primMakeWeakPtr" mkWeakPtr :: k -> v -> IO () -> WeakPtr v
foreign import prim "primMakeWeakPtrWOFinalizer" mkWeakPtrWOFinalizer :: k -> v -> WeakPtr v
foreign import prim "primDeRefWeakPtr" deRefWeakPtr :: WeakPtr v -> Maybe v
foreign import prim "primFinalizeWeakPtr" finalizeWeakPtr :: WeakPtr v -> Maybe (IO ())
%%]

Not yet clear what this is supposed to do, but provide the interface, forgetting the finalizer stuff.

%%[99
mkWeakPtrForeignEnv :: k -> v -> Addr -> Addr -> Int -> Addr -> WeakPtr v
mkWeakPtrForeignEnv k v finalizer pointer hasEnv env = mkWeakPtr k v (return ())

%%]



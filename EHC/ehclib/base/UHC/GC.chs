%%[99
-- Interface to garbage collector
module UHC.GC
  ( gc
  )
  where

import UHC.Base
%%]

%%[99
-- trigger GC
foreign import prim safe "primGC" gc :: IO Bool
%%]

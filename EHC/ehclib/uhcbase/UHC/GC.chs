%%[99
{-# LANGUAGE NoImplicitPrelude #-}

-- Interface to garbage collector
module UHC.GC
  ( gc
  )
  where

import UHC.Base0
%%]

%%[99
-- trigger GC
foreign import prim safe "primGC" gc :: IO Bool
%%]

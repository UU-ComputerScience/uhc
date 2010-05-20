%%[99
{-# LANGUAGE NoImplicitPrelude #-}


module UHC.Bounded
where

import UHC.Base

instance Bounded () where
    minBound = ()
    maxBound = ()

%%]

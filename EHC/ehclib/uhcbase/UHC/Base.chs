%%[99
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

module UHC.Base   -- adapted from the Hugs prelude
(
 ) where


#include "IntLikeInstance.h"

data Bool    = False | True

not         :: Bool -> Bool
not True     = False
not False    = True

%%]
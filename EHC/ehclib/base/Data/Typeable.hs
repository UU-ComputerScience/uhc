-- 20090226, AD: the same as GHS's Typeable.hs-boot, ifndeffing out some imports
{-# OPTIONS_GHC -XNoImplicitPrelude #-}

module Data.Typeable where

import Data.Maybe
#ifndef __EHC__
import GHC.Base
import GHC.Show
#endif

data TypeRep
data TyCon

#ifndef __EHC__
mkTyCon      :: String -> TyCon
mkTyConApp   :: TyCon -> [TypeRep] -> TypeRep
showsTypeRep :: TypeRep -> ShowS

cast :: (Typeable a, Typeable b) => a -> Maybe b
#endif

class Typeable a where
  typeOf :: a -> TypeRep


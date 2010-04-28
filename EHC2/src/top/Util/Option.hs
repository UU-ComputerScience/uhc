{-# OPTIONS -fglasgow-exts  #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
-----------------------------------------------------------------------------

module Top.Util.Option where

import Control.Monad.State

option :: a -> String -> Option a
option a s = Option { defaultValue = a, currentValue = a, optionDescription = s }

data Option         a = Option { defaultValue :: a, currentValue :: a, optionDescription :: String }
data OptionAccess m a = Access { getOption :: m a, setOption :: a -> m () }

ignoreOption :: Monad m => Option a -> OptionAccess m a
ignoreOption value = 
   Access { getOption = return (currentValue value), setOption = const $ return () }

optionAccessTrans :: (forall a . m1 a -> m2 a) -> OptionAccess m1 b -> OptionAccess m2 b
optionAccessTrans f oa = 
   Access { getOption = f (getOption oa), setOption = f . setOption oa }

useOption :: MonadState s m => (s -> Option a) -> (Option a -> s -> s) -> OptionAccess m a
useOption getter setter = 
   let f b x = setter ((getter x) { currentValue = b }) x
   in Access { getOption = gets (currentValue . getter), setOption = modify . f }

instance (Show a, Eq a) => Show (Option a) where
   show a = 
      let extra | currentValue a == defaultValue a = " (default)"
                | otherwise                        = ""
      in optionDescription a ++ ": " ++ show (currentValue a) ++ extra

instance Functor Option where
   fmap f a = a { defaultValue = f (defaultValue a), currentValue = f (currentValue a) }
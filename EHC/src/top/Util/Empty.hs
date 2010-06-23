-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  portable
-----------------------------------------------------------------------------

module Top.Util.Empty where

------------------------------------------------------------------------
-- * Empty type class

class Empty a where
   empty :: a

instance Empty () where
   empty = ()

instance (Empty a, Empty b) => Empty (a, b) where
   empty = (empty, empty)

instance Empty [a] where
   empty = []
   
instance Empty (Maybe a) where
   empty = Nothing
   
instance Empty a => Empty (Either a b) where
   empty = Left empty
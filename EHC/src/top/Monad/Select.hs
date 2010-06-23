{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
-----------------------------------------------------------------------------

module Top.Monad.Select 
   ( module Top.Monad.Select
   , module Control.Monad.State
   ) where

import Top.Util.Embedding
import Control.Monad.State

--------------------------------------------------------
-- Select Monad

newtype Select t m a = Select (m a)

instance Monad m => Monad (Select t m) where
   return a       = Select (return a) 
   Select f >>= g = Select (do x <- f
                               let Select h = g x
                               h)

instance (MonadState s m, Embedded label s t) => MonadState t (Select t m) where
   get   = Select (gets   (getE embedding  ))
   put i = Select (modify (setE embedding i))

instance MonadTrans (Select t) where
   lift = select
   
select :: m a -> Select t m a
select = Select

--------------------------------------------------------
-- SelectFix Monad

-- ToDo: fix with kind annotation
data SelectFix t m a = SelectFix (m a) | SelectFix_Dummy_ (t m)

instance Monad m => Monad (SelectFix t m) where
   return a          = SelectFix (return a)
   SelectFix f >>= g = SelectFix (do x <- f
                                     let SelectFix h = g x
                                     h)
                            
instance (MonadState s m, Embedded label s (t m)) => MonadState (t m) (SelectFix t m) where
   get   = SelectFix (gets   (getE embedding  ))
   put i = SelectFix (modify (setE embedding i))

instance MonadTrans (SelectFix t) where
   lift = selectFix

selectFix :: m a -> SelectFix t m a
selectFix = SelectFix

--------------------------------------------------------
-- Class Embedded

class Embedded label s t | label s -> t, t -> label where
   embedding :: Embedding s t

instance Embedded c s2 t => Embedded c (s1, s2) t where
   embedding = composeE sndE embedding
   
--------------------------------------------------------
-- deselect functions for Select Monad

deselect :: Select t m a -> m a  
deselect (Select m) = m

deselectFor :: (Embedded label s t, MonadState s m) => label -> Select t m a -> m a
deselectFor  _ = deselect

--------------------------------------------------------
-- deselect functions for SelectFix Monad

deselectFix :: SelectFix t m a -> m a  
deselectFix (SelectFix m) = m

deselectFixFor :: (Embedded label s (t m), MonadState s m) => label -> SelectFix t m a -> m a
deselectFixFor _ = deselectFix 
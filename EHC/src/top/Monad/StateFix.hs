{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
-----------------------------------------------------------------------------

module Top.Monad.StateFix 
   ( module Top.Monad.StateFix
   , module Control.Monad.State
   ) where

import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Writer

type StateFix s = StateFixT s Identity

data StateFixT s m a = Fix { unFix :: StateT (s (StateFixT s m)) m a }

instance Monad m => Monad (StateFixT s m) where 
   return  = Fix . return
   m >>= f = Fix (unFix m >>= unFix . f)

instance Monad m => MonadState (s (StateFixT s m)) (StateFixT s m) where
   get = Fix $ get
   put = Fix . put

instance MonadTrans (StateFixT s) where
   lift = Fix . lift
   
instance MonadWriter w m => MonadWriter w (StateFixT s m) where
   tell   = lift . tell
   listen = Fix . listen . unFix
   pass   = Fix . pass   . unFix
   
--

runStateFixT :: StateFixT s m a -> s (StateFixT s m) -> m (a, s (StateFixT s m))
runStateFixT = runStateT . unFix

evalStateFixT :: Monad m => StateFixT s m a -> s (StateFixT s m) -> m a
evalStateFixT = evalStateT . unFix

execStateFixT :: Monad m => StateFixT s m a -> s (StateFixT s m) -> m (s (StateFixT s m))
execStateFixT = execStateT . unFix

--

runStateFix :: StateFix s a -> s (StateFix s) -> (a, s (StateFix s))
runStateFix m = runIdentity . runStateFixT m

evalStateFix :: StateFix s a -> s (StateFix s) -> a
evalStateFix m = runIdentity . evalStateFixT m

execStateFix :: StateFix s a -> s (StateFix s) -> (s (StateFix s))
execStateFix m = runIdentity . execStateFixT m
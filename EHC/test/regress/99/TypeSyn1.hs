{- ----------------------------------------------------------------------------------------
   what    : the tysyn Forest caused type inferencing problems because the syn was prematurely/impredicatively
             bound to a tyvar, thereby inhibiting proper expansion and correct tyvar binding.
   expected: no compiletime errors
---------------------------------------------------------------------------------------- -}

module TypeSyn1 where

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Monoid

data Tree a   = Node {
		rootLabel :: a,		-- ^ label value
		subForest :: Forest a	-- ^ zero or more child trees
	}

type Forest a = [Tree a]

instance Functor Tree where
  fmap f (Node x ts) = Node (f x) (map (fmap f) ts)

instance Applicative Tree where
  pure x = Node x []
  Node f tfs <*> tx@(Node x txs) =
    Node (f x) (map (f <$>) txs ++ map (<*> tx) tfs)

instance Monad Tree where
  return x = Node x []
  Node x ts >>= f = Node x' (ts' ++ map (>>= f) ts)
    where Node x' ts' = f x

instance Traversable Tree where
  traverse f (Node x ts) = Node <$> f x <*> traverse (traverse f) ts

instance Foldable Tree where
  foldMap f (Node x ts) = f x `mappend` foldMap (foldMap f) ts

main = return ()

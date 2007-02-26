-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  portable
-----------------------------------------------------------------------------

module Top.Util.Embedding where

data Embedding a b = Embedding { getE :: a -> b, changeE :: (b -> b) -> a -> a }

setE :: Embedding a b -> b -> a -> a
setE e = changeE e . const

withE :: Embedding a b -> (b -> c) -> a -> c
withE e f = f . (getE e)

------------------------------
-- useful embeddings

idE :: Embedding a a
idE = Embedding { getE = id, changeE = id }

fstE :: Embedding (a, b) a
fstE = Embedding { getE = fst, changeE = \f (a, b) -> (f a, b) }

sndE :: Embedding (a, b) b
sndE = Embedding { getE = snd, changeE = \f (a, b) -> (a, f b) }

------------------------------
-- compositions of embeddings

composeE :: Embedding a b -> Embedding b c -> Embedding a c
composeE e1 e2 = Embedding { getE = getE e2 . getE e1, changeE = changeE e1 . changeE e2 }

fromFstE :: Embedding a c -> Embedding (a, b) c
fromFstE = composeE fstE

fromSndE :: Embedding b c -> Embedding (a, b) c
fromSndE = composeE sndE
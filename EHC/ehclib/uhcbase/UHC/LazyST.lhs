\begin{code}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ST
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The 'ST' Monad.
--
-- Adapted for use in UHC, for Lazy ST
--
-----------------------------------------------------------------------------

-- #hide
module UHC.LazyST where

import UHC.Base
import UHC.ST(STRep(..), STret(..))
import qualified UHC.ST as ST

\end{code}

\begin{code}
newtype ST s a = ST (STRep s a)

instance Functor (ST s) where
    fmap f (ST m) = ST $ \ s ->
      case (m s) of { ( new_s, r ) ->
      ( new_s, f r ) }

instance Monad (ST s) where
    {-# INLINE return #-}
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
    return x = ST (\ s -> ( s, x ))
    m >> k   = m >>= \ _ -> k

    (ST m) >>= k
      = ST (\ s ->
        case (m s) of { ( new_s, r ) ->
        case (k r) of { ST k2 ->
        (k2 new_s) }})

-- liftST is useful when we want a lifted result from an ST computation.  See
-- fixST below.
liftST :: ST s a -> State s -> STret s a
liftST (ST m) = \s -> case m s of ( s', r ) -> STret s' r

-- | Allow the result of a state transformer computation to be used (lazily)
-- inside the computation.
-- Note that if @f@ is strict, @'fixST' f = _|_@.
fixST :: (a -> ST s a) -> ST s a
fixST k = ST $ \ s ->
    let ans       = liftST (k r) s
        STret _ r = ans
    in
    case ans of STret s' x -> ( s', x )

instance  Show (ST s a)  where
    showsPrec _ _  = showString "<<ST action>>"
    -- showList       = showList__ (showsPrec 0)
\end{code}

\begin{code}
runST :: (forall s. ST s a) -> a
runST st = runSTRep (case st of { ST st_rep -> st_rep })

runSTRep :: (forall s. STRep s a) -> a
runSTRep st_rep = case st_rep {- realWorld -} (State realWorld) of
                        ( _, r ) -> r
\end{code}

\begin{code}
{-|
Convert a strict 'ST' computation into a lazy one.  The strict state
thread passed to 'strictToLazyST' is not performed until the result of
the lazy state thread it returns is demanded.
-}
strictToLazyST :: ST.ST s a -> ST s a
strictToLazyST m = ST $ \s ->
  let pr = ST.liftST m s
      ~(ST.STret _  r) = pr
      ~(ST.STret s' _) = pr
  in (s', r)
{-
        let 
           pr = case s of { S# s# -> ST.liftST m s# }
           r  = case pr of { ST.STret _ v -> v }
           s' = case pr of { ST.STret s2# _ -> S# s2# }
        in
        (r, s')
-}

{-| 
Convert a lazy 'ST' computation into a strict one.
-}
lazyToStrictST :: ST s a -> ST.ST s a
lazyToStrictST (ST m) = ST.ST $ \s ->
        case (m s) of
        (s', a) -> letstrict s'' = s'
                   in ( s'', a )

unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST = strictToLazyST . ST.unsafeInterleaveST . lazyToStrictST
{-
-}
\end{code}

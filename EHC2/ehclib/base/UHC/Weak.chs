%%[99
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Weak
-- Copyright   :  (c) The University of Glasgow, 1998-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Weak pointers.
--
-- Adapted for use in EHC
--
-----------------------------------------------------------------------------

-- #hide
module UHC.Weak
  ( Weak
  , mkWeak, deRefWeak
  , finalize

  , mkWeakForeignEnv
  )
  where

import UHC.Base
import UHC.IOBase
import UHC.Ptr
import UHC.WeakPtr
import Data.Typeable

{-|
A weak pointer object with a key and a value.  The value has type @v@.

A weak pointer expresses a relationship between two objects, the
/key/ and the /value/:  if the key is considered to be alive by the
garbage collector, then the value is also alive.  A reference from
the value to the key does /not/ keep the key alive.

A weak pointer may also have a finalizer of type @IO ()@; if it does,
then the finalizer will be run at most once, at a time after the key
has become unreachable by the program (\"dead\").  The storage manager
attempts to run the finalizer(s) for an object soon after the object
dies, but promptness is not guaranteed.  

It is not guaranteed that a finalizer will eventually run, and no
attempt is made to run outstanding finalizers when the program exits.
Therefore finalizers should not be relied on to clean up resources -
other methods (eg. exception handlers) should be employed, possibly in
addition to finalisers.

References from the finalizer to the key are treated in the same way
as references from the value to the key: they do not keep the key
alive.  A finalizer may therefore ressurrect the key, perhaps by
storing it in the same data structure.

The finalizer, and the relationship between the key and the value,
exist regardless of whether the program keeps a reference to the
'Weak' object or not.

There may be multiple weak pointers with the same key.  In this
case, the finalizers for each of these weak pointers will all be
run in some arbitrary order, or perhaps concurrently, when the key
dies.  If the programmer specifies a finalizer that assumes it has
the only reference to an object (for example, a file that it wishes
to close), then the programmer must ensure that there is only one
such finalizer.

If there are no other threads to run, the runtime system will check
for runnable finalizers before declaring the system to be deadlocked.
-}
newtype Weak v = Weak (WeakPtr v)

#include "Typeable.h"
INSTANCE_TYPEABLE1(Weak,weakTc,"Weak")

-- | Establishes a weak pointer to @k@, with value @v@ and a finalizer.
--
-- This is the most general interface for building a weak pointer.
--
mkWeak  :: k                            -- ^ key
        -> v                            -- ^ value
        -> Maybe (IO ())                -- ^ finalizer
        -> IO (Weak v)                  -- ^ returns: a weak pointer object

mkWeak key val (Just finalizer) = ioFromPrim $ \s ->
   Weak (mkWeakPtr key val finalizer)
mkWeak key val Nothing = ioFromPrim $ \s ->
   Weak (mkWeakPtrWOFinalizer key val)

{-|
Dereferences a weak pointer.  If the key is still alive, then
@'Just' v@ is returned (where @v@ is the /value/ in the weak pointer), otherwise
'Nothing' is returned.

The return value of 'deRefWeak' depends on when the garbage collector
runs, hence it is in the 'IO' monad.
-}
deRefWeak :: Weak v -> IO (Maybe v)
deRefWeak (Weak w) = ioFromPrim $ \s -> deRefWeakPtr w

-- | Causes a the finalizer associated with a weak pointer to be run
-- immediately.
finalize :: Weak v -> IO ()
finalize (Weak w) =
   case finalizeWeakPtr w of
     Just f -> f
     _      -> return ()
{-
   case finalizeWeak# w s of
        (# s1, 0#, _ #) -> (# s1, () #) -- already dead, or no finaliser
        (# s1, _,  f #) -> f s1
-}

#ifndef __UHC__
{-
Instance Eq (Weak v) where
  (Weak w1) == (Weak w2) = w1 `sameWeak#` w2
-}


-- run a batch of finalizers from the garbage collector.  We're given 
-- an array of finalizers and the length of the array, and we just
-- call each one in turn.
--
-- the IO primitives are inlined by hand here to get the optimal
-- code (sigh) --SDM.

runFinalizerBatch :: Int -> Array# (IO ()) -> IO ()
runFinalizerBatch (I# n) arr = 
   let  go m  = IO $ \s ->
                  case m of 
                  0# -> (# s, () #)
                  _  -> let m' = m -# 1# in
                        case indexArray# arr m' of { (# io #) -> 
                        case unIO io s of          { (# s', _ #) -> 
                        unIO (go m') s'
                        }}
   in
        go n

#endif
%%]

Not yet clear what this is supposed to do, but provide the interface.

%%[99
mkWeakForeignEnv :: k -> v -> Addr -> Addr -> Int -> Addr -> IO (Weak v)
mkWeakForeignEnv k v finalizer pointer hasEnv env = ioFromPrim $ \_ ->
  Weak (mkWeakPtrForeignEnv k v finalizer pointer hasEnv env)
%%]



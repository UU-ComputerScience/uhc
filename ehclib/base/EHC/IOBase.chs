%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% IO basic stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
module EHC.IOBase
  ( unsafePerformIO,
  
        -- To and from from ST
    stToIO, ioToST, unsafeIOToST, unsafeSTToIO,

        -- References
    IORef(..), newIORef, readIORef, writeIORef, 
    -- IOArray(..), newIOArray, readIOArray, writeIOArray, unsafeReadIOArray, unsafeWriteIOArray,
    -- MVar(..),

  )
  where

import EHC.Prelude
import EHC.ST
import EHC.STRef

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unsafe
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
unsafePerformIO :: IO a -> a
unsafePerformIO (IO m)
  = case m ioWorld of
      (_, x) -> x

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% IO <-> ST
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
-- ---------------------------------------------------------------------------
-- Coercions between IO and ST

-- | A monad transformer embedding strict state transformers in the 'IO'
-- monad.  The 'RealWorld' parameter indicates that the internal state
-- used by the 'ST' computation is a special one supplied by the 'IO'
-- monad, and thus distinct from those used by invocations of 'runST'.
stToIO        :: ST RealWorld a -> IO a
stToIO (ST m) = IO m

ioToST        :: IO a -> ST RealWorld a
ioToST (IO m) = (ST m)

-- This relies on IO and ST having the same representation modulo the
-- constraint on the type of the state
--
unsafeIOToST        :: IO a -> ST s a
unsafeIOToST (IO io) = ST $ \ s -> (unsafeCoerce io) s

unsafeSTToIO :: ST s a -> IO a
unsafeSTToIO (ST m) = IO (unsafeCoerce m)

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% IORef
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
-- ---------------------------------------------------------------------------
-- IORefs

-- |A mutable variable in the 'IO' monad
newtype IORef a = IORef (STRef RealWorld a)

-- explicit instance because Haddock can't figure out a derived one
instance Eq (IORef a) where
  IORef x == IORef y = x == y

-- |Build a new 'IORef'
newIORef    :: a -> IO (IORef a)
newIORef v = stToIO (newSTRef v) >>= \ var -> return (IORef var)

-- |Read the value of an 'IORef'
readIORef   :: IORef a -> IO a
readIORef  (IORef var) = stToIO (readSTRef var)

-- |Write a new value into an 'IORef'
writeIORef  :: IORef a -> a -> IO ()
writeIORef (IORef var) v = stToIO (writeSTRef var v)

%%]

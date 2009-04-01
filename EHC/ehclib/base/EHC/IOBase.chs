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

        -- IO Exception
    try,

  )
  where

import EHC.Prelude
import EHC.ST
import EHC.STRef

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unsafe
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9999
unsafePerformIO :: IO a -> a
unsafePerformIO (IO m)
  = case m ioWorld of
      x -> x

%%]

%%[99
unsafePerformIO :: IO a -> a
unsafePerformIO (IO m)
  = case m ioWorld of
      (_, x) -> x

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% IO <-> ST
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This version assumes that ST has structure: state -> (state,result), and IO: state -> result
%%[9999
-- ---------------------------------------------------------------------------
-- Coercions between IO and ST

-- | A monad transformer embedding strict state transformers in the 'IO'
-- monad.  The 'RealWorld' parameter indicates that the internal state
-- used by the 'ST' computation is a special one supplied by the 'IO'
-- monad, and thus distinct from those used by invocations of 'runST'.
stToIO        :: ST RealWorld a -> IO a
stToIO (ST m) = IO (\s -> case m s of {(_,r) -> r})

ioToST        :: IO a -> ST RealWorld a
ioToST (IO m) = ST (\s -> case m s of {r -> (s,r)})

-- This relies on IO and ST having the same representation modulo the
-- constraint on the type of the state
--
unsafeIOToST        :: IO a -> ST s a
unsafeIOToST io = unsafeCoerce (ioToST io)

unsafeSTToIO :: ST s a -> IO a
unsafeSTToIO m = stToIO (unsafeCoerce m)

%%]

This version assumes that ST and IO have the same internal representation: state -> (state,result)
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
%%% try
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
-- | The construct 'try' @comp@ exposes IO errors which occur within a
-- computation, and which are not fully handled.
--
-- Non-I\/O exceptions are not caught by this variant; to catch all
-- exceptions, use 'Control.Exception.try' from "Control.Exception".

try            :: IO a -> IO (Either IOError a)
try f          =  catch (do r <- f
                            return (Right r))
                        (return . Left)

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

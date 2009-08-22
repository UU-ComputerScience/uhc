%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MVar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
A MVar is used for synchronization.
Concurrency is not (yet) supported by EHC,
so MVar is straightforwardly built on top of MutVar.
In GHC MVar is offered by module GHC.Conc
%%]

%%[99
module UHC.MVar
  ( MVar
  , newEmptyMVar, newMVar
  
  , tryTakeMVar, takeMVar
  , tryPutMVar, putMVar
  
  , isEmptyMVar
  
  , withMVar
  
  , addMVarFinalizer
  )
  where

import UHC.Base
import UHC.OldException
import UHC.MutVar
import UHC.IOBase
import UHC.Weak

import Debug.Trace

%%]

%%[99
-- type    MVar' a = MutVar RealWorld (Maybe a)
type    MState = State RealWorld
-- newtype MVar  a = MVar (MVar' a)
%%]

Underlying routines using in MutVar

%%[99
-- Possibly return value in var
tryTakeMutVar :: MVar' a -> MState -> ( MState, Maybe a )
tryTakeMutVar v s
  = case readMutVar v s of
      (s2,x@(Just _)) -> letstrict s3 = writeMutVar v Nothing s2 in (s3,x)
      fail            -> fail

-- return Just with non put value when cannot put
tryPutMutVar :: MVar' a -> a -> MState -> ( MState, Maybe a )
tryPutMutVar v a s
  = case readMutVar v s of
      (s2,Just _) -> (s2,Just a)
      (s2,_     ) -> letstrict s3 = writeMutVar v (Just a) s2 in (s3,Nothing)
%%]

Actual MVar

%%[99
-- |Create an 'MVar' which is initially empty.
newEmptyMVar  :: IO (MVar a)
newEmptyMVar = IO $ \ s ->
    case newMutVar Nothing s of
       ( s2, svar ) -> ( s2, MVar svar )

-- |Create an 'MVar' which contains the supplied value.
newMVar :: a -> IO (MVar a)
newMVar value = do
    mvar <- newEmptyMVar
    putMVar mvar value
    return mvar
{-
    newEmptyMVar        >>= \ mvar ->
    putMVar mvar value  >>
    (trace "newMVar.ret" $ return mvar)
-}

takeMVar :: MVar a -> IO a
takeMVar (MVar mvar) = IO $ \ s ->
    case tryTakeMutVar mvar s of
      (_,Nothing) -> error "UHC.MVar.takeMVar: MVar holds nothing"
      (s2,Just a) -> (s2,a)

putMVar  :: MVar a -> a -> IO ()
putMVar (MVar mvar) x = IO $ \ s ->
    case tryPutMutVar mvar x s of
      (s2,Nothing) -> (s2,())
      _            -> error "UHC.MVar.putMVar: MVar already holds something"

-- |A non-blocking version of 'takeMVar'.  The 'tryTakeMVar' function
-- returns immediately, with 'Nothing' if the 'MVar' was empty, or
-- @'Just' a@ if the 'MVar' was full with contents @a@.  After 'tryTakeMVar',
-- the 'MVar' is left empty.
tryTakeMVar :: MVar a -> IO (Maybe a)
tryTakeMVar (MVar m) = IO $ \ s ->
    case readMutVar m s of
      (s2,x@(Just _)) -> letstrict s3 = writeMutVar m Nothing s2 in (s3,x)
      fail            -> fail

-- |A non-blocking version of 'putMVar'.  The 'tryPutMVar' function
-- attempts to put the value @a@ into the 'MVar', returning 'True' if
-- it was successful, or 'False' otherwise.
tryPutMVar  :: MVar a -> a -> IO Bool
tryPutMVar (MVar mvar) x = IO $ \ s ->
    case tryPutMutVar mvar x s of
      (s2,Nothing) -> (s2,True)
      (s2,_      ) -> (s2,False)

-- |Check whether a given 'MVar' is empty.
--
isEmptyMVar :: MVar a -> IO Bool
isEmptyMVar (MVar mv) = IO $ \ s -> 
    case readMutVar mv s of
      (s2,Just  _) -> (s2,False)
      (s2,Nothing) -> (s2,True)

withMVar :: MVar a -> (a -> IO b) -> IO b
withMVar m io = do
    a <- takeMVar m
    b <- catchAny (io a)
            (\e -> do putMVar m a; throw e)
    putMVar m a
    return b

-- |Add a finalizer to an 'MVar'.
-- Note that finalizer/weakptr support as of 20090412 is, well, weak,
-- but provide this function for GHC compatibility.
addMVarFinalizer :: MVar a -> IO () -> IO ()
addMVarFinalizer (MVar m) finalizer = do
{-
  mkWeak m () (Just finalizer)
-}
  return ()

%%]

%%[99
%%]
sameMVar :: MVar a -> MVar a -> Bool
sameMVar (MVar v1) (MVar v2) = sameMutVar v1 v2

instance Eq (MVar a) where
   (==) = sameMVar

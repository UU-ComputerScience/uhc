%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MutVar: mutable var
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
module UHC.MutVar
  ( MutVar
  , newMutVar, readMutVar, writeMutVar, sameMutVar, atomicModifyMutVar
  )
  where

import UHC.Base

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MutVar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
data MutVar s a		-- opaque

foreign import prim "primNewMutVar"   newMutVar   :: a -> State s -> (State s, MutVar s a)
foreign import prim "primReadMutVar"  readMutVar  :: MutVar s a -> State s -> (State s, a)
foreign import prim "primWriteMutVar" writeMutVar :: MutVar s a -> a -> State s -> State s
foreign import prim "primSameMutVar"  sameMutVar  :: MutVar s a -> MutVar s a -> Bool

-- no threads, hence no atomicity issues
atomicModifyMutVar :: MutVar s a -> (a -> (a,b)) -> State s -> (State s, b)
atomicModifyMutVar mv f s1
  = letstrict _ = s3 in (s3,vres)
  where (s2,v) = readMutVar mv s1
        (vnew,vres) = letstrict _ = s2 in f v
        s3 = writeMutVar mv vnew s2
%%]

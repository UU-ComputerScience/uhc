%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Provide dummy concurrency
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
EHC not yet offers threads and other concurrency features.
Module UHC.Conc provides just enough dummy implementations of threading primitives to
fool the rest of the libraries.
In particular, System.IO requires this.
%%]

%%[99
{-# LANGUAGE NoImplicitPrelude #-}
{-# EXCLUDE_IF_TARGET js #-}
{-# EXCLUDE_IF_TARGET cr #-}

module UHC.Conc
  ( threadWaitRead, threadWaitWrite
  )
  where

import UHC.Base
import UHC.OldException
import System.Posix.Types

%%]

%%[99
threadWaitRead :: Fd -> IO ()
threadWaitRead _ = return ()

threadWaitWrite :: Fd -> IO ()
threadWaitWrite _ = return ()

%%]

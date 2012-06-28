{-# LANGUAGE NoImplicitPrelude #-}
{-# EXCLUDE_IF_TARGET js #-}

module UHC.Conc
  ( threadWaitRead, threadWaitWrite
  )
  where

import UHC.Base
import UHC.OldException
import System.Posix.Types


threadWaitRead :: Fd -> IO ()
threadWaitRead _ = return ()

threadWaitWrite :: Fd -> IO ()
threadWaitWrite _ = return ()


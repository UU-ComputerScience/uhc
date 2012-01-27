{-# LANGUAGE CPP #-}
{-# EXCLUDE_IF_TARGET js #-}

module System (
    ExitCode(ExitSuccess,ExitFailure),
    getArgs, getProgName, getEnv, 
#ifndef __UHC__
    system, 
#endif
    exitWith, exitFailure
  ) where

import System.Exit
import System.Environment
#ifndef __UHC__
import System.Cmd
#endif

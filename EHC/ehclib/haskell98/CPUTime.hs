{-# EXCLUDE_IF_TARGET jscript #-}
module CPUTime (
    getCPUTime, cpuTimePrecision 
  ) where

import System.CPUTime

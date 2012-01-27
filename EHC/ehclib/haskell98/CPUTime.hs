{-# EXCLUDE_IF_TARGET js #-}
module CPUTime (
    getCPUTime, cpuTimePrecision 
  ) where

import System.CPUTime

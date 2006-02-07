-------------------------------------------------------------------------
-- Version of program
-------------------------------------------------------------------------

module Version
  ( versionInfo, versionDist
  )
  where

-------------------------------------------------------------------------
-- Version info
-------------------------------------------------------------------------

versionSvn      = "$Id$"
versionMajor    = "0"
versionMinor    = "02"
versionQuality  = "alpha"
versionDist     = versionMajor ++ "." ++ versionMinor ++ versionQuality
versionProg     = "ruler"
versionInfo     = versionProg ++ "(" ++ versionDist ++ ")" ++ ", " ++ versionSvn


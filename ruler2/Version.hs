-------------------------------------------------------------------------
-- Version of program
-- $Id$
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
versionMinor    = "03"
versionQuality  = "experimental"
versionDist     = versionMajor ++ "." ++ versionMinor ++ versionQuality
versionProg     = "ruler"
versionInfo     = versionProg ++ "(" ++ versionDist ++ ")" ++ ", " ++ versionSvn


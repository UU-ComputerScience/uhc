module Version
( versionInfo, versionDist, versionSvn )
where

versionSvn      = "$Id$"
versionMajor    = "0"
versionMinor    = "04"
versionQuality  = "alpha"
versionDist     = versionMajor ++ "." ++ versionMinor ++ versionQuality
versionProg     = "ruler"
versionInfo     = versionProg ++ "(" ++ versionDist ++ ")" ++ ", " ++ versionSvn

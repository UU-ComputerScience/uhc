-------------------------------------------------------------------------
-- Version of program
-- $Id$
-------------------------------------------------------------------------

%%[1 hs module (Version)
%%]

%%[1 hs export (versionInfo, versionDist, versionSvn)
%%]

-------------------------------------------------------------------------
-- Version info
-------------------------------------------------------------------------

%%[1 hs
versionSvn      = "$Id$"
versionMajor    = "0"
versionMinor    = "04"
versionQuality  = "alpha"
versionDist     = versionMajor ++ "." ++ versionMinor ++ versionQuality
versionProg     = "ruler"
versionInfo     = versionProg ++ "(" ++ versionDist ++ ")" ++ ", " ++ versionSvn
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Configuration, internal versions acting as a weak timestamp to distinguish different formats
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 module {%{EH}ConfigInternalVersions}
%%]

%%[50 import (Data.Word, Data.Bits)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Versions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 hs export(InternalVersionCombined)
-- | Encoding of internal version
type InternalVersion 			= Word64
type InternalVersionCombined 	= Word64

mkInternalVersion :: Int -> InternalVersion
mkInternalVersion = fromIntegral
%%]

%%[50 hs export(internalVersionCombined)
internalVersionCombined :: InternalVersionCombined
internalVersionCombined =
      internalVersionHI
  .|. (internalVersionCore 		`shiftL` 8)
  .|. (internalVersionCoreRun 	`shiftL` 16)
  .|. (internalVersionTySys 	`shiftL` 24)
  .|. (internalVersionCodeGen 	`shiftL` 32)
%%]

%%[50 hs export(internalVersionTySys, internalVersionCodeGen, internalVersionHI, internalVersionCore, internalVersionCoreRun)
-- | For variation in type inferencing
internalVersionTySys = mkInternalVersion 2

-- | For variation in code gen
internalVersionCodeGen = mkInternalVersion 1

-- | For binary/serialized HI .hi files and all data stored there
internalVersionHI = mkInternalVersion 1

-- | For binary/serialized Core .cr/.bcr/.tcr etc files
internalVersionCore = mkInternalVersion 20

-- | For binary/serialized CoreRun .crr/.bcrr./tcrr etc files
internalVersionCoreRun = mkInternalVersion 11
%%]


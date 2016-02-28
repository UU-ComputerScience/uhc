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
  .|. (internalVersionCore 		`shiftL` 6)
  .|. (internalVersionCoreRun 	`shiftL` 12)
  .|. (internalVersionTySys 	`shiftL` 18)
  .|. (internalVersionCodeGen 	`shiftL` 24)
  .|. (internalVersionEH 	    `shiftL` 30)
  .|. (internalVersionHS 	    `shiftL` 36)
%%]

%%[50 hs export(internalVersionTySys, internalVersionCodeGen, internalVersionHI, internalVersionCore, internalVersionCoreRun, internalVersionHS)
-- | For variation in type inferencing
internalVersionTySys = mkInternalVersion 1

-- | For variation in code gen
internalVersionCodeGen = mkInternalVersion 1

-- | For binary/serialized HI .hi files and all data stored there
internalVersionHI = mkInternalVersion 1

-- | For binary/serialized Core .cr/.bcr/.tcr etc files
internalVersionCore = mkInternalVersion 2

-- | For binary/serialized CoreRun .crr/.bcrr./tcrr etc files
internalVersionCoreRun = mkInternalVersion 1

-- | For binary/serialized EH intermediate structures
internalVersionEH = mkInternalVersion 2

-- | For HS frontend differences
internalVersionHS = mkInternalVersion 1

%%]


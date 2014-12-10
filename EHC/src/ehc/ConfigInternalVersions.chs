%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Configuration, internal versions acting as a weak timestamp to distinguish different formats
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 module {%{EH}ConfigInternalVersions}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Versions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 hs export(InternalVersionHI, InternalVersionCore)
-- | Encoding of internal version
type InternalVersion = Int

type InternalVersionHI 		= InternalVersion
type InternalVersionCore 	= InternalVersion

mkInternalVersion :: Int -> InternalVersion
mkInternalVersion = id
%%]

%%[50 hs export(internalVersionHI, internalVersionCore)
-- | For binary/serialized HI .hi files and all data stored there
internalVersionHI = mkInternalVersion 1

-- | For binary/serialized Core .cr/.bcr etc files
internalVersionCore = mkInternalVersion 11
%%]


module MarshalError (
  	module Foreign.Marshal.Error,
	IOErrorType,
	mkIOError,
	alreadyExistsErrorType,
	doesNotExistErrorType,
	alreadyInUseErrorType,
	fullErrorType,
	eofErrorType,
	illegalOperationErrorType,
	permissionErrorType,
	userErrorType,
	annotateIOError
  ) where

import System.IO.Error
import Foreign.Marshal.Error

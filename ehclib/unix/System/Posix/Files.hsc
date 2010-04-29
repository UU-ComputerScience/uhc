{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Files
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- Functions defined by the POSIX standards for manipulating and querying the
-- file system. Names of underlying POSIX functions are indicated whenever
-- possible. A more complete documentation of the POSIX functions together
-- with a more detailed description of different error conditions are usually
-- available in the system's manual pages or from
-- <http://www.unix.org/version3/online.html> (free registration required).
--
-- When a function that calls an underlying POSIX function fails, the errno
-- code is converted to an 'IOError' using 'Foreign.C.Error.errnoToIOError'.
-- For a list of which errno codes may be generated, consult the POSIX
-- documentation for the underlying function.
--
-----------------------------------------------------------------------------

module System.Posix.Files (
    -- * File modes
    -- FileMode exported by System.Posix.Types
    unionFileModes, intersectFileModes,
    nullFileMode,
    ownerReadMode, ownerWriteMode, ownerExecuteMode, ownerModes,
    groupReadMode, groupWriteMode, groupExecuteMode, groupModes,
    otherReadMode, otherWriteMode, otherExecuteMode, otherModes,
    setUserIDMode, setGroupIDMode,
    stdFileMode,   accessModes,
    fileTypeModes,
    blockSpecialMode, characterSpecialMode, namedPipeMode, regularFileMode,
    directoryMode, symbolicLinkMode, socketMode,

    -- ** Setting file modes
    setFileMode, setFdMode, setFileCreationMask,

    -- ** Checking file existence and permissions
    fileAccess, fileExist,

    -- * File status
    FileStatus,
    -- ** Obtaining file status
    getFileStatus, getFdStatus, getSymbolicLinkStatus,
    -- ** Querying file status
    deviceID, fileID, fileMode, linkCount, fileOwner, fileGroup,
    specialDeviceID, fileSize, accessTime, modificationTime,
    statusChangeTime,
    isBlockDevice, isCharacterDevice, isNamedPipe, isRegularFile,
    isDirectory, isSymbolicLink, isSocket,

    -- * Creation
    createNamedPipe, 
    createDevice,

    -- * Hard links
    createLink, removeLink,

    -- * Symbolic links
    createSymbolicLink, readSymbolicLink,

    -- * Renaming files
    rename,

    -- * Changing file ownership
    setOwnerAndGroup,  setFdOwnerAndGroup,
#if HAVE_LCHOWN
    setSymbolicLinkOwnerAndGroup,
#endif

    -- * Changing file timestamps
    setFileTimes, touchFile,

    -- * Setting file sizes
    setFileSize, setFdSize,

    -- * Find system-specific limits for a file
    PathVar(..), getPathVar, getFdPathVar,
  ) where

#include "HsUnix.h"

import System.Posix.Error
import System.Posix.Types
import System.IO.Unsafe
import Data.Bits
import System.Posix.Internals
import Foreign
import Foreign.C

-- -----------------------------------------------------------------------------
-- POSIX file modes

-- The abstract type 'FileMode', constants and operators for
-- manipulating the file modes defined by POSIX.

-- | No permissions.
nullFileMode :: FileMode
nullFileMode = 0

-- | Owner has read permission.
ownerReadMode :: FileMode
ownerReadMode = (#const S_IRUSR)

-- | Owner has write permission.
ownerWriteMode :: FileMode
ownerWriteMode = (#const S_IWUSR)

-- | Owner has execute permission.
ownerExecuteMode :: FileMode
ownerExecuteMode = (#const S_IXUSR)

-- | Group has read permission.
groupReadMode :: FileMode
groupReadMode = (#const S_IRGRP)

-- | Group has write permission.
groupWriteMode :: FileMode
groupWriteMode = (#const S_IWGRP)

-- | Group has execute permission.
groupExecuteMode :: FileMode
groupExecuteMode = (#const S_IXGRP)

-- | Others have read permission.
otherReadMode :: FileMode
otherReadMode = (#const S_IROTH)

-- | Others have write permission.
otherWriteMode :: FileMode
otherWriteMode = (#const S_IWOTH)

-- | Others have execute permission.
otherExecuteMode :: FileMode
otherExecuteMode = (#const S_IXOTH)

-- | Set user ID on execution.
setUserIDMode :: FileMode
setUserIDMode = (#const S_ISUID)

-- | Set group ID on execution.
setGroupIDMode :: FileMode
setGroupIDMode = (#const S_ISGID)

-- | Owner, group and others have read and write permission.
stdFileMode :: FileMode
stdFileMode = ownerReadMode  .|. ownerWriteMode .|. 
	      groupReadMode  .|. groupWriteMode .|. 
	      otherReadMode  .|. otherWriteMode

-- | Owner has read, write and execute permission.
ownerModes :: FileMode
ownerModes = (#const S_IRWXU)

-- | Group has read, write and execute permission.
groupModes :: FileMode
groupModes = (#const S_IRWXG)

-- | Others have read, write and execute permission.
otherModes :: FileMode
otherModes = (#const S_IRWXO)

-- | Owner, group and others have read, write and execute permission.
accessModes :: FileMode
accessModes = ownerModes .|. groupModes .|. otherModes

-- | Combines the two file modes into one that contains modes that appear in
-- either.
unionFileModes :: FileMode -> FileMode -> FileMode
unionFileModes m1 m2 = m1 .|. m2

-- | Combines two file modes into one that only contains modes that appear in
-- both.
intersectFileModes :: FileMode -> FileMode -> FileMode
intersectFileModes m1 m2 = m1 .&. m2

fileTypeModes :: FileMode
fileTypeModes = (#const S_IFMT)

blockSpecialMode :: FileMode
blockSpecialMode = (#const S_IFBLK)

characterSpecialMode :: FileMode
characterSpecialMode = (#const S_IFCHR)

namedPipeMode :: FileMode
namedPipeMode = (#const S_IFIFO)

regularFileMode :: FileMode
regularFileMode = (#const S_IFREG)

directoryMode :: FileMode
directoryMode = (#const S_IFDIR)

symbolicLinkMode :: FileMode
symbolicLinkMode = (#const S_IFLNK)

socketMode :: FileMode
socketMode = (#const S_IFSOCK)

-- | @setFileMode path mode@ changes permission of the file given by @path@
-- to @mode@. This operation may fail with 'throwErrnoPathIfMinus1_' if @path@
-- doesn't exist or if the effective user ID of the current process is not that
-- of the file's owner.
--
-- Note: calls @chmod@.
setFileMode :: FilePath -> FileMode -> IO ()
setFileMode name m =
  withCString name $ \s -> do
    throwErrnoPathIfMinus1_ "setFileMode" name (c_chmod s m)

-- | @setFdMode fd mode@ acts like 'setFileMode' but uses a file descriptor
-- @fd@ instead of a 'FilePath'.
--
-- Note: calls @fchmod@.
setFdMode :: Fd -> FileMode -> IO ()
setFdMode (Fd fd) m =
  throwErrnoIfMinus1_ "setFdMode" (c_fchmod fd m)

foreign import ccall unsafe "HsUnix.h fchmod" 
  c_fchmod :: CInt -> CMode -> IO CInt

-- | @setFileCreationMask mode@ sets the file mode creation mask to @mode@.
-- Modes set by this operation are subtracted from files and directories upon
-- creation. The previous file creation mask is returned.
--
-- Note: calls @umask@.
setFileCreationMask :: FileMode -> IO FileMode
setFileCreationMask mask = c_umask mask

-- -----------------------------------------------------------------------------
-- access()

-- | @fileAccess name read write exec@ checks if the file (or other file system
-- object) @name@ can be accessed for reading, writing and\/or executing. To
-- check a permission set the corresponding argument to 'True'.
--
-- Note: calls @access@.
fileAccess :: FilePath -> Bool -> Bool -> Bool -> IO Bool
fileAccess name readOK writeOK execOK = access name flags
  where
   flags   = read_f .|. write_f .|. exec_f
   read_f  = if readOK  then (#const R_OK) else 0
   write_f = if writeOK then (#const W_OK) else 0
   exec_f  = if execOK  then (#const X_OK) else 0

-- | Checks for the existence of the file.
--
-- Note: calls @access@.
fileExist :: FilePath -> IO Bool
fileExist name = 
  withCString name $ \s -> do
    r <- c_access s (#const F_OK)
    if (r == 0)
	then return True
	else do err <- getErrno
	        if (err == eNOENT)
		   then return False
		   else throwErrnoPath "fileExist" name

access :: FilePath -> CMode -> IO Bool
access name flags = 
  withCString name $ \s -> do
    r <- c_access s (fromIntegral flags)
    if (r == 0)
	then return True
	else do err <- getErrno
	        if (err == eACCES)
		   then return False
		   else throwErrnoPath "fileAccess" name

-- -----------------------------------------------------------------------------
-- stat() support

-- | POSIX defines operations to get information, such as owner, permissions,
-- size and access times, about a file. This information is represented by the
-- 'FileStatus' type.
--
-- Note: see @chmod@.
newtype FileStatus = FileStatus (ForeignPtr CStat)

-- | ID of the device on which this file resides.
deviceID         :: FileStatus -> DeviceID
-- | inode number
fileID           :: FileStatus -> FileID
-- | File mode (such as permissions).
fileMode         :: FileStatus -> FileMode
-- | Number of hard links to this file.
linkCount        :: FileStatus -> LinkCount
-- | ID of owner.
fileOwner        :: FileStatus -> UserID
-- | ID of group.
fileGroup        :: FileStatus -> GroupID
-- | Describes the device that this file represents.
specialDeviceID  :: FileStatus -> DeviceID
-- | Size of the file in bytes. If this file is a symbolic link the size is
-- the length of the pathname it contains.
fileSize         :: FileStatus -> FileOffset
-- | Time of last access.
accessTime       :: FileStatus -> EpochTime
-- | Time of last modification.
modificationTime :: FileStatus -> EpochTime
-- | Time of last status change (i.e. owner, group, link count, mode, etc.).
statusChangeTime :: FileStatus -> EpochTime

deviceID (FileStatus stat) = 
  unsafePerformIO $ withForeignPtr stat $ (#peek struct stat, st_dev)
fileID (FileStatus stat) = 
  unsafePerformIO $ withForeignPtr stat $ (#peek struct stat, st_ino)
fileMode (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ (#peek struct stat, st_mode)
linkCount (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ (#peek struct stat, st_nlink)
fileOwner (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ (#peek struct stat, st_uid)
fileGroup (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ (#peek struct stat, st_gid)
specialDeviceID (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ (#peek struct stat, st_rdev)
fileSize (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ (#peek struct stat, st_size)
accessTime (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ (#peek struct stat, st_atime)
modificationTime (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ (#peek struct stat, st_mtime)
statusChangeTime (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ (#peek struct stat, st_ctime)

-- | Checks if this file is a block device.
isBlockDevice     :: FileStatus -> Bool
-- | Checks if this file is a character device.
isCharacterDevice :: FileStatus -> Bool
-- | Checks if this file is a named pipe device.
isNamedPipe       :: FileStatus -> Bool
-- | Checks if this file is a regular file device.
isRegularFile     :: FileStatus -> Bool
-- | Checks if this file is a directory device.
isDirectory       :: FileStatus -> Bool
-- | Checks if this file is a symbolic link device.
isSymbolicLink    :: FileStatus -> Bool
-- | Checks if this file is a socket device.
isSocket          :: FileStatus -> Bool

isBlockDevice stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == blockSpecialMode
isCharacterDevice stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == characterSpecialMode
isNamedPipe stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == namedPipeMode
isRegularFile stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == regularFileMode
isDirectory stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == directoryMode
isSymbolicLink stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == symbolicLinkMode
isSocket stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == socketMode

-- | @getFileStatus path@ calls gets the @FileStatus@ information (user ID,
-- size, access times, etc.) for the file @path@.
--
-- Note: calls @stat@.
getFileStatus :: FilePath -> IO FileStatus
getFileStatus path = do
  fp <- mallocForeignPtrBytes (#const sizeof(struct stat)) 
  withForeignPtr fp $ \p ->
    withCString path $ \s -> 
      throwErrnoPathIfMinus1_ "getFileStatus" path (c_stat s p)
  return (FileStatus fp)

-- | @getFdStatus fd@ acts as 'getFileStatus' but uses a file descriptor @fd@.
--
-- Note: calls @fstat@.
getFdStatus :: Fd -> IO FileStatus
getFdStatus (Fd fd) = do
  fp <- mallocForeignPtrBytes (#const sizeof(struct stat)) 
  withForeignPtr fp $ \p ->
    throwErrnoIfMinus1_ "getFdStatus" (c_fstat fd p)
  return (FileStatus fp)

-- | Acts as 'getFileStatus' except when the 'FilePath' refers to a symbolic
-- link. In that case the @FileStatus@ information of the symbolic link itself
-- is returned instead of that of the file it points to.
--
-- Note: calls @lstat@.
getSymbolicLinkStatus :: FilePath -> IO FileStatus
getSymbolicLinkStatus path = do
  fp <- mallocForeignPtrBytes (#const sizeof(struct stat)) 
  withForeignPtr fp $ \p ->
    withCString path $ \s -> 
      throwErrnoPathIfMinus1_ "getSymbolicLinkStatus" path (c_lstat s p)
  return (FileStatus fp)

foreign import ccall unsafe "HsUnix.h __hsunix_lstat" 
  c_lstat :: CString -> Ptr CStat -> IO CInt

-- | @createNamedPipe fifo mode@  
-- creates a new named pipe, @fifo@, with permissions based on
-- @mode@. May fail with 'throwErrnoPathIfMinus1_' if a file named @name@
-- already exists or if the effective user ID of the current process doesn't
-- have permission to create the pipe.
--
-- Note: calls @mkfifo@.
createNamedPipe :: FilePath -> FileMode -> IO ()
createNamedPipe name mode = do
  withCString name $ \s -> 
    throwErrnoPathIfMinus1_ "createNamedPipe" name (c_mkfifo s mode)

-- | @createDevice path mode dev@ creates either a regular or a special file
-- depending on the value of @mode@ (and @dev@).  @mode@ will normally be either
-- 'blockSpecialMode' or 'characterSpecialMode'.  May fail with
-- 'throwErrnoPathIfMinus1_' if a file named @name@ already exists or if the
-- effective user ID of the current process doesn't have permission to create
-- the file.
--
-- Note: calls @mknod@.
createDevice :: FilePath -> FileMode -> DeviceID -> IO ()
createDevice path mode dev =
  withCString path $ \s ->
    throwErrnoPathIfMinus1_ "createDevice" path (c_mknod s mode dev)

foreign import ccall unsafe "HsUnix.h __hsunix_mknod" 
  c_mknod :: CString -> CMode -> CDev -> IO CInt

-- -----------------------------------------------------------------------------
-- Hard links

-- | @createLink old new@ creates a new path, @new@, linked to an existing file,
-- @old@.
--
-- Note: calls @link@.
createLink :: FilePath -> FilePath -> IO ()
createLink name1 name2 =
  withCString name1 $ \s1 ->
  withCString name2 $ \s2 ->
  throwErrnoPathIfMinus1_ "createLink" name1 (c_link s1 s2)

-- | @removeLink path@ removes the link named @path@.
--
-- Note: calls @unlink@.
removeLink :: FilePath -> IO ()
removeLink name =
  withCString name $ \s ->
  throwErrnoPathIfMinus1_ "removeLink" name (c_unlink s)

-- -----------------------------------------------------------------------------
-- Symbolic Links

-- | @createSymbolicLink file1 file2@ creates a symbolic link named @file2@
-- which points to the file @file1@.
--
-- Symbolic links are interpreted at run-time as if the contents of the link
-- had been substituted into the path being followed to find a file or directory.
--
-- Note: calls @symlink@.
createSymbolicLink :: FilePath -> FilePath -> IO ()
createSymbolicLink file1 file2 =
  withCString file1 $ \s1 ->
  withCString file2 $ \s2 ->
  throwErrnoPathIfMinus1_ "createSymbolicLink" file1 (c_symlink s1 s2)

foreign import ccall unsafe "HsUnix.h symlink"
  c_symlink :: CString -> CString -> IO CInt

-- ToDo: should really use SYMLINK_MAX, but not everyone supports it yet,
-- and it seems that the intention is that SYMLINK_MAX is no larger than
-- PATH_MAX.
#if !defined(PATH_MAX)
-- PATH_MAX is not defined on systems with unlimited path length.
-- Ugly.  Fix this.
#define PATH_MAX 4096
#endif

-- | Reads the @FilePath@ pointed to by the symbolic link and returns it.
--
-- Note: calls @readlink@.
readSymbolicLink :: FilePath -> IO FilePath
readSymbolicLink file =
  allocaArray0 (#const PATH_MAX) $ \buf -> do
    withCString file $ \s -> do
      len <- throwErrnoPathIfMinus1 "readSymbolicLink" file $ 
	c_readlink s buf (#const PATH_MAX)
      peekCStringLen (buf,fromIntegral len)

foreign import ccall unsafe "HsUnix.h readlink"
  c_readlink :: CString -> CString -> CSize -> IO CInt

-- -----------------------------------------------------------------------------
-- Renaming files

-- | @rename old new@ renames a file or directory from @old@ to @new@.
--
-- Note: calls @rename@.
rename :: FilePath -> FilePath -> IO ()
rename name1 name2 =
  withCString name1 $ \s1 ->
  withCString name2 $ \s2 ->
  throwErrnoPathIfMinus1_ "rename" name1 (c_rename s1 s2)

foreign import ccall unsafe "HsUnix.h rename"
   c_rename :: CString -> CString -> IO CInt

-- -----------------------------------------------------------------------------
-- chown()

-- | @setOwnerAndGroup path uid gid@ changes the owner and group of @path@ to
-- @uid@ and @gid@, respectively.
--
-- If @uid@ or @gid@ is specified as -1, then that ID is not changed.
--
-- Note: calls @chown@.
setOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO ()
setOwnerAndGroup name uid gid = do
  withCString name $ \s ->
    throwErrnoPathIfMinus1_ "setOwnerAndGroup" name (c_chown s uid gid)

foreign import ccall unsafe "HsUnix.h chown"
  c_chown :: CString -> CUid -> CGid -> IO CInt

-- | Acts as 'setOwnerAndGroup' but uses a file descriptor instead of a
-- 'FilePath'.
--
-- Note: calls @fchown@.
setFdOwnerAndGroup :: Fd -> UserID -> GroupID -> IO ()
setFdOwnerAndGroup (Fd fd) uid gid = 
  throwErrnoIfMinus1_ "setFdOwnerAndGroup" (c_fchown fd uid gid)

foreign import ccall unsafe "HsUnix.h fchown"
  c_fchown :: CInt -> CUid -> CGid -> IO CInt

#if HAVE_LCHOWN
-- | Acts as 'setOwnerAndGroup' but does not follow symlinks (and thus
-- changes permissions on the link itself).
--
-- Note: calls @lchown@.
setSymbolicLinkOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO ()
setSymbolicLinkOwnerAndGroup name uid gid = do
  withCString name $ \s ->
    throwErrnoPathIfMinus1_ "setSymbolicLinkOwnerAndGroup" name
	(c_lchown s uid gid)

foreign import ccall unsafe "HsUnix.h lchown"
  c_lchown :: CString -> CUid -> CGid -> IO CInt
#endif

-- -----------------------------------------------------------------------------
-- utime()

-- | @setFileTimes path atime mtime@ sets the access and modification times
-- associated with file @path@ to @atime@ and @mtime@, respectively.
--
-- Note: calls @utime@.
setFileTimes :: FilePath -> EpochTime -> EpochTime -> IO ()
setFileTimes name atime mtime = do
  withCString name $ \s ->
   allocaBytes (#const sizeof(struct utimbuf)) $ \p -> do
     (#poke struct utimbuf, actime)  p atime
     (#poke struct utimbuf, modtime) p mtime
     throwErrnoPathIfMinus1_ "setFileTimes" name (c_utime s p)

-- | @touchFile path@ sets the access and modification times associated with
-- file @path@ to the current time.
--
-- Note: calls @utime@.
touchFile :: FilePath -> IO ()
touchFile name = do
  withCString name $ \s ->
   throwErrnoPathIfMinus1_ "touchFile" name (c_utime s nullPtr)

-- -----------------------------------------------------------------------------
-- Setting file sizes

-- | Truncates the file down to the specified length. If the file was larger
-- than the given length before this operation was performed the extra is lost.
--
-- Note: calls @truncate@.
setFileSize :: FilePath -> FileOffset -> IO ()
setFileSize file off = 
  withCString file $ \s ->
    throwErrnoPathIfMinus1_ "setFileSize" file (c_truncate s off)

foreign import ccall unsafe "HsUnix.h truncate"
  c_truncate :: CString -> COff -> IO CInt

-- | Acts as 'setFileSize' but uses a file descriptor instead of a 'FilePath'.
--
-- Note: calls @ftruncate@.
setFdSize :: Fd -> FileOffset -> IO ()
setFdSize (Fd fd) off =
  throwErrnoIfMinus1_ "setFdSize" (c_ftruncate fd off)

-- -----------------------------------------------------------------------------
-- pathconf()/fpathconf() support

data PathVar
  = FileSizeBits		  {- _PC_FILESIZEBITS     -}
  | LinkLimit                     {- _PC_LINK_MAX         -}
  | InputLineLimit                {- _PC_MAX_CANON        -}
  | InputQueueLimit               {- _PC_MAX_INPUT        -}
  | FileNameLimit                 {- _PC_NAME_MAX         -}
  | PathNameLimit                 {- _PC_PATH_MAX         -}
  | PipeBufferLimit               {- _PC_PIPE_BUF         -}
				  -- These are described as optional in POSIX:
  				  {- _PC_ALLOC_SIZE_MIN     -}
  				  {- _PC_REC_INCR_XFER_SIZE -}
  				  {- _PC_REC_MAX_XFER_SIZE  -}
  				  {- _PC_REC_MIN_XFER_SIZE  -}
 				  {- _PC_REC_XFER_ALIGN     -}
  | SymbolicLinkLimit		  {- _PC_SYMLINK_MAX      -}
  | SetOwnerAndGroupIsRestricted  {- _PC_CHOWN_RESTRICTED -}
  | FileNamesAreNotTruncated      {- _PC_NO_TRUNC         -}
  | VDisableChar		  {- _PC_VDISABLE         -}
  | AsyncIOAvailable		  {- _PC_ASYNC_IO         -}
  | PrioIOAvailable		  {- _PC_PRIO_IO          -}
  | SyncIOAvailable		  {- _PC_SYNC_IO          -}

pathVarConst :: PathVar -> CInt
pathVarConst v = case v of
	LinkLimit     			-> (#const _PC_LINK_MAX)
	InputLineLimit			-> (#const _PC_MAX_CANON)
	InputQueueLimit			-> (#const _PC_MAX_INPUT)
	FileNameLimit			-> (#const _PC_NAME_MAX)
	PathNameLimit			-> (#const _PC_PATH_MAX)
	PipeBufferLimit			-> (#const _PC_PIPE_BUF)
	SetOwnerAndGroupIsRestricted	-> (#const _PC_CHOWN_RESTRICTED)
	FileNamesAreNotTruncated	-> (#const _PC_NO_TRUNC)
	VDisableChar			-> (#const _PC_VDISABLE)

#ifdef _PC_SYNC_IO
	SyncIOAvailable		-> (#const _PC_SYNC_IO)
#else
	SyncIOAvailable		-> error "_PC_SYNC_IO not available"
#endif

#ifdef _PC_ASYNC_IO
	AsyncIOAvailable	-> (#const _PC_ASYNC_IO)
#else
	AsyncIOAvailable	-> error "_PC_ASYNC_IO not available"
#endif

#ifdef _PC_PRIO_IO
	PrioIOAvailable		-> (#const _PC_PRIO_IO)
#else
	PrioIOAvailable		-> error "_PC_PRIO_IO not available"
#endif

#if _PC_FILESIZEBITS
	FileSizeBits		-> (#const _PC_FILESIZEBITS)
#else
	FileSizeBits		-> error "_PC_FILESIZEBITS not available"
#endif

#if _PC_SYMLINK_MAX
	SymbolicLinkLimit	-> (#const _PC_SYMLINK_MAX)
#else
	SymbolicLinkLimit	-> error "_PC_SYMLINK_MAX not available"
#endif


-- | @getPathVar var path@ obtains the dynamic value of the requested
-- configurable file limit or option associated with file or directory @path@.
-- For defined file limits, @getPathVar@ returns the associated
-- value.  For defined file options, the result of @getPathVar@
-- is undefined, but not failure.
--
-- Note: calls @pathconf@.
getPathVar :: FilePath -> PathVar -> IO Limit
getPathVar name v = do
  withCString name $ \ nameP -> 
    throwErrnoPathIfMinus1 "getPathVar" name $ 
      c_pathconf nameP (pathVarConst v)

foreign import ccall unsafe "HsUnix.h pathconf" 
  c_pathconf :: CString -> CInt -> IO CLong


-- | @getFdPathVar var fd@ obtains the dynamic value of the requested
-- configurable file limit or option associated with the file or directory
-- attached to the open channel @fd@. For defined file limits, @getFdPathVar@
-- returns the associated value.  For defined file options, the result of
-- @getFdPathVar@ is undefined, but not failure.
--
-- Note: calls @fpathconf@.
getFdPathVar :: Fd -> PathVar -> IO Limit
getFdPathVar (Fd fd) v =
    throwErrnoIfMinus1 "getFdPathVar" $ 
      c_fpathconf fd (pathVarConst v)

foreign import ccall unsafe "HsUnix.h fpathconf" 
  c_fpathconf :: CInt -> CInt -> IO CLong

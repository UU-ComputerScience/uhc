module System.Posix.Directory (
   -- * Creating and removing directories
   createDirectory, removeDirectory,

   -- * Reading directories
   DirStream,
   openDirStream,
   readDirStream,
   rewindDirStream,   
   closeDirStream,
   DirStreamOffset,
   tellDirStream,
   seekDirStream,

   -- * The working dirctory
   getWorkingDirectory,
   changeWorkingDirectory,
   changeWorkingDirectoryFd,
  ) where

import System.IO.Error
--import System.Posix.Error
import System.Posix.Types
import System.Posix.Internals
import Foreign
import Foreign.C

-- | @createDirectory dir mode@ calls @mkdir@ to 
--   create a new directory, @dir@, with permissions based on
--  @mode@.
createDirectory :: FilePath -> FileMode -> IO ()
createDirectory name mode =
  withCString name $ \s -> 
    throwErrnoPathIfMinus1_ "createDirectory" name (c_mkdir s mode)  

foreign import ccall unsafe "mkdir"
  c_mkdir :: CString -> CMode -> IO CInt

newtype DirStream = DirStream (Ptr CDir)

-- | @openDirStream dir@ calls @opendir@ to obtain a
--   directory stream for @dir@.
openDirStream :: FilePath -> IO DirStream
openDirStream name =
  withCString name $ \s -> do
    dirp <- throwErrnoPathIfNull "openDirStream" name $ c_opendir s
    return (DirStream dirp)

-- | @readDirStream dp@ calls @readdir@ to obtain the
--   next directory entry (@struct dirent@) for the open directory
--   stream @dp@, and returns the @d_name@ member of that
--  structure.
readDirStream :: DirStream -> IO FilePath
readDirStream (DirStream dirp) =
  alloca $ \ptr_dEnt  -> loop ptr_dEnt
 where
  loop ptr_dEnt = do
    resetErrno
    r <- readdir dirp ptr_dEnt
    if (r == 0)
	 then do dEnt <- peek ptr_dEnt
		 if (dEnt == nullPtr)
		    then return []
		    else do
	 	     entry <- (d_name dEnt >>= peekCString)
		     freeDirEnt dEnt
		     return entry
	 else do errno <- getErrno
		 if (errno == eINTR) then loop ptr_dEnt else do
		 let (Errno eo) = errno
		 if (eo == end_of_dir)
		    then return []
		    else throwErrno "readDirStream"

-- | @rewindDirStream dp@ calls @rewinddir@ to reposition
--   the directory stream @dp@ at the beginning of the directory.
rewindDirStream :: DirStream -> IO ()
rewindDirStream (DirStream dirp) = c_rewinddir dirp

-- | @closeDirStream dp@ calls @closedir@ to close
--   the directory stream @dp@.
closeDirStream :: DirStream -> IO ()
closeDirStream (DirStream dirp) = do
  throwErrnoIfMinus1_ "closeDirStream" (c_closedir dirp)

newtype DirStreamOffset = DirStreamOffset COff

seekDirStream :: DirStream -> DirStreamOffset -> IO ()
seekDirStream (DirStream dirp) (DirStreamOffset off) =
  c_seekdir dirp off

tellDirStream :: DirStream -> IO DirStreamOffset
tellDirStream (DirStream dirp) = do
  off <- c_telldir dirp
  return (DirStreamOffset off)

{-
 Renamings of functionality provided via Directory interface,
 kept around for b.wards compatibility and for having more POSIXy
 names
-}

-- | @getWorkingDirectory@ calls @getcwd@ to obtain the name
--   of the current working directory.
getWorkingDirectory :: IO FilePath
getWorkingDirectory = do
  p <- mallocBytes long_path_size
  go p long_path_size
  where go p bytes = do
    	  p' <- c_getcwd p (fromIntegral bytes)
	  if p' /= nullPtr 
	     then do s <- peekCString p'
		     free p'
		     return s
	     else do errno <- getErrno
		     if errno == eRANGE
		        then do let bytes' = bytes * 2
			        p'' <- reallocBytes p bytes'
			        go p'' bytes'
		        else throwErrno "getCurrentDirectory"

-- | @changeWorkingDirectory dir@ calls @chdir@ to change
--   the current working directory to @dir@.
changeWorkingDirectory :: FilePath -> IO ()
changeWorkingDirectory path =
  modifyIOError (`ioeSetFileName` path) $
    withCString path $ \s -> 
       throwErrnoIfMinus1Retry_ "changeWorkingDirectory" (c_chdir s)

removeDirectory :: FilePath -> IO ()
removeDirectory path =
  modifyIOError (`ioeSetFileName` path) $
    withCString path $ \s ->
       throwErrnoIfMinus1Retry_ "removeDirectory" (c_rmdir s)

changeWorkingDirectoryFd :: Fd -> IO ()
changeWorkingDirectoryFd (Fd fd) = 
  throwErrnoIfMinus1_ "changeWorkingDirectoryFd" (c_fchdir fd)


foreign import ccall unsafe "HsUnix.h seekdir"
  c_seekdir :: Ptr CDir -> COff -> IO ()

foreign import ccall unsafe "HsUnix.h telldir"
  c_telldir :: Ptr CDir -> IO COff

foreign import ccall unsafe "HsUnix.h getcwd"
   c_getcwd   :: Ptr CChar -> CSize -> IO (Ptr CChar)

foreign import ccall unsafe "HsUnix.h __hsunix_long_path_size"
  long_path_size :: Int

foreign import ccall unsafe "HsUnix.h chdir"
   c_chdir :: CString -> IO CInt

foreign import ccall unsafe "HsUnix.h rmdir"
   c_rmdir :: CString -> IO CInt

foreign import ccall unsafe "HsUnix.h fchdir"
  c_fchdir :: CInt -> IO CInt 


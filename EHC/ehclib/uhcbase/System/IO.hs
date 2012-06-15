{-# LANGUAGE NoImplicitPrelude, CPP #-}
{-# OPTIONS_GHC -XNoImplicitPrelude -#include "HsBase.h" #-}
{-# EXCLUDE_IF_TARGET js #-}
{-# OPTIONS_UHC "--optP=-traditional-cpp" #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.IO
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The standard IO library.
--
-----------------------------------------------------------------------------


module System.IO (
    -- * The IO monad

    IO,                        -- instance MonadFix
    fixIO,                     -- :: (a -> IO a) -> IO a

    -- * Files and handles

    FilePath,                  -- :: String

    Handle,             -- abstract, instance of: Eq, Show.

    -- ** Standard handles

    -- | Three handles are allocated during program initialisation,
    -- and are initially open.

    stdin, stdout, stderr,   -- :: Handle

    -- * Opening and closing files

    -- ** Opening files

    withFile,
    openFile,                  -- :: FilePath -> IOMode -> IO Handle
    IOMode(ReadMode,WriteMode,AppendMode,ReadWriteMode),

    -- ** Closing files

    hClose,                    -- :: Handle -> IO ()

    -- ** Special cases

    -- | These functions are also exported by the "Prelude".

    readFile,                  -- :: FilePath -> IO String
    writeFile,                 -- :: FilePath -> String -> IO ()
    appendFile,                -- :: FilePath -> String -> IO ()

    -- ** File locking

    -- $locking

    -- * Operations on handles

    -- ** Determining and changing the size of a file

    hFileSize,                 -- :: Handle -> IO Integer
#ifdef __GLASGOW_HASKELL__
    hSetFileSize,              -- :: Handle -> Integer -> IO ()
#endif

    -- ** Detecting the end of input

    hIsEOF,                    -- :: Handle -> IO Bool
    isEOF,                     -- :: IO Bool

    -- ** Buffering operations

    BufferMode(NoBuffering,LineBuffering,BlockBuffering),
    hSetBuffering,             -- :: Handle -> BufferMode -> IO ()
    hGetBuffering,             -- :: Handle -> IO BufferMode
    hFlush,                    -- :: Handle -> IO ()

    -- ** Repositioning handles

    hGetPosn,                  -- :: Handle -> IO HandlePosn
    hSetPosn,                  -- :: HandlePosn -> IO ()
    HandlePosn,                -- abstract, instance of: Eq, Show.

    hSeek,                     -- :: Handle -> SeekMode -> Integer -> IO ()
    SeekMode(AbsoluteSeek,RelativeSeek,SeekFromEnd),
#if !defined(__NHC__)
    hTell,                     -- :: Handle -> IO Integer
#endif

    -- ** Handle properties

    hIsOpen, hIsClosed,        -- :: Handle -> IO Bool
    hIsReadable, hIsWritable,  -- :: Handle -> IO Bool
    hIsSeekable,               -- :: Handle -> IO Bool

    -- ** Terminal operations (not portable: GHC\/Hugs only)

#if !defined(__NHC__)
    hIsTerminalDevice,          -- :: Handle -> IO Bool

    hSetEcho,                   -- :: Handle -> Bool -> IO ()
    hGetEcho,                   -- :: Handle -> IO Bool
#endif

    -- ** Showing handle state (not portable: GHC only)

#ifdef __GLASGOW_HASKELL__
    hShow,                      -- :: Handle -> IO String
#endif

    -- * Text input and output

    -- ** Text input
    hWaitForInput,             -- :: Handle -> Int -> IO Bool
    hReady,                    -- :: Handle -> IO Bool
    hGetChar,                  -- :: Handle -> IO Char
    hGetLine,                  -- :: Handle -> IO [Char]
    hLookAhead,                -- :: Handle -> IO Char
    hGetContents,              -- :: Handle -> IO [Char]
    -- ** Text output

    hPutChar,                  -- :: Handle -> Char -> IO ()
    hPutStr,                   -- :: Handle -> [Char] -> IO ()
    hPutStrLn,                 -- :: Handle -> [Char] -> IO ()
    hPrint,                    -- :: Show a => Handle -> a -> IO ()

    -- ** Special cases for standard input and output

    -- | These functions are also exported by the "Prelude".

    interact,                  -- :: (String -> String) -> IO ()
    putChar,                   -- :: Char   -> IO ()
    putStr,                    -- :: String -> IO () 
    putStrLn,                  -- :: String -> IO ()
    print,                     -- :: Show a => a -> IO ()
    getChar,                   -- :: IO Char
    getLine,                   -- :: IO String
    getContents,               -- :: IO String
    readIO,                    -- :: Read a => String -> IO a
    readLn,                    -- :: Read a => IO a

    -- * Binary input and output
    withBinaryFile,
    openBinaryFile,            -- :: FilePath -> IOMode -> IO Handle
    hSetBinaryMode,            -- :: Handle -> Bool -> IO ()
    hPutBuf,                   -- :: Handle -> Ptr a -> Int -> IO ()
    hGetBuf,                   -- :: Handle -> Ptr a -> Int -> IO Int
#if !defined(__NHC__) && !defined(__HUGS__)
    hPutBufNonBlocking,        -- :: Handle -> Ptr a -> Int -> IO Int
    hGetBufNonBlocking,        -- :: Handle -> Ptr a -> Int -> IO Int
#endif
    -- * Temporary files

    openTempFile,
    openBinaryTempFile,
  ) where

#ifndef __NHC__
import Data.Bits
import Data.List
import Data.Maybe
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import System.Posix.Internals

#endif

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.IOBase       -- Together these four Prelude modules define
import GHC.Handle       -- all the stuff exported by IO for the GHC version
import GHC.IO
import GHC.Exception
import GHC.Num
import Text.Read
import GHC.Show

#endif

#ifdef __UHC__
import UHC.Base
import UHC.IOBase
import UHC.Handle
import UHC.IO
import UHC.OldException (bracket, onException)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.IO.Fix (fixIO)
#endif

#ifdef __HUGS__
import Hugs.IO
import Hugs.IOExts
import Hugs.IORef
import System.IO.Unsafe ( unsafeInterleaveIO )

#endif

#ifdef __NHC__

import IO
  ( Handle ()
  , HandlePosn ()
  , IOMode (ReadMode,WriteMode,AppendMode,ReadWriteMode)
  , BufferMode (NoBuffering,LineBuffering,BlockBuffering)
  , SeekMode (AbsoluteSeek,RelativeSeek,SeekFromEnd)
  , stdin, stdout, stderr
  , openFile                  -- :: FilePath -> IOMode -> IO Handle
  , hClose                    -- :: Handle -> IO ()
  , hFileSize                 -- :: Handle -> IO Integer
  , hIsEOF                    -- :: Handle -> IO Bool
  , isEOF                     -- :: IO Bool
  , hSetBuffering             -- :: Handle -> BufferMode -> IO ()
  , hGetBuffering             -- :: Handle -> IO BufferMode
  , hFlush                    -- :: Handle -> IO ()
  , hGetPosn                  -- :: Handle -> IO HandlePosn
  , hSetPosn                  -- :: HandlePosn -> IO ()
  , hSeek                     -- :: Handle -> SeekMode -> Integer -> IO ()
  , hWaitForInput             -- :: Handle -> Int -> IO Bool
  , hGetChar                  -- :: Handle -> IO Char
  , hGetLine                  -- :: Handle -> IO [Char]
  , hLookAhead                -- :: Handle -> IO Char
  , hGetContents              -- :: Handle -> IO [Char]
  , hPutChar                  -- :: Handle -> Char -> IO ()
  , hPutStr                   -- :: Handle -> [Char] -> IO ()
  , hPutStrLn                 -- :: Handle -> [Char] -> IO ()
  , hPrint                    -- :: Handle -> [Char] -> IO ()
  , hReady                    -- :: Handle -> [Char] -> IO ()
  , hIsOpen, hIsClosed        -- :: Handle -> IO Bool
  , hIsReadable, hIsWritable  -- :: Handle -> IO Bool
  , hIsSeekable               -- :: Handle -> IO Bool
  , bracket

  , IO ()
  , FilePath                  -- :: String
  )
import NHC.IOExtras (fixIO, hPutBuf, hGetBuf)
import NHC.FFI (Ptr)

#endif

-- -----------------------------------------------------------------------------
-- Standard IO
#if  defined(__GLASGOW_HASKELL__) || defined(__UHC__)
-- | Write a character to the standard output device
-- (same as 'hPutChar' 'stdout').

putChar         :: Char -> IO ()
putChar c       =  hPutChar stdout c

-- | Write a string to the standard output device
-- (same as 'hPutStr' 'stdout').

putStr          :: String -> IO ()
putStr s        =  hPutStr stdout s

-- | The same as 'putStr', but adds a newline character.

putStrLn        :: String -> IO ()
putStrLn s      =  do putStr s 
                      putChar '\n'

-- | The 'print' function outputs a value of any printable type to the
-- standard output device.
-- Printable types are those that are instances of class 'Show'; 'print'
-- converts values to strings for output using the 'show' operation and
-- adds a newline.
--
-- For example, a program to print the first 20 integers and their
-- powers of 2 could be written as:
--
-- > main = print ([(n, 2^n) | n <- [0..19]])

print           :: Show a => a -> IO ()
print x         =  putStrLn (show x)

-- | Read a character from the standard input device
-- (same as 'hGetChar' 'stdin').

getChar         :: IO Char
getChar         =  hGetChar stdin

-- | Read a line from the standard input device
-- (same as 'hGetLine' 'stdin').

getLine         :: IO String
getLine         =  hGetLine stdin

-- | The 'getContents' operation returns all user input as a single string,
-- which is read lazily as it is needed
-- (same as 'hGetContents' 'stdin').

getContents     :: IO String
getContents     =  hGetContents stdin

-- | The 'interact' function takes a function of type @String->String@
-- as its argument.  The entire input from the standard input device is
-- passed to this function as its argument, and the resulting string is
-- output on the standard output device.

interact        ::  (String -> String) -> IO ()
interact f      =   do s <- getContents
                       putStr (f s)

-- | The 'readFile' function reads a file and
-- returns the contents of the file as a string.
-- The file is read lazily, on demand, as with 'getContents'.

readFile        :: FilePath -> IO String
readFile name   =  openFile name ReadMode >>= hGetContents

-- | The computation 'writeFile' @file str@ function writes the string @str@,
-- to the file @file@.
writeFile :: FilePath -> String -> IO ()
writeFile f txt = withFile f WriteMode (\ hdl -> hPutStr hdl txt)

-- | The computation 'appendFile' @file str@ function appends the string @str@,
-- to the file @file@.
--
-- Note that 'writeFile' and 'appendFile' write a literal string
-- to a file.  To write a value of any printable type, as with 'print',
-- use the 'show' function to convert the value to a string first.
--
-- > main = appendFile "squares" (show [(x,x*x) | x <- [0,0.1..2]])

appendFile      :: FilePath -> String -> IO ()
appendFile f txt = withFile f AppendMode (\ hdl -> hPutStr hdl txt)

-- | The 'readLn' function combines 'getLine' and 'readIO'.

readLn          :: Read a => IO a
readLn          =  do l <- getLine
                      r <- readIO l
                      return r

-- | The 'readIO' function is similar to 'read' except that it signals
-- parse failure to the 'IO' monad instead of terminating the program.

readIO          :: Read a => String -> IO a
readIO s        =  case (do { (x,t) <- reads s ;
                              ("","") <- lex t ;
                              return x }) of
                        [x]    -> return x
                        []     -> ioError (userError "Prelude.readIO: no parse")
                        _      -> ioError (userError "Prelude.readIO: ambiguous parse")
#endif  /* __GLASGOW_HASKELL__ */

#ifndef __NHC__
-- | Computation 'hReady' @hdl@ indicates whether at least one item is
-- available for input from handle @hdl@.
-- 
-- This operation may fail with:
--
--  * 'System.IO.Error.isEOFError' if the end of file has been reached.

hReady          :: Handle -> IO Bool
hReady h        =  hWaitForInput h 0

-- | The same as 'hPutStr', but adds a newline character.

hPutStrLn       :: Handle -> String -> IO ()
hPutStrLn hndl str = do
 hPutStr  hndl str
 hPutChar hndl '\n'

-- | Computation 'hPrint' @hdl t@ writes the string representation of @t@
-- given by the 'shows' function to the file or channel managed by @hdl@
-- and appends a newline.
--
-- This operation may fail with:
--
--  * 'System.IO.Error.isFullError' if the device is full; or
--
--  * 'System.IO.Error.isPermissionError' if another system resource limit would be exceeded.

hPrint          :: Show a => Handle -> a -> IO ()
hPrint hdl      =  hPutStrLn hdl . show
#endif /* !__NHC__ */

-- | @'withFile' name mode act@ opens a file using 'openFile' and passes
-- the resulting handle to the computation @act@.  The handle will be
-- closed on exit from 'withFile', whether by normal termination or by
-- raising an exception.
withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile name mode = bracket (openFile name mode) hClose

-- | @'withBinaryFile' name mode act@ opens a file using 'openBinaryFile'
-- and passes the resulting handle to the computation @act@.  The handle
-- will be closed on exit from 'withBinaryFile', whether by normal
-- termination or by raising an exception.
withBinaryFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile name mode = bracket (openBinaryFile name mode) hClose

-- ---------------------------------------------------------------------------
-- fixIO

#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
fixIO :: (a -> IO a) -> IO a
fixIO k = do
    ref <- newIORef (throw NonTermination)
    ans <- unsafeInterleaveIO (readIORef ref)
    result <- k ans
    writeIORef ref result
    return result

#elif defined(__UHC__)
-- See System.IO.Fix
#endif

#if defined(__NHC__)
-- Assume a unix platform, where text and binary I/O are identical.
openBinaryFile = openFile
hSetBinaryMode _ _ = return ()
#endif

-- | The function creates a temporary file in ReadWrite mode.
-- The created file isn\'t deleted automatically, so you need to delete it manually.
--
-- The file is creates with permissions such that only the current
-- user can read\/write it.
--
-- With some exceptions (see below), the file will be created securely
-- in the sense that an attacker should not be able to cause
-- openTempFile to overwrite another file on the filesystem using your
-- credentials, by putting symbolic links (on Unix) in the place where
-- the temporary file is to be created.  On Unix the @O_CREAT@ and
-- @O_EXCL@ flags are used to prevent this attack, but note that
-- @O_EXCL@ is sometimes not supported on NFS filesystems, so if you
-- rely on this behaviour it is best to use local filesystems only.
--
openTempFile :: FilePath   -- ^ Directory in which to create the file
             -> String     -- ^ File name template. If the template is \"foo.ext\" then
                           -- the created file will be \"fooXXX.ext\" where XXX is some
                           -- random number.
             -> IO (FilePath, Handle)
openTempFile tmp_dir template = openTempFile' "openTempFile" tmp_dir template False

-- | Like 'openTempFile', but opens the file in binary mode. See 'openBinaryFile' for more comments.
openBinaryTempFile :: FilePath -> String -> IO (FilePath, Handle)
openBinaryTempFile tmp_dir template = openTempFile' "openBinaryTempFile" tmp_dir template True

openTempFile' :: String -> FilePath -> String -> Bool -> IO (FilePath, Handle)
openTempFile' loc tmp_dir template binary = do
  pid <- c_getpid
  findTempName pid
  where
    -- We split off the last extension, so we can use .foo.ext files
    -- for temporary files (hidden on Unix OSes). Unfortunately we're
    -- below filepath in the hierarchy here.
    (prefix,suffix) =
       case break (== '.') $ reverse template of
         -- First case: template contains no '.'s. Just re-reverse it.
         (rev_suffix, "")       -> (reverse rev_suffix, "")
         -- Second case: template contains at least one '.'. Strip the
         -- dot from the prefix and prepend it to the suffix (if we don't
         -- do this, the unique number will get added after the '.' and
         -- thus be part of the extension, which is wrong.)
         (rev_suffix, '.':rest) -> (reverse rest, '.':reverse rev_suffix)
         -- Otherwise, something is wrong, because (break (== '.')) should
         -- always return a pair with either the empty string or a string
         -- beginning with '.' as the second component.
         _                      -> error "bug in System.IO.openTempFile"

#ifndef __NHC__
    oflags1 = rw_flags .|. o_EXCL

    binary_flags
      | binary    = o_BINARY
      | otherwise = 0

    oflags = oflags1 .|. binary_flags
#endif

#ifdef __NHC__
    findTempName x = do h <- openFile filepath ReadWriteMode
                        return (filepath, h)
#else
    findTempName x = do
      fd <- withCString filepath $ \ f ->
              c_open f oflags 0o600
      if fd < 0
       then do
         errno <- getErrno
         if errno == eEXIST
           then findTempName (x+1)
           else ioError (errnoToIOError loc errno Nothing (Just tmp_dir))
       else do
         -- XXX We want to tell fdToHandle what the filepath is,
         -- as any exceptions etc will only be able to report the
         -- fd currently
         h <- fdToHandle fd `onException` c_close fd
         return (filepath, h)
#endif
      where
        filename        = prefix ++ show x ++ suffix
        filepath        = tmp_dir `combine` filename

        -- XXX bits copied from System.FilePath, since that's not available here
        combine a b
                  | null b = a
                  | null a = b
                  | last a == pathSeparator = a ++ b
                  | otherwise = a ++ [pathSeparator] ++ b

#if __HUGS__
        fdToHandle fd   = openFd (fromIntegral fd) False ReadWriteMode binary
#endif

-- XXX Should use filepath library
pathSeparator :: Char
#ifdef mingw32_HOST_OS
pathSeparator = '\\'
#else
pathSeparator = '/'
#endif

#ifndef __NHC__
-- XXX Copied from GHC.Handle
std_flags, output_flags, rw_flags :: CInt
std_flags    = o_NONBLOCK   .|. o_NOCTTY
output_flags = std_flags    .|. o_CREAT
rw_flags     = output_flags .|. o_RDWR
#endif

#ifdef __NHC__
foreign import ccall "getpid" c_getpid :: IO Int
#endif

-- $locking
-- Implementations should enforce as far as possible, at least locally to the
-- Haskell process, multiple-reader single-writer locking on files.
-- That is, /there may either be many handles on the same file which manage
-- input, or just one handle on the file which manages output/.  If any
-- open or semi-closed handle is managing a file for output, no new
-- handle can be allocated for that file.  If any open or semi-closed
-- handle is managing a file for input, new handles can only be allocated
-- if they do not manage output.  Whether two files are the same is
-- implementation-dependent, but they should normally be the same if they
-- have the same absolute path name and neither has been renamed, for
-- example.
--
-- /Warning/: the 'readFile' operation holds a semi-closed handle on
-- the file until the entire contents of the file have been consumed.
-- It follows that an attempt to write to a file (using 'writeFile', for
-- example) that was earlier opened by 'readFile' will usually result in
-- failure with 'System.IO.Error.isAlreadyInUseError'.

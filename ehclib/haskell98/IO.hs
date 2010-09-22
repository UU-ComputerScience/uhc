{-# LANGUAGE CPP #-}

module IO (
    Handle, 
#ifndef __UHC_TARGET_C__
    HandlePosn,
#endif
    IOMode(ReadMode,WriteMode,AppendMode,ReadWriteMode),
#ifndef __UHC_TARGET_C__
    BufferMode(NoBuffering,LineBuffering,BlockBuffering),
    SeekMode(AbsoluteSeek,RelativeSeek,SeekFromEnd),
#endif
    stdin, stdout, stderr, 
    openFile, hClose, 
#ifndef __UHC_TARGET_C__
    hFileSize, hIsEOF, isEOF,
    hSetBuffering, hGetBuffering,
#endif
    hFlush, 
#ifndef __UHC_TARGET_C__
    hGetPosn, hSetPosn, hSeek, 
    hWaitForInput, hReady, hGetChar, hGetLine, hLookAhead,
#endif
    hGetContents, 
    hPutChar, hPutStr, hPutStrLn,
#ifndef __UHC_TARGET_C__
    hPrint,
    hIsOpen, hIsClosed, hIsReadable, hIsWritable, hIsSeekable,
#endif
    isAlreadyExistsError, isDoesNotExistError, isAlreadyInUseError, 
    isFullError, isEOFError,
    isIllegalOperation, isPermissionError, isUserError, 
    ioeGetErrorString, ioeGetHandle, ioeGetFileName,
    try, bracket, bracket_,

    -- ...and what the Prelude exports
    IO, FilePath, IOError, ioError, userError, catch, interact,
    putChar, putStr, putStrLn, print, getChar, getLine, getContents,
    readFile, writeFile, appendFile,
#ifndef __UHC_TARGET_C__
    readIO, readLn
#endif
  ) where

#ifdef __UHC_TARGET_C__
import UHC.OldIO
#else
import System.IO
#endif
import System.IO.Error

-- | The 'bracket' function captures a common allocate, compute, deallocate
-- idiom in which the deallocation step must occur even in the case of an
-- error during computation. This is similar to try-catch-finally in Java.
--
-- This version handles only IO errors, as defined by Haskell 98.
-- The version of @bracket@ in "Control.Exception" handles all exceptions,
-- and should be used instead.

bracket        :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after m = do
        x  <- before
        rs <- try (m x)
        _ <- after x
        case rs of
           Right r -> return r
           Left  e -> ioError e

-- | A variant of 'bracket' where the middle computation doesn't want @x@.
--
-- This version handles only IO errors, as defined by Haskell 98.
-- The version of @bracket_@ in "Control.Exception" handles all exceptions,
-- and should be used instead.

bracket_        :: IO a -> (a -> IO b) -> IO c -> IO c
bracket_ before after m = do
         x  <- before
         rs <- try m
         _ <- after x
         case rs of
            Right r -> return r
            Left  e -> ioError e

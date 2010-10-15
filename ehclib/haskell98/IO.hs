{-# LANGUAGE CPP #-}

module IO (
    Handle, 
#if !( defined(__UHC_TARGET_C__) || defined(__UHC_TARGET_JSCRIPT__) )
    HandlePosn,
#endif
    IOMode(ReadMode,WriteMode,AppendMode,ReadWriteMode),
#if !( defined(__UHC_TARGET_C__) || defined(__UHC_TARGET_JSCRIPT__) )
    BufferMode(NoBuffering,LineBuffering,BlockBuffering),
    SeekMode(AbsoluteSeek,RelativeSeek,SeekFromEnd),
#endif
#if !defined(__UHC_TARGET_JSCRIPT__)
    stdin,
#endif
    stdout, stderr, 
#if !defined(__UHC_TARGET_JSCRIPT__)
    openFile,
#endif
    hClose, 
#if !( defined(__UHC_TARGET_C__) || defined(__UHC_TARGET_JSCRIPT__) )
    hFileSize, hIsEOF, isEOF,
    hSetBuffering, hGetBuffering,
#endif
    hFlush, 
#if !( defined(__UHC_TARGET_C__) || defined(__UHC_TARGET_JSCRIPT__) )
    hGetPosn, hSetPosn, hSeek, 
    hWaitForInput, hReady, hGetChar, hGetLine, hLookAhead,
#endif
#if !defined(__UHC_TARGET_JSCRIPT__)
    hGetContents, 
#endif
    hPutChar, hPutStr, hPutStrLn,
#if !( defined(__UHC_TARGET_C__) || defined(__UHC_TARGET_JSCRIPT__) )
    hPrint,
    hIsOpen, hIsClosed, hIsReadable, hIsWritable, hIsSeekable,
#endif
#if !( defined(__UHC_TARGET_JSCRIPT__) )
    isAlreadyExistsError, isDoesNotExistError, isAlreadyInUseError, 
    isFullError, isEOFError,
    isIllegalOperation, isPermissionError, isUserError, 
    ioeGetErrorString, ioeGetHandle, ioeGetFileName,
    try,
    bracket, bracket_,
#endif

    -- ...and what the Prelude exports
    IO,
#if !( defined(__UHC_TARGET_JSCRIPT__) )
    FilePath,
#endif
    IOError, ioError, userError, catch,
#if !defined(__UHC_TARGET_JSCRIPT__)
    interact,
#endif
    putChar, putStr, putStrLn, print,
#if !defined(__UHC_TARGET_JSCRIPT__)
    getChar, getLine, getContents,
    readFile, writeFile, appendFile,
#endif
#if !( defined(__UHC_TARGET_C__) || defined(__UHC_TARGET_JSCRIPT__) )
    readIO, readLn
#endif
  ) where

#if ( defined(__UHC_TARGET_C__) || defined(__UHC_TARGET_JSCRIPT__) )
import UHC.OldIO
#else
import System.IO
#endif
#if !( defined(__UHC_TARGET_JSCRIPT__) )
import System.IO.Error
#endif

-- | The 'bracket' function captures a common allocate, compute, deallocate
-- idiom in which the deallocation step must occur even in the case of an
-- error during computation. This is similar to try-catch-finally in Java.
--
-- This version handles only IO errors, as defined by Haskell 98.
-- The version of @bracket@ in "Control.Exception" handles all exceptions,
-- and should be used instead.

#if !( defined(__UHC_TARGET_JSCRIPT__) )
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
#endif

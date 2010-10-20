%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% First IO implementation, to become obsolete
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
{-# LANGUAGE NoImplicitPrelude, CPP #-}

module UHC.OldIO
  ( Handle
  , IOMode(..)
  
  -- IO functions
  , hClose, hPutChar, hPutStr, hPutStrLn, hFlush
  , putChar, putStr, putStrLn, print{- hPrint, -} 
  , stdout, stderr
#if !defined(__UHC_TARGET_JSCRIPT__)
  , stdin
  , openFile
  , hGetContents, hGetChar, hGetLine
  , getChar, getLine, getContents, interact
  , readFile, writeFile, appendFile
#endif
  )
  where

import UHC.Base
import UHC.IOBase
%%]

%%[99
----------------------------------------------------------------
-- Handle
----------------------------------------------------------------

-- See IOBase
%%]

%%[99
----------------------------------------------------------------
-- I/O primitives and their wrapping in the I/O monad
----------------------------------------------------------------

#if defined (__UHC_TARGET_C__) || defined (__UHC_TARGET_LLVM__)
foreign import prim primHClose        :: FHandle -> ()
foreign import prim primHFlush        :: FHandle -> ()
foreign import prim primHGetChar      :: FHandle -> Char
foreign import prim primHPutChar      :: FHandle -> Char -> ()
#else
foreign import prim primHClose        :: Handle -> ()
foreign import prim primHFlush        :: Handle -> ()
foreign import prim primHGetChar      :: Handle -> Char
foreign import prim primHPutChar      :: Handle -> Char -> ()
#endif

#if defined (__UHC_TARGET_C__) || defined (__UHC_TARGET_LLVM__)
foreign import prim primOpenFile      :: String -> IOMode -> FHandle
foreign import prim primStdin         :: FHandle
foreign import prim primStdout        :: FHandle
foreign import prim primStderr        :: FHandle
foreign import prim primHIsEOF        :: FHandle -> Bool
#else
#ifdef __UHC_TARGET_JAZY__
foreign import prim primStdin         :: Handle
foreign import prim primStdout        :: Handle
foreign import prim primStderr        :: Handle
#endif
foreign import prim primOpenFileOrStd :: String -> IOMode -> Maybe Int -> Handle
#endif


hClose       :: Handle -> IO ()
#if defined (__UHC_TARGET_C__) || defined (__UHC_TARGET_LLVM__)
hClose (OldHandle h)     =  ioFromPrim (\_ -> primHClose h)
#else
hClose h     =  ioFromPrim (\_ -> primHClose h)
#endif

hFlush       :: Handle -> IO ()
#if defined (__UHC_TARGET_C__) || defined (__UHC_TARGET_LLVM__)
hFlush (OldHandle h)     =  ioFromPrim (\_ -> primHFlush h)
#else
hFlush h     =  ioFromPrim (\_ -> primHFlush h)
#endif

hGetChar     :: Handle -> IO Char
#if defined (__UHC_TARGET_C__) || defined (__UHC_TARGET_LLVM__)
hGetChar (OldHandle h)   =  ioFromPrim (\_ -> primHGetChar h)
#else
hGetChar h   =  ioFromPrim (\_ -> primHGetChar h)
#endif

hPutChar     :: Handle -> Char -> IO ()
#if defined (__UHC_TARGET_C__) || defined (__UHC_TARGET_LLVM__)
hPutChar (OldHandle h) c =  ioFromPrim (\_ -> primHPutChar h c)
#else
hPutChar h c =  ioFromPrim (\_ -> primHPutChar h c)
#endif



#if defined (__UHC_TARGET_C__) || defined (__UHC_TARGET_LLVM__)


openFile     :: FilePath -> IOMode -> IO Handle
openFile f m =  ioFromPrim (\_ -> OldHandle (primOpenFile (forceString f) m))

stdin, stdout, stderr :: Handle
stdin  = OldHandle primStdin
stdout = OldHandle primStdout
stderr = OldHandle primStderr

hIsEOF       :: Handle -> IO Bool
hIsEOF (OldHandle h) =  ioFromPrim (\_ -> primHIsEOF h)

#elif defined(__UHC_TARGET_JSCRIPT__)
stdout, stderr :: Handle
stdout = OldHandle (JSHandle "stdout")
stderr = OldHandle (JSHandle "stderr")
#else

openFile     :: FilePath -> IOMode -> IO Handle
openFile f m =  ioFromPrim (\_ -> primOpenFileOrStd f m Nothing)

#ifdef __UHC_TARGET_JAZY__

stdin, stdout, stderr :: Handle
stdin  = primStdin
stdout = primStdout
stderr = primStderr

#else

stdin, stdout, stderr :: Handle
stdin  = primOpenFileOrStd "<stdin>"  ReadMode  (Just 0)
stdout = primOpenFileOrStd "<stdout>" WriteMode (Just 1)
stderr = primOpenFileOrStd "<stderr>" WriteMode (Just 2)

#endif

#endif


%%]

%%[99
----------------------------------------------------------------
-- I/O utilities
----------------------------------------------------------------

-- specializations for stdin, stdout

#if !defined(__UHC_TARGET_JSCRIPT__)
getChar     :: IO Char
getChar     = hGetChar stdin

getLine     :: IO String
getLine     = hGetLine stdin

getContents :: IO String
getContents = hGetContents stdin
#endif

putChar     :: Char -> IO ()
putChar     = hPutChar stdout

print       :: Show a => a -> IO ()
print       = hPrint stdout

putStr      :: String -> IO ()
putStr      = hPutStr   stdout

putStrLn    :: String -> IO ()
putStrLn    = hPutStrLn stdout

#if !defined(__UHC_TARGET_JSCRIPT__)
interact    :: (String -> String) -> IO ()
interact f  = getContents >>= (putStr . f)
#endif


-- combinations with newline and show

hPutStrLn     :: Handle -> String -> IO ()
hPutStrLn h s =  do { hPutStr h s
                    ; hPutChar h '\n'
                    }

hPrint        :: Show a => Handle -> a -> IO ()
hPrint h      =  hPutStrLn h . show

#if !defined(__UHC_TARGET_JSCRIPT__)
hGetLine :: Handle -> IO String
hGetLine h = do { c <- hGetChar h
                ; hGetLine2 c
                }
  where
   hGetLine2 '\n' = return ""
   hGetLine2 c    = do { cs <- hGetLine h
                       ; return (c:cs)
                       }
#if defined (__UHC_TARGET_C__) || defined (__UHC_TARGET_LLVM__)
   getRest        = hGetLine h
#else
   getRest        = do c <- catch (hGetChar h) 
                                  (\ ex -> if isEOFError ex 
                                           then return '\n' 
                                           else ioError ex
                                  )
                       hGetLine2 c
   isEOFError ex = ioe_type ex == EOF
#endif
#endif

#if !defined(__UHC_TARGET_JSCRIPT__)
-- combinations with Read
-- raises an exception instead of an error
readIO          :: Read a => String -> IO a
readIO s         = case [x | (x,t) <- reads s, ("","") <- lex t] of
                        [x] -> return x
                        []  -> ioError (userError "PreludeIO.readIO: no parse")
                        _   -> ioError (userError 
                                       "PreludeIO.readIO: ambiguous parse")

readLn          :: Read a => IO a
readLn           = do l <- getLine
                      r <- readIO l
                      return r
#endif


-- file open&process&close wrapped in one function

#if !defined(__UHC_TARGET_JSCRIPT__)
readFile        :: FilePath -> IO String
readFile name    = openFile name ReadMode >>= hGetContents

writeFile       :: FilePath -> String -> IO ()
writeFile        = writeFile2 WriteMode

appendFile      :: FilePath -> String -> IO ()
appendFile       = writeFile2 AppendMode

writeFile2      :: IOMode -> FilePath -> String -> IO ()
writeFile2 mode name s 
    = do h <- openFile name mode
#if defined (__UHC_TARGET_C__) || defined (__UHC_TARGET_LLVM__)
         hPutStr h s
#else
         catchException (hPutStr h s) (\e -> hClose h >> throw e)
#endif
         hClose h
#endif

%%]

%%[99
----------------------------------------------------------------
-- additional I/O primitives and their wrapping in the I/O monad
----------------------------------------------------------------

#if defined (__UHC_TARGET_C__) || defined (__UHC_TARGET_LLVM__)

#else
foreign import prim primHPutByteArray   :: Handle -> ByteArray -> ()
foreign import prim primHGetContents    :: Handle -> String
#endif



----------------------------------------------------------------
-- String I/O can be expressed in terms of basic primitives,
--  or for efficiency using additional primitives
----------------------------------------------------------------

#if !defined(__UHC_TARGET_JSCRIPT__)
hGetContents     :: Handle -> IO String
#endif
hPutStr          :: Handle -> String -> IO ()


#if defined (__UHC_TARGET_C__) || defined (__UHC_TARGET_LLVM__)

hGetContents h = do b <- hIsEOF h
                    if b
                     then return ""
                     else do { c <- hGetChar h
                             ; cs <- hGetContents h
                             ; return (c:cs) 
                             }

#else

hGetContents h   = ioFromPrim (\_ -> primHGetContents h)
                   
#endif


#if defined(__UHC_TARGET_C__) || defined(__UHC_TARGET_JSCRIPT__)

hPutStr h s = do if null s 
                  then return () 
                  else do { hPutChar h (head s)
                          ; hPutStr  h (tail s)
                          }

#else

foreign import prim primStringToByteArray :: String -> Int -> ByteArray

hPutStr h s = do let (shd,stl) = splitAt 1000 s
                 ioFromPrim (\_ -> primHPutByteArray h (primStringToByteArray shd 1000))
                 if null stl then return () else hPutStr h stl
                   
#endif



%%]

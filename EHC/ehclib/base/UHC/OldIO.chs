%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% First IO implementation, to become obsolete
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
module UHC.OldIO
  ( Handle
  , IOMode(..)
  
  -- IO functions
  , hClose, hGetContents, hGetChar, hGetLine, hPutChar, hPutStr, hPutStrLn, hFlush
  , stdin, stdout, stderr, openFile
  , putChar, putStr, putStrLn, print, {- hPrint, -} getChar, getLine, getContents, interact
  , readFile, writeFile, appendFile
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

#ifdef __UHC_TARGET_C__
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

#ifdef __UHC_TARGET_C__
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
#ifdef __UHC_TARGET_C__
hClose (OldHandle h)     =  ioFromPrim (\_ -> primHClose h)
#else
hClose h     =  ioFromPrim (\_ -> primHClose h)
#endif

hFlush       :: Handle -> IO ()
#ifdef __UHC_TARGET_C__
hFlush (OldHandle h)     =  ioFromPrim (\_ -> primHFlush h)
#else
hFlush h     =  ioFromPrim (\_ -> primHFlush h)
#endif

hGetChar     :: Handle -> IO Char
#ifdef __UHC_TARGET_C__
hGetChar (OldHandle h)   =  ioFromPrim (\_ -> primHGetChar h)
#else
hGetChar h   =  ioFromPrim (\_ -> primHGetChar h)
#endif

hPutChar     :: Handle -> Char -> IO ()
#ifdef __UHC_TARGET_C__
hPutChar (OldHandle h) c =  ioFromPrim (\_ -> primHPutChar h c)
#else
hPutChar h c =  ioFromPrim (\_ -> primHPutChar h c)
#endif


#ifdef __UHC_TARGET_C__

openFile     :: FilePath -> IOMode -> IO Handle
openFile f m =  ioFromPrim (\_ -> OldHandle (primOpenFile (forceString f) m))

stdin, stdout, stderr :: Handle
stdin  = OldHandle primStdin
stdout = OldHandle primStdout
stderr = OldHandle primStderr

hIsEOF       :: Handle -> IO Bool
hIsEOF (OldHandle h) =  ioFromPrim (\_ -> primHIsEOF h)

#else

openFile     :: FilePath -> IOMode -> IO Handle
openFile f m =  ioFromPrim (\_ -> primOpenFileOrStd f m Nothing)

stdin, stdout, stderr :: Handle
#ifdef __UHC_TARGET_JAZY__
stdin  = primStdin
stdout = primStdout
stderr = primStderr
#else
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

getChar     :: IO Char
getChar     = hGetChar stdin

getLine     :: IO String
getLine     = hGetLine stdin

getContents :: IO String
getContents = hGetContents stdin

putChar     :: Char -> IO ()
putChar     = hPutChar stdout

print       :: Show a => a -> IO ()
print       = hPrint stdout

putStr      :: String -> IO ()
putStr      = hPutStr   stdout

putStrLn    :: String -> IO ()
putStrLn    = hPutStrLn stdout

interact    :: (String -> String) -> IO ()
interact f  = getContents >>= (putStr . f)


-- combinations with newline and show

hPutStrLn     :: Handle -> String -> IO ()
hPutStrLn h s =  do { hPutStr h s
                    ; hPutChar h '\n'
                    }

hPrint        :: Show a => Handle -> a -> IO ()
hPrint h      =  hPutStrLn h . show

hGetLine :: Handle -> IO String
hGetLine h = do { c <- hGetChar h
                ; hGetLine2 c
                }
  where
   hGetLine2 '\n' = return ""
   hGetLine2 c    = do { cs <- hGetLine h
                       ; return (c:cs)
                       }
#ifdef __UHC_TARGET_C__
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



-- file open&process&close wrapped in one function

readFile        :: FilePath -> IO String
readFile name    = openFile name ReadMode >>= hGetContents

writeFile       :: FilePath -> String -> IO ()
writeFile        = writeFile2 WriteMode

appendFile      :: FilePath -> String -> IO ()
appendFile       = writeFile2 AppendMode

writeFile2      :: IOMode -> FilePath -> String -> IO ()
writeFile2 mode name s 
    = do h <- openFile name mode
#ifdef __UHC_TARGET_C__
         hPutStr h s
#else
         catchException (hPutStr h s) (\e -> hClose h >> throw e)
#endif
         hClose h


%%]

%%[99
----------------------------------------------------------------
-- additional I/O primitives and their wrapping in the I/O monad
----------------------------------------------------------------

#ifdef __UHC_TARGET_C__

#else
foreign import prim primHPutByteArray   :: Handle -> ByteArray -> ()
foreign import prim primHGetContents    :: Handle -> String
#endif



----------------------------------------------------------------
-- String I/O can be expressed in terms of basic primitives,
--  or for efficiency using additional primitives
----------------------------------------------------------------

hGetContents     :: Handle -> IO String
hPutStr          :: Handle -> String -> IO ()


#ifdef __UHC_TARGET_C__

hGetContents h = do b <- hIsEOF h
                    if b
                     then return ""
                     else do { c <- hGetChar h
                             ; cs <- hGetContents h
                             ; return (c:cs) 
                             }

hPutStr h s = do if null s 
                  then return () 
                  else do { hPutChar h (head s)
                          ; hPutStr  h (tail s)
                          }

#else

foreign import prim primStringToByteArray :: String -> Int -> ByteArray

hGetContents h   = ioFromPrim (\_ -> primHGetContents h)

hPutStr h s = do let (shd,stl) = splitAt 1000 s
                 ioFromPrim (\_ -> primHPutByteArray h (primStringToByteArray shd 1000))
                 if null stl then return () else hPutStr h stl
                   
#endif



%%]

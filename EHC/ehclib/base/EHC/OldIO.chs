%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% First IO implementation, to become obsolete
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
module EHC.OldIO
  ( Handle
  , IOMode(..)
  
  -- IO functions
  , hClose, hGetContents, hGetChar, hGetLine, hPutChar, hPutStr, hPutStrLn, hFlush
  , stdin, stdout, stderr, openFile
  , putChar, putStr, putStrLn, print, {- hPrint, -} getChar, getLine, getContents, interact
  , readFile, writeFile, appendFile
  )
  where

import EHC.Prelude
import EHC.IOBase
%%]

%%[99
----------------------------------------------------------------
-- Handle
----------------------------------------------------------------
%%]

%%[99
----------------------------------------------------------------
-- I/O primitives and their wrapping in the I/O monad
----------------------------------------------------------------

foreign import prim primHClose        :: Handle -> ()
foreign import prim primHFlush        :: Handle -> ()
foreign import prim primHGetChar      :: Handle -> Char
foreign import prim primHPutChar      :: Handle -> Char -> ()

#ifdef __EHC_FULL_PROGRAM_ANALYSIS__
foreign import prim primOpenFile      :: String -> IOMode -> Handle
foreign import prim primStdin         :: Handle
foreign import prim primStdout        :: Handle
foreign import prim primStderr        :: Handle
foreign import prim primHIsEOF        :: Handle -> Bool
#else
#ifdef __EHC_TARGET_JAZY__
foreign import prim primStdin         :: Handle
foreign import prim primStdout        :: Handle
foreign import prim primStderr        :: Handle
#endif
foreign import prim primOpenFileOrStd :: String -> IOMode -> Maybe Int -> Handle
#endif


hClose       :: Handle -> IO ()
hClose h     =  ioFromPrim (\_ -> primHClose h)

hFlush       :: Handle -> IO ()
hFlush h     =  ioFromPrim (\_ -> primHFlush h)

hGetChar     :: Handle -> IO Char
hGetChar h   =  ioFromPrim (\_ -> primHGetChar h)

hPutChar     :: Handle -> Char -> IO ()
hPutChar h c =  ioFromPrim (\_ -> primHPutChar h c)


#ifdef __EHC_FULL_PROGRAM_ANALYSIS__

openFile     :: FilePath -> IOMode -> IO Handle
openFile f m =  ioFromPrim (\_ -> primOpenFile (forceString f) m)

stdin, stdout, stderr :: Handle
stdin  = primStdin
stdout = primStdout
stderr = primStderr

hIsEOF       :: Handle -> IO Bool
hIsEOF h     =  ioFromPrim (\_ -> primHIsEOF h)

#else

openFile     :: FilePath -> IOMode -> IO Handle
openFile f m =  ioFromPrim (\_ -> primOpenFileOrStd f m Nothing)

stdin, stdout, stderr :: Handle
#ifdef __EHC_TARGET_JAZY__
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
#ifdef __EHC_FULL_PROGRAM_ANALYSIS__
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
#ifdef __EHC_FULL_PROGRAM_ANALYSIS__
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

#ifdef __EHC_FULL_PROGRAM_ANALYSIS__

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


#ifdef __EHC_FULL_PROGRAM_ANALYSIS__

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

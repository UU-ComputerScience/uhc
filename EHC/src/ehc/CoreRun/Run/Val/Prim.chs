%%[0 hs
-- {-# LANGUAGE MagicHash #-}
-- {-# OPTIONS_GHC -O2 #-}
%%]

%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CoreRun Val impl: primitive bindings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs module {%{EH}CoreRun.Run.Val.Prim}
%%]

%%[(8 corerun) hs import({%{EH}CoreRun.Prim}, {%{EH}CoreRun.Run.Val}, {%{EH}CoreRun.Run}, {%{EH}CoreRun})
%%]

%%[(8 corerun) hs import(UHC.Util.Pretty)
%%]

%%[(8 corerun) hs import(qualified Data.Vector as V, qualified Data.Vector.Mutable as MV)
%%]

%%[(8 corerun) hs import(qualified Data.ByteString.Char8 as BSC8)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Impl of primitives for Val runner
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(rvalPrim)
-- | Apply primitive to arguments
rvalPrim :: (RunSem RValCxt RValEnv RVal m a) => RunPrim -> RValV -> RValT m a
rvalPrim pr as = do 
    -- as' <- forM (V.toList as) rsemDeref
    let as' = V.toList as
    -- rsemTr $ "Prim:" >#< show pr >|< ppParensCommas as'
    case (pr, as') of
      -- Int arithmetic
      (RP_primAddInt, [RVal_Int i1, RVal_Int i2]) -> rsemPush $ RVal_Int $ i1 + i2
      (RP_primSubInt, [RVal_Int i1, RVal_Int i2]) -> rsemPush $ RVal_Int $ i1 - i2
      (RP_primMulInt, [RVal_Int i1, RVal_Int i2]) -> rsemPush $ RVal_Int $ i1 * i2
      (RP_primDivInt, [RVal_Int i1, RVal_Int i2]) -> rsemPush $ RVal_Int $ i1 `div` i2
      (RP_primEqInt, [RVal_Int i1, RVal_Int i2]) -> hsUnmarshall $ i1 == i2
      (RP_primLeInt, [RVal_Int i1, RVal_Int i2]) -> hsUnmarshall $ i1 <= i2
      (RP_primNeInt, [RVal_Int i1, RVal_Int i2]) -> hsUnmarshall $ i1 /= i2
      
      -- Exception handling
      (RP_primCatchException, [x, hdl]) -> rsemEvl x -- err $ "Not impl: RP_primCatchException" -- TBD
      
      -- MutVar
      {-
      (RP_primNewMutVar, [x, s]) -> (liftIO $ newIORef x) >>= \mv -> mkTuple [s, RHsV_MutVar mv]
      (RP_primReadMutVar, [RHsV_MutVar mv, s]) -> (liftIO $ readIORef mv) >>= \v -> mkTuple [s, v]
      (RP_primWriteMutVar, [RHsV_MutVar mv, v, s]) -> (liftIO $ writeIORef mv v) >> rsemPush s
      (RP_primSameMutVar, _) -> err $ "Not impl: RP_primSameMutVar" -- TBD
      -}
      
      -- Base
      (RP_primPackedStringToInteger, [RVal_PackedString x]) -> rsemPush $ RVal_Integer $ read $ BSC8.unpack x
      (RP_primPackedStringNull, [RVal_PackedString x]) -> hsUnmarshall $ BSC8.null x
      (RP_primPackedStringHead, [RVal_PackedString x]) -> rsemPush $ RVal_Char $ BSC8.head x
      (RP_primPackedStringTail, [RVal_PackedString x]) -> rsemPush $ RVal_PackedString $ BSC8.tail x
      (RP_primShowInteger, [RVal_Integer x]) -> hsUnmarshall $ show x
      
      -- Base: Bounded
      (RP_primMaxInt, _) -> rsemPush $ RVal_Int $ maxBound
      (RP_primMinInt, _) -> rsemPush $ RVal_Int $ minBound
  
      -- Prims: conversion
      (RP_primIntegerToInt32, [RVal_Integer x]) -> rsemPush $ RVal_Int32 $ fromIntegral x
      (RP_primIntToInteger  , [RVal_Int x]) -> rsemPush $ RVal_Integer $ fromIntegral x
      (RP_primIntegerToInt  , [RVal_Integer x]) -> rsemPush $ RVal_Int $ fromIntegral x
      
      -- IO
{-
		-- * The IO monad

	  -- IO                        -- instance MonadFix
	  | RP_fixIO                     -- :: (a -> IO a) -> IO a

		-- * Files and handles

	  -- FilePath                  -- :: String

	  -- Handle             -- abstract, instance of: Eq, Show.

		-- ** Standard handles

		-- | Three handles are allocated during program initialisation,
		-- and are initially open.

-}
	  -- | RP_stdin, RP_stdout, RP_stderr -- :: Handle
      (RP_stdin , _) -> rsemPush $ RHsV_Handle stdin
      (RP_stdout, _) -> rsemPush $ RHsV_Handle stdout
      (RP_stderr, _) -> rsemPush $ RHsV_Handle stderr
{-

		-- * Opening and closing files

		-- ** Opening files

	  | RP_withFile
	  | RP_openFile                  -- :: FilePath -> IOMode -> IO Handle
	  -- IOMode(ReadMode,WriteMode,AppendMode,ReadWriteMode),

-}
		-- ** Closing files

	  -- | RP_hClose                    -- :: Handle -> IO ()
      (RP_hClose, [RHsV_Handle h]) -> primIO (hClose h)

{-
		-- ** Special cases

		-- | These functions are also exported by the "Prelude".

	  | RP_readFile                  -- :: FilePath -> IO String
	  | RP_writeFile                 -- :: FilePath -> String -> IO ()
	  | RP_appendFile                -- :: FilePath -> String -> IO ()

		-- ** File locking

		-- $locking
-}

		-- * Operations on handles

		-- ** Determining and changing the size of a file

	  -- | RP_hFileSize                 -- :: Handle -> IO Integer
      (RP_hFileSize, [RHsV_Handle h]) -> primInputIO (hFileSize h)
	-- #ifdef __GLASGOW_HASKELL__
	  -- | RP_hSetFileSize              -- :: Handle -> Integer -> IO ()
      (RP_hSetFileSize, [RHsV_Handle h, RVal_Integer i]) -> primIO (hSetFileSize h i)
	-- #endif

		-- ** Detecting the end of input

	  -- | RP_hIsEOF                    -- :: Handle -> IO Bool
      (RP_hIsEOF, [RHsV_Handle h]) -> primInputIO (hIsEOF h)
{-
	  | RP_isEOF                     -- :: IO Bool
-}
		-- ** Buffering operations

{-
	  -- BufferMode(NoBuffering,LineBuffering,BlockBuffering),
	  -- | RP_hSetBuffering             -- :: Handle -> BufferMode -> IO ()
      (RP_hSetBuffering, [RHsV_Handle h, RVal_Int m]) -> primInputIO (hSetBuffering h m)
	  | RP_hGetBuffering             -- :: Handle -> IO BufferMode
-}
	  -- | RP_hFlush                    -- :: Handle -> IO ()
      (RP_hFlush, [RHsV_Handle h]) -> primIO (hFlush h)

{-
		-- ** Repositioning handles

	  | RP_hGetPosn                  -- :: Handle -> IO HandlePosn
	  | RP_hSetPosn                  -- :: HandlePosn -> IO ()
	  -- HandlePosn,                -- abstract, instance of: Eq, Show.

	  | RP_hSeek                     -- :: Handle -> SeekMode -> Integer -> IO ()
	  -- SeekMode(AbsoluteSeek,RelativeSeek,SeekFromEnd),
	-- #if !defined(__NHC__)
	  | RP_hTell                     -- :: Handle -> IO Integer
	-- #endif

		-- ** Handle properties

	  | RP_hIsOpen
	  | RP_hIsClosed        -- :: Handle -> IO Bool
	  | RP_hIsReadable
	  | RP_hIsWritable  -- :: Handle -> IO Bool
	  | RP_hIsSeekable               -- :: Handle -> IO Bool

		-- ** Terminal operations (not portable: GHC\/Hugs only)

	-- #if !defined(__NHC__)
	  | RP_hIsTerminalDevice          -- :: Handle -> IO Bool

	  | RP_hSetEcho                   -- :: Handle -> Bool -> IO ()
	  | RP_hGetEcho                   -- :: Handle -> IO Bool
	-- #endif

		-- ** Showing handle state (not portable: GHC only)

	-- #ifdef __GLASGOW_HASKELL__
-}
	  -- | RP_hShow                      -- :: Handle -> IO String
      (RP_hShow, [RHsV_Handle h]) -> primInputIO (hShow h)
{-
	-- #endif

		-- * Text input and output

		-- ** Text input
-}
	  -- | RP_hWaitForInput             -- :: Handle -> Int -> IO Bool
      (RP_hWaitForInput, [RHsV_Handle h, RVal_Int i]) -> primInputIO (hWaitForInput h i)
{-
	  | RP_hReady                    -- :: Handle -> IO Bool
-}
	  -- | RP_hGetChar                  -- :: Handle -> IO Char
      (RP_hGetChar, [RHsV_Handle h]) -> primInputIO (hGetChar h)
	  -- | RP_hGetLine                  -- :: Handle -> IO [Char]
      (RP_hGetLine, [RHsV_Handle h]) -> primInputIO (hGetLine h)
	  -- | RP_hLookAhead                -- :: Handle -> IO Char
      (RP_hLookAhead, [RHsV_Handle h]) -> primInputIO (hLookAhead h)
	  -- | RP_hGetContents              -- :: Handle -> IO [Char]
      (RP_hGetContents, [RHsV_Handle h]) -> primInputIO (hGetContents h)
{-
		-- ** Text output

-}
	  -- | RP_hPutChar                  -- :: Handle -> Char -> IO ()
      (RP_hPutChar, [RHsV_Handle h, x]) -> primOutputIO (hPutChar h) x
	  -- | RP_hPutStr                   -- :: Handle -> [Char] -> IO ()
      (RP_hPutStr, [RHsV_Handle h, x]) -> primOutputIO (hPutStr h) x
{-
	  | RP_hPutStrLn                 -- :: Handle -> [Char] -> IO ()
	  | RP_hPrint                    -- :: Show a => Handle -> a -> IO ()

		-- ** Special cases for standard input and output

		-- | These functions are also exported by the "Prelude".

	  | RP_interact                  -- :: (String -> String) -> IO ()
	  -- putChar                   -- :: Char   -> IO ()
	  -- putStr                    -- :: String -> IO () 
	  -- putStrLn                  -- :: String -> IO ()
	  -- print                     -- :: Show a => a -> IO ()
	  -- getChar                   -- :: IO Char
	  -- getLine                   -- :: IO String
	  -- getContents               -- :: IO String
	  | RP_readIO                    -- :: Read a => String -> IO a
	  | RP_readLn                    -- :: Read a => IO a

		-- * Binary input and output
	  | RP_withBinaryFile
	  | RP_openBinaryFile            -- :: FilePath -> IOMode -> IO Handle
-}
	  -- | RP_hSetBinaryMode            -- :: Handle -> Bool -> IO ()
      (RP_hSetBinaryMode, [RHsV_Handle h, b]) -> primOutputIO (hSetBinaryMode h) b
	  -- | RP_hPutBuf                   -- :: Handle -> Ptr a -> Int -> IO ()
      (RP_hPutBuf, [RHsV_Handle h, RHsV_Addr a, RVal_Int i]) -> primIO (hPutBuf h (Ptr a) i)
	  -- | RP_hGetBuf                   -- :: Handle -> Ptr a -> Int -> IO Int
      (RP_hGetBuf, [RHsV_Handle h, RHsV_Addr a, RVal_Int i]) -> primInputIO (hGetBuf h (Ptr a) i)
{-
	-- #if !defined(__NHC__) && !defined(__HUGS__)
-}
	  -- | RP_hPutBufNonBlocking        -- :: Handle -> Ptr a -> Int -> IO Int
      (RP_hPutBufNonBlocking, [RHsV_Handle h, RHsV_Addr a, RVal_Int i]) -> primIO (hPutBufNonBlocking h (Ptr a) i)
	  -- | RP_hGetBufNonBlocking        -- :: Handle -> Ptr a -> Int -> IO Int
      (RP_hGetBufNonBlocking, [RHsV_Handle h, RHsV_Addr a, RVal_Int i]) -> primInputIO (hGetBufNonBlocking h (Ptr a) i)
{-
	-- #endif
		-- * Temporary files

	  | RP_openTempFile
	  | RP_openBinaryTempFile
-}
        -- * Additional ones
      -- | RP_primShowHandle               -- :: Handle -> String
      (RP_primShowHandle, [RHsV_Handle h]) -> hsUnmarshall $ show h
      -- | RP_primEqHandle                 -- :: Handle -> Handle -> Bool
      (RP_primEqHandle, [RHsV_Handle h1, RHsV_Handle h2]) -> hsUnmarshall $ h1 == h2

      (pr, _) -> err $ "CoreRun.Run.Val.Prim:" >#< show pr
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs
-- | Voidify IO on RVal level, i.e. make IO ()
-- rvalVoid :: (RunSem RValCxt RValEnv RVal m RVal) => RValT m a -> RValT m RVal
rvalVoid :: (RunSem RValCxt RValEnv RVal m a) => RValT m b -> RValT m a
rvalVoid m = m >> mkUnit
{-# INLINE rvalVoid #-}

-- | IO, no result
primIO :: (RunSem RValCxt RValEnv RVal m a) => IO x -> RValT m a
primIO io = rvalVoid $ liftIO $ io
-- {-# INLINE primIO #-}

-- | Input-like IO
primInputIO :: (RunSem RValCxt RValEnv RVal m a, HSMarshall x) => IO x -> RValT m a
primInputIO io = (liftIO $ io) >>= hsUnmarshall
-- {-# INLINE primInputIO #-}

-- | Output-like IO
primOutputIO :: (RunSem RValCxt RValEnv RVal m a, HSMarshall x) => (x -> IO ()) -> RVal -> RValT m a
primOutputIO io x = rvalVoid $ hsMarshall rvalRetEvl x >>= (liftIO . io)
-- {-# INLINE primOutputIO #-}
%%]



%%[0 hs
-- {-# LANGUAGE DeriveGeneric #-}
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
%%[(8 corerun) hs import(Data.Bits, Data.Maybe)
%%]

%%[(8 corerun) hs import(GHC.Generics)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Impl of primitives for Val runner
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(rvalPrim)
-- | Apply primitive to arguments
rvalPrim :: (RunSem RValCxt RValEnv RVal m a) => RunPrim -> RValV -> RValT m a
rvalPrim pr as = do 
    as' <- forM (V.toList as) $ \a -> rsemDeref a >>= rsemPop
    -- let as' = V.toList as
    -- rsemTr $ "Prim:" >#< show pr >|< ppParensCommas as'
    case (pr, as') of
      -- Unsafe stuff
      (RP_primUnsafeId, [x]) -> rsemPush x

      -- Char
      (RP_primEqChar		, [RVal_Char i1, RVal_Char i2]	) -> hsUnmarshall $ i1 == i2
      (RP_primCmpChar		, [RVal_Char i1, RVal_Char i2]	) -> hsUnmarshall $ i1 `compare` i2
  
      (RP_primCharToInt  	, [RVal_Char x]					) -> rsemPush $ RVal_Int $ fromEnum x
      (RP_primIntToChar  	, [RVal_Int x]					) -> rsemPush $ RVal_Char $ toEnum x
      
      -- Int
      (RP_primAddInt		, [RVal_Int i1, RVal_Int i2]) -> rsemPush $ RVal_Int $ i1 + i2
      (RP_primSubInt		, [RVal_Int i1, RVal_Int i2]) -> rsemPush $ RVal_Int $ i1 - i2
      (RP_primMulInt		, [RVal_Int i1, RVal_Int i2]) -> rsemPush $ RVal_Int $ i1 * i2
      (RP_primDivInt		, [RVal_Int i1, RVal_Int i2]) -> rsemPush $ RVal_Int $ i1 `div` i2
      (RP_primQuotInt		, [RVal_Int i1, RVal_Int i2]) -> rsemPush $ RVal_Int $ i1 `quot` i2
      (RP_primRemInt		, [RVal_Int i1, RVal_Int i2]) -> rsemPush $ RVal_Int $ i1 `rem` i2
      (RP_primModInt		, [RVal_Int i1, RVal_Int i2]) -> rsemPush $ RVal_Int $ i1 `mod` i2
      (RP_primNegInt		, [RVal_Int i1]				) -> rsemPush $ RVal_Int (-i1)

      (RP_primDivModInt		, [RVal_Int i1, RVal_Int i2]) -> hsUnmarshall $ i1 `divMod` i2
      (RP_primQuotRemInt	, [RVal_Int i1, RVal_Int i2]) -> hsUnmarshall $ i1 `quotRem` i2
      
      (RP_primMinInt		, []						) -> rsemPush $ RVal_Int minBound
      (RP_primMaxInt		, []						) -> rsemPush $ RVal_Int maxBound
      
      (RP_primEqInt			, [RVal_Int i1, RVal_Int i2]) -> hsUnmarshall $ i1 == i2
      (RP_primNeInt			, [RVal_Int i1, RVal_Int i2]) -> hsUnmarshall $ i1 /= i2
      
      (RP_primCmpInt		, [RVal_Int i1, RVal_Int i2]) -> hsUnmarshall $ i1 `compare` i2
      (RP_primLeInt			, [RVal_Int i1, RVal_Int i2]) -> hsUnmarshall $ i1 <= i2
      (RP_primLtInt			, [RVal_Int i1, RVal_Int i2]) -> hsUnmarshall $ i1 <  i2
      (RP_primGeInt			, [RVal_Int i1, RVal_Int i2]) -> hsUnmarshall $ i1 >= i2
      (RP_primGtInt			, [RVal_Int i1, RVal_Int i2]) -> hsUnmarshall $ i1 >  i2
      
      (RP_primAndInt		, [RVal_Int i1, RVal_Int i2]) -> rsemPush $ RVal_Int $ i1 .&. i2
      (RP_primOrInt			, [RVal_Int i1, RVal_Int i2]) -> rsemPush $ RVal_Int $ i1 .|. i2
      (RP_primXorInt		, [RVal_Int i1, RVal_Int i2]) -> rsemPush $ RVal_Int $ i1 `xor` i2
      (RP_primComplementInt	, [RVal_Int i1]				) -> rsemPush $ RVal_Int $ complement i1
      (RP_primShiftLeftInt	, [RVal_Int i1, RVal_Int i2]) -> rsemPush $ RVal_Int $ i1 `shiftL` i2
      (RP_primShiftRightInt	, [RVal_Int i1, RVal_Int i2]) -> rsemPush $ RVal_Int $ i1 `shiftR` i2
      (RP_primRotateLeftInt	, [RVal_Int i1, RVal_Int i2]) -> rsemPush $ RVal_Int $ i1 `rotateL` i2
      (RP_primRotateRightInt, [RVal_Int i1, RVal_Int i2]) -> rsemPush $ RVal_Int $ i1 `rotateR` i2
      (RP_primBitSize		, [RVal_Int i1]				) -> rsemPush $ RVal_Int $ fromJust $ bitSizeMaybe i1
      (RP_primBitSizeMaybe	, [RVal_Int i1]				) -> hsUnmarshall $ bitSizeMaybe i1
      (RP_primPopCount		, [RVal_Int i1]				) -> rsemPush $ RVal_Int $ popCount i1
      (RP_primBit			, [RVal_Int i1]				) -> rsemPush $ RVal_Int $ bit i1

      (RP_primIntToInteger  , [RVal_Int x]				) -> rsemPush $ RVal_Integer $ fromIntegral x
      (RP_primIntegerToInt  , [RVal_Integer x]			) -> rsemPush $ RVal_Int $ fromIntegral x

      -- Integer
      (RP_primAddInteger		, [RVal_Integer i1, RVal_Integer i2]) -> rsemPush $ RVal_Integer $ i1 + i2
      (RP_primSubInteger		, [RVal_Integer i1, RVal_Integer i2]) -> rsemPush $ RVal_Integer $ i1 - i2
      (RP_primMulInteger		, [RVal_Integer i1, RVal_Integer i2]) -> rsemPush $ RVal_Integer $ i1 * i2
      (RP_primDivInteger		, [RVal_Integer i1, RVal_Integer i2]) -> rsemPush $ RVal_Integer $ i1 `div` i2
      (RP_primQuotInteger		, [RVal_Integer i1, RVal_Integer i2]) -> rsemPush $ RVal_Integer $ i1 `quot` i2
      (RP_primRemInteger		, [RVal_Integer i1, RVal_Integer i2]) -> rsemPush $ RVal_Integer $ i1 `rem` i2
      (RP_primModInteger		, [RVal_Integer i1, RVal_Integer i2]) -> rsemPush $ RVal_Integer $ i1 `mod` i2
      (RP_primNegInteger		, [RVal_Integer i1]					) -> rsemPush $ RVal_Integer (-i1)

      (RP_primDivModInteger		, [RVal_Integer i1, RVal_Integer i2]) -> hsUnmarshall $ i1 `divMod` i2
      (RP_primQuotRemInteger	, [RVal_Integer i1, RVal_Integer i2]) -> hsUnmarshall $ i1 `quotRem` i2
            
      (RP_primEqInteger			, [RVal_Integer i1, RVal_Integer i2]) -> hsUnmarshall $ i1 == i2
      (RP_primNeInteger			, [RVal_Integer i1, RVal_Integer i2]) -> hsUnmarshall $ i1 /= i2
      
      (RP_primCmpInteger		, [RVal_Integer i1, RVal_Integer i2]) -> hsUnmarshall $ i1 `compare` i2
      (RP_primLeInteger			, [RVal_Integer i1, RVal_Integer i2]) -> hsUnmarshall $ i1 <= i2
      (RP_primLtInteger			, [RVal_Integer i1, RVal_Integer i2]) -> hsUnmarshall $ i1 <  i2
      (RP_primGeInteger			, [RVal_Integer i1, RVal_Integer i2]) -> hsUnmarshall $ i1 >= i2
      (RP_primGtInteger			, [RVal_Integer i1, RVal_Integer i2]) -> hsUnmarshall $ i1 >  i2
      
      -- Exception handling
      (RP_primCatchException, [x, hdl]) -> rsemEvl x -- err $ "Not impl: RP_primCatchException" -- TBD
            
      -- Base
      (RP_primPackedStringToInteger, [RVal_PackedString x]) -> rsemPush $ RVal_Integer $ read $ BSC8.unpack x
      (RP_primPackedStringNull, [RVal_PackedString x]) -> hsUnmarshall $ BSC8.null x
      (RP_primPackedStringHead, [RVal_PackedString x]) -> rsemPush $ RVal_Char $ BSC8.head x
      (RP_primPackedStringTail, [RVal_PackedString x]) -> rsemPush $ RVal_PackedString $ BSC8.tail x
      (RP_primShowInteger, [RVal_Integer x]) -> hsUnmarshall $ show x
      
      -- Prims: conversion
      (RP_primIntegerToInt32, [RVal_Integer x]) -> rsemPush $ RVal_Int32 $ fromIntegral x
      
      -- IO
{-
		--- * The IO monad

-}

		--- * Files and handles

	  -- FilePath                  -- :: String

	  -- Handle             -- abstract, instance of: Eq, Show.

		--- ** Standard handles

		--- | Three handles are allocated during program initialisation,
		-- and are initially open.

	  --- | RP_stdin, RP_stdout, RP_stderr -- :: Handle
      (RP_stdin , _) -> rsemPush $ RHsV_Handle stdin
      (RP_stdout, _) -> rsemPush $ RHsV_Handle stdout
      (RP_stderr, _) -> rsemPush $ RHsV_Handle stderr

      --- | RP_openFile                  -- :: FilePath -> IOMode -> IO Handle
      (RP_openFile, [fp, md]) -> primIO_2ArgVal1ResVal openFile fp md
      --- | RP_openBinaryFile                  -- :: FilePath -> IOMode -> IO Handle
      (RP_openBinaryFile, [fp, md]) -> primIO_2ArgVal1ResVal openBinaryFile fp md

	  -- IOMode(ReadMode,WriteMode,AppendMode,ReadWriteMode),

		--- ** Closing files

	  --- | RP_hClose                    -- :: Handle -> IO ()
      (RP_hClose, [RHsV_Handle h]) -> primIO (hClose h)

		--- * Operations on handles

		--- ** Determining and changing the size of a file

	  --- | RP_hFileSize                 -- :: Handle -> IO Integer
      (RP_hFileSize, [RHsV_Handle h]) -> primIO_0Arg1ResVal (hFileSize h)
	--- #ifdef __GLASGOW_HASKELL__
	  --- | RP_hSetFileSize              -- :: Handle -> Integer -> IO ()
      (RP_hSetFileSize, [RHsV_Handle h, RVal_Integer i]) -> primIO (hSetFileSize h i)
	--- #endif

		--- ** Detecting the end of input

	  --- | RP_hIsEOF                    -- :: Handle -> IO Bool
      (RP_hIsEOF, [h]) -> primIO_1ArgVal1ResVal hIsEOF h

		--- ** Buffering operations

	  -- BufferMode(NoBuffering,LineBuffering,BlockBuffering),
	  --- | RP_hSetBuffering             -- :: Handle -> BufferMode -> IO ()
      (RP_hSetBuffering, [h, m]) -> primIO_2ArgVal0Res hSetBuffering h m
	  --- | RP_hGetBuffering             -- :: Handle -> IO BufferMode
      (RP_hGetBuffering, [h]) -> primIO_1ArgVal1ResVal hGetBuffering h
	  --- | RP_hFlush                    -- :: Handle -> IO ()
      (RP_hFlush, [RHsV_Handle h]) -> primIO (hFlush h)

		--- ** Repositioning handles

	  -- HandlePosn,                -- abstract, instance of: Eq, Show.

	  -- SeekMode(AbsoluteSeek,RelativeSeek,SeekFromEnd),
	  --- | RP_hSeek                     -- :: Handle -> SeekMode -> Integer -> IO ()
      (RP_hSeek, [h, m, i]) -> primIO_3ArgVal0Res hSeek h m i
	-- #if !defined(__NHC__)
	  --- | RP_hTell                     -- :: Handle -> IO Integer
      (RP_hTell, [h]) -> primIO_1ArgVal1ResVal hTell h
	-- #endif

		--- ** Handle properties

	  --- | RP_hIsOpen
      (RP_hIsOpen, [h]) -> primIO_1ArgVal1ResVal hIsOpen h

	  --- | RP_hIsClosed        -- :: Handle -> IO Bool
      (RP_hIsClosed, [h]) -> primIO_1ArgVal1ResVal hIsClosed h

	  --- | RP_hIsReadable
      (RP_hIsReadable, [h]) -> primIO_1ArgVal1ResVal hIsReadable h

	  --- | RP_hIsWritable  -- :: Handle -> IO Bool
      (RP_hIsWritable, [h]) -> primIO_1ArgVal1ResVal hIsWritable h

	  --- | RP_hIsSeekable               -- :: Handle -> IO Bool
      (RP_hIsSeekable, [h]) -> primIO_1ArgVal1ResVal hIsSeekable h


{-
		--- ** Terminal operations (not portable: GHC\/Hugs only)

	--- #if !defined(__NHC__)
	  | RP_hIsTerminalDevice          -- :: Handle -> IO Bool

	  | RP_hSetEcho                   -- :: Handle -> Bool -> IO ()
	  | RP_hGetEcho                   -- :: Handle -> IO Bool
	--- #endif

		--- ** Showing handle state (not portable: GHC only)

	-- #ifdef __GLASGOW_HASKELL__
-}
	  --- | RP_hShow                      -- :: Handle -> IO String
      (RP_hShow, [RHsV_Handle h]) -> primIO_0Arg1ResVal (hShow h)
{-
	--- #endif

		--- * Text input and output

		--- ** Text input
-}
	  --- | RP_hWaitForInput             -- :: Handle -> Int -> IO Bool
      (RP_hWaitForInput, [RHsV_Handle h, RVal_Int i]) -> primIO_0Arg1ResVal (hWaitForInput h i)
	  --- | RP_hGetChar                  -- :: Handle -> IO Char
      (RP_hGetChar, [RHsV_Handle h]) -> primIO_0Arg1ResVal (hGetChar h)
	  --- | RP_hGetLine                  -- :: Handle -> IO [Char]
      (RP_hGetLine, [RHsV_Handle h]) -> primIO_0Arg1ResVal (hGetLine h)
	  --- | RP_hLookAhead                -- :: Handle -> IO Char
      (RP_hLookAhead, [RHsV_Handle h]) -> primIO_0Arg1ResVal (hLookAhead h)
	  --- | RP_hGetContents              -- :: Handle -> IO [Char]
      (RP_hGetContents, [RHsV_Handle h]) -> primIO_0Arg1ResVal (hGetContents h)
{-
		--- ** Text output

-}
	  --- | RP_hPutChar                  -- :: Handle -> Char -> IO ()
      (RP_hPutChar, [RHsV_Handle h, x]) -> primIO_1ArgVal0Res (hPutChar h) x
	  --- | RP_hPutStr                   -- :: Handle -> [Char] -> IO ()
      (RP_hPutStr, [RHsV_Handle h, x]) -> primIO_1ArgVal0Res (hPutStr h) x

		--- * Binary input and output
	  --- | RP_hSetBinaryMode            -- :: Handle -> Bool -> IO ()
      (RP_hSetBinaryMode, [RHsV_Handle h, b]) -> primIO_1ArgVal0Res (hSetBinaryMode h) b
	  --- | RP_hPutBuf                   -- :: Handle -> Ptr a -> Int -> IO ()
      (RP_hPutBuf, [RHsV_Handle h, RHsV_Addr a, RVal_Int i]) -> primIO (hPutBuf h (Ptr a) i)
	  --- | RP_hGetBuf                   -- :: Handle -> Ptr a -> Int -> IO Int
      (RP_hGetBuf, [RHsV_Handle h, RHsV_Addr a, RVal_Int i]) -> primIO_0Arg1ResVal (hGetBuf h (Ptr a) i)
{-
	--- #if !defined(__NHC__) && !defined(__HUGS__)
-}
	  --- | RP_hPutBufNonBlocking        -- :: Handle -> Ptr a -> Int -> IO Int
      (RP_hPutBufNonBlocking, [RHsV_Handle h, RHsV_Addr a, RVal_Int i]) -> primIO (hPutBufNonBlocking h (Ptr a) i)
	  --- | RP_hGetBufNonBlocking        -- :: Handle -> Ptr a -> Int -> IO Int
      (RP_hGetBufNonBlocking, [RHsV_Handle h, RHsV_Addr a, RVal_Int i]) -> primIO_0Arg1ResVal (hGetBufNonBlocking h (Ptr a) i)
{-
	--- #endif
-}
		--- * Temporary files

      --- | RP_openTempFile                  -- :: FilePath -> String -> IO (FilePath, Handle)
      (RP_openTempFile, [fp, tmpl]) -> primIO_2ArgVal1ResVal openTempFile fp tmpl
      --- | RP_openBinaryTempFile                  -- :: FilePath -> String -> IO (FilePath, Handle)
      (RP_openBinaryTempFile, [fp, tmpl]) -> primIO_2ArgVal1ResVal openBinaryTempFile fp tmpl

        --- * Additional ones
      --- | RP_primShowHandle               -- :: Handle -> String
      (RP_primShowHandle, [RHsV_Handle h]) -> hsUnmarshall $ show h
      --- | RP_primEqHandle                 -- :: Handle -> Handle -> Bool
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

-- | IO, taking 0 arg yielding a result
primIO_0Arg1ResVal :: (RunSem RValCxt RValEnv RVal m a, HSMarshall x) => IO x -> RValT m a
primIO_0Arg1ResVal io = (liftIO $ io) >>= hsUnmarshall
-- {-# INLINE primIO_0Arg1ResVal #-}

-- | IO, taking 1 (still to marshall) arg yielding ()
primIO_1ArgVal0Res :: (RunSem RValCxt RValEnv RVal m a, HSMarshall x) => (x -> IO ()) -> RVal -> RValT m a
primIO_1ArgVal0Res io x = do
  x' <- hsMarshall rvalPrimargEvl x
  rvalVoid $ liftIO $ io x'
-- {-# INLINE primIO_1ArgVal0Res #-}

-- | IO, taking 2 (still to marshall) arg yielding ()
primIO_2ArgVal0Res :: (RunSem RValCxt RValEnv RVal m a, HSMarshall x, HSMarshall y) => (x -> y -> IO ()) -> RVal -> RVal -> RValT m a
primIO_2ArgVal0Res io x y = do
  x' <- hsMarshall rvalPrimargEvl x
  y' <- hsMarshall rvalPrimargEvl y
  rvalVoid $ liftIO $ io x' y'
-- {-# INLINE primIO_2ArgVal0Res #-}

-- | IO, taking 3 (still to marshall) arg yielding ()
primIO_3ArgVal0Res :: (RunSem RValCxt RValEnv RVal m a, HSMarshall x, HSMarshall y, HSMarshall z) => (x -> y -> z -> IO ()) -> RVal -> RVal -> RVal -> RValT m a
primIO_3ArgVal0Res io x y z = do
  x' <- hsMarshall rvalPrimargEvl x
  y' <- hsMarshall rvalPrimargEvl y
  z' <- hsMarshall rvalPrimargEvl z
  rvalVoid $ liftIO $ io x' y' z'
-- {-# INLINE primIO_3ArgVal0Res #-}

-- | IO, taking 1 (still to marshall) arg yielding a result
primIO_1ArgVal1ResVal :: (RunSem RValCxt RValEnv RVal m a, HSMarshall x, HSMarshall res) => (x -> IO res) -> RVal -> RValT m a
primIO_1ArgVal1ResVal io x = hsMarshall rvalPrimargEvl x >>= (liftIO . io) >>= hsUnmarshall
-- {-# INLINE primIO_1ArgVal0Res #-}

-- | IO, taking 1 (still to marshall) arg yielding a result
primIO_2ArgVal1ResVal :: (RunSem RValCxt RValEnv RVal m a, HSMarshall x, HSMarshall y, HSMarshall res) => (x -> y -> IO res) -> RVal -> RVal -> RValT m a
primIO_2ArgVal1ResVal io x y = do
  x' <- hsMarshall rvalPrimargEvl x
  y' <- hsMarshall rvalPrimargEvl y
  liftIO (io x' y') >>= hsUnmarshall
-- {-# INLINE primIO_2Arg0Res #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Required generic instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun)
deriving instance Generic IOMode
instance HSMarshall IOMode

deriving instance Generic BufferMode
instance HSMarshall BufferMode

deriving instance Generic SeekMode
instance HSMarshall SeekMode

instance HSMarshall a => HSMarshall (Maybe a)

instance HSMarshall Ordering

instance (HSMarshall a, HSMarshall b) => HSMarshall (a,b)
instance (HSMarshall a, HSMarshall b, HSMarshall c) => HSMarshall (a,b,c)
instance (HSMarshall a, HSMarshall b, HSMarshall c, HSMarshall d) => HSMarshall (a,b,c,d)
instance (HSMarshall a, HSMarshall b, HSMarshall c, HSMarshall d, HSMarshall e) => HSMarshall (a,b,c,d,e)
%%]



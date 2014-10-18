%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Run Core infrastructure: builtin primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs module {%{EH}CoreRun.Prim}
%%]

%%[(8888 corerun) hs import({%{EH}Base.HsName.Builtin},{%{EH}Base.Common},{%{EH}Opts},{%{EH}Ty},{%{EH}Error},{%{EH}Gam},{%{EH}Gam.DataGam})
%%]

%%[(8888 corerun) hs import({%{EH}CoreRun})
%%]

%%[(8 corerun) hs import(qualified Data.Map as Map)
%%]

%%[(8888 corerun) hs import(Data.Maybe, Data.Monoid)
%%]

%%[(8888 corerun) hs import(Control.Monad, Control.Monad.Error)
%%]

%%[(8888 corerun) hs import(Control.Monad.RWS.Strict)
%%]
%%[(8888 corerun) hs import(Control.Monad.State.Strict)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Enumeration of all primitives which should be taken care of by an implementation of running CoreRun
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(RunPrim(..))
-- | Primitives.
-- Assumption: name of primitive starts with 3 choosable char + exact name of prim
data RunPrim
  = 
    -- Int arithmetic
    RP_primAddInt
  | RP_primSubInt
  | RP_primMulInt
  | RP_primDivInt
  | RP_primEqInt
  
    -- UHC.IOBase: Exception handling
  | RP_primCatchException
  
    -- UHC.MutVar
  | RP_primNewMutVar
  | RP_primReadMutVar
  | RP_primWriteMutVar
  | RP_primSameMutVar
  
    -- UHC.Base
  | RP_primPackedStringToInteger
  | RP_primPackedStringNull -- :: PackedString -> Bool
  | RP_primPackedStringHead -- :: PackedString -> Char
  | RP_primPackedStringTail -- :: PackedString -> PackedString
  
    -- UHC.Prims
  | RP_primIntegerToInt32
  
    -- System.IO
    -- * The IO monad

  -- IO                        -- instance MonadFix
  | RP_fixIO                     -- :: (a -> IO a) -> IO a

    -- * Files and handles

  -- FilePath                  -- :: String

  -- Handle             -- abstract, instance of: Eq, Show.

    -- ** Standard handles

    -- | Three handles are allocated during program initialisation,
    -- and are initially open.

  | RP_stdin
  | RP_stdout
  | RP_stderr   -- :: Handle

    -- * Opening and closing files

    -- ** Opening files

  | RP_withFile
  | RP_openFile                  -- :: FilePath -> IOMode -> IO Handle
  -- IOMode(ReadMode,WriteMode,AppendMode,ReadWriteMode),

    -- ** Closing files

  | RP_hClose                    -- :: Handle -> IO ()

    -- ** Special cases

    -- | These functions are also exported by the "Prelude".

  | RP_readFile                  -- :: FilePath -> IO String
  | RP_writeFile                 -- :: FilePath -> String -> IO ()
  | RP_appendFile                -- :: FilePath -> String -> IO ()

    -- ** File locking

    -- $locking

    -- * Operations on handles

    -- ** Determining and changing the size of a file

  | RP_hFileSize                 -- :: Handle -> IO Integer
-- #ifdef __GLASGOW_HASKELL__
  | RP_hSetFileSize              -- :: Handle -> Integer -> IO ()
-- #endif

    -- ** Detecting the end of input

  | RP_hIsEOF                    -- :: Handle -> IO Bool
  | RP_isEOF                     -- :: IO Bool

    -- ** Buffering operations

  -- BufferMode(NoBuffering,LineBuffering,BlockBuffering),
  | RP_hSetBuffering             -- :: Handle -> BufferMode -> IO ()
  | RP_hGetBuffering             -- :: Handle -> IO BufferMode
  | RP_hFlush                    -- :: Handle -> IO ()

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
  | RP_hShow                      -- :: Handle -> IO String
-- #endif

    -- * Text input and output

    -- ** Text input
  | RP_hWaitForInput             -- :: Handle -> Int -> IO Bool
  | RP_hReady                    -- :: Handle -> IO Bool
  | RP_hGetChar                  -- :: Handle -> IO Char
  | RP_hGetLine                  -- :: Handle -> IO [Char]
  | RP_hLookAhead                -- :: Handle -> IO Char
  | RP_hGetContents              -- :: Handle -> IO [Char]
    -- ** Text output

  | RP_hPutChar                  -- :: Handle -> Char -> IO ()
  | RP_hPutStr                   -- :: Handle -> [Char] -> IO ()
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
  | RP_hSetBinaryMode            -- :: Handle -> Bool -> IO ()
  | RP_hPutBuf                   -- :: Handle -> Ptr a -> Int -> IO ()
  | RP_hGetBuf                   -- :: Handle -> Ptr a -> Int -> IO Int
-- #if !defined(__NHC__) && !defined(__HUGS__)
  | RP_hPutBufNonBlocking        -- :: Handle -> Ptr a -> Int -> IO Int
  | RP_hGetBufNonBlocking        -- :: Handle -> Ptr a -> Int -> IO Int
-- #endif
    -- * Temporary files

  | RP_openTempFile
  | RP_openBinaryTempFile


  deriving (Show, Eq, Ord, Enum, Bounded)
%%]

%%[(8 corerun) hs export(showRunPrim)
-- | Show prim without initial 3 chars
showRunPrim :: RunPrim -> String
showRunPrim p = drop 3 $ show p
%%]

%%[(8 corerun) hs export(allRunPrimMp)
allRunPrimMp :: Map.Map String RunPrim
allRunPrimMp = Map.fromList [ (showRunPrim p, p) | p <- [ minBound .. maxBound ] ]
%%]


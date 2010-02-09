%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% IO basic stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
module UHC.IOBase
  ( unsafePerformIO,
  
        -- To and from from ST
    stToIO, ioToST, unsafeIOToST, unsafeSTToIO,

        -- References
    IORef(..), newIORef, readIORef, writeIORef, 
    -- IOArray(..), newIOArray, readIOArray, writeIOArray, unsafeReadIOArray, unsafeWriteIOArray,
    -- MVar(..),

        -- Buffers
    Buffer(..), RawBuffer, BufferState(..), BufferList(..), BufferMode(..),
    bufferIsWritable, bufferEmpty, bufferFull, 
    -- extra:
    -- rawBufferContents,

        -- Handles, file descriptors,
    FilePath,  
    Handle(..), Handle__(..), HandleType(..), IOMode(..), FD,
    isReadableHandleType, isWritableHandleType, isReadWriteHandleType, {- showHandle, -}
    
    Handle,
    
        -- IOError + exception
    IOError       (..),
    IOErrorType   (..),
    IOException, ioException,
    ioError, userError,
    throwIOError,
    
        -- Exception
    SomeException,
#ifdef __UHC_TARGET_C__
    throw,
#endif
    
    	-- MVar
    MVar, MVar',   -- '

    try,

        -- Exception related: catch, throw
#ifdef __UHC_TARGET_C__
#else
    catchTracedException,
#endif
    catch, catchException,
    
#ifdef __UHC_TARGET_C__
    FHandle,
#endif
  )
  where

import UHC.Base
import UHC.ST
import UHC.STRef
import UHC.Types
import UHC.MutVar
import UHC.ByteArray

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% IO <-> ST
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This version assumes that ST has structure: state -> (state,result), and IO: state -> result
%%[9999
-- ---------------------------------------------------------------------------
-- Coercions between IO and ST

-- | A monad transformer embedding strict state transformers in the 'IO'
-- monad.  The 'RealWorld' parameter indicates that the internal state
-- used by the 'ST' computation is a special one supplied by the 'IO'
-- monad, and thus distinct from those used by invocations of 'runST'.
stToIO        :: ST RealWorld a -> IO a
stToIO (ST m) = IO (\s -> case m s of {(_,r) -> r})

ioToST        :: IO a -> ST RealWorld a
ioToST (IO m) = ST (\s -> case m s of {r -> (s,r)})

-- This relies on IO and ST having the same representation modulo the
-- constraint on the type of the state
--
unsafeIOToST        :: IO a -> ST s a
unsafeIOToST io = unsafeCoerce (ioToST io)

unsafeSTToIO :: ST s a -> IO a
unsafeSTToIO m = stToIO (unsafeCoerce m)

%%]

This version assumes that ST and IO have the same internal representation: state -> (state,result)
%%[99
-- ---------------------------------------------------------------------------
-- Coercions between IO and ST

-- | A monad transformer embedding strict state transformers in the 'IO'
-- monad.  The 'RealWorld' parameter indicates that the internal state
-- used by the 'ST' computation is a special one supplied by the 'IO'
-- monad, and thus distinct from those used by invocations of 'runST'.
stToIO        :: ST RealWorld a -> IO a
stToIO (ST m) = IO m

ioToST        :: IO a -> ST RealWorld a
ioToST (IO m) = (ST m)

-- This relies on IO and ST having the same representation modulo the
-- constraint on the type of the state
--
unsafeIOToST        :: IO a -> ST s a
unsafeIOToST (IO io) = ST $ \ s -> (unsafeCoerce io) s

unsafeSTToIO :: ST s a -> IO a
unsafeSTToIO (ST m) = IO (unsafeCoerce m)

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% try
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
-- | The construct 'try' @comp@ exposes IO errors which occur within a
-- computation, and which are not fully handled.
--
-- Non-I\/O exceptions are not caught by this variant; to catch all
-- exceptions, use 'Control.Exception.try' from "Control.Exception".

try            :: IO a -> IO (Either IOError a)
try f          =  catch (do r <- f
                            return (Right r))
                        (return . Left)

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% IORef
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
-- ---------------------------------------------------------------------------
-- IORefs

-- |A mutable variable in the 'IO' monad
newtype IORef a = IORef (STRef RealWorld a)

-- explicit instance because Haddock can't figure out a derived one
instance Eq (IORef a) where
  IORef x == IORef y = x == y

-- |Build a new 'IORef'
newIORef    :: a -> IO (IORef a)
newIORef v = stToIO (newSTRef v) >>= \ var -> return (IORef var)

-- |Read the value of an 'IORef'
readIORef   :: IORef a -> IO a
readIORef  (IORef var) = stToIO (readSTRef var)

-- |Write a new value into an 'IORef'
writeIORef  :: IORef a -> a -> IO ()
writeIORef (IORef var) v = stToIO (writeSTRef var v)

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unsafe
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
unsafePerformIO :: IO a -> a
unsafePerformIO (IO m)
  = case m ioWorld of
      (_, x) -> x

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Buffering
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
-- ---------------------------------------------------------------------------
-- Buffers

-- The buffer is represented by a mutable variable containing a
-- record, where the record contains the raw buffer and the start/end
-- points of the filled portion.  We use a mutable variable so that
-- the common operation of writing (or reading) some data from (to)
-- the buffer doesn't need to modify, and hence copy, the handle
-- itself, it just updates the buffer.  

-- There will be some allocation involved in a simple hPutChar in
-- order to create the new Buffer structure (below), but this is
-- relatively small, and this only has to be done once per write
-- operation.

-- The buffer contains its size - we could also get the size by
-- calling sizeOfMutableByteArray on the raw buffer, but that tends
-- to be rounded up to the nearest Word.

type RawBuffer = MutableByteArray RealWorld

{-
rawBufferContents :: RawBuffer -> Addr
rawBufferContents = mutableByteArrayContents
-}

-- INVARIANTS on a Buffer:
--
--   * A handle *always* has a buffer, even if it is only 1 character long
--     (an unbuffered handle needs a 1 character buffer in order to support
--      hLookAhead and hIsEOF).
--   * r <= w
--   * if r == w, then r == 0 && w == 0
--   * if state == WriteBuffer, then r == 0
--   * a write buffer is never full.  If an operation
--     fills up the buffer, it will always flush it before 
--     returning.
--   * a read buffer may be full as a result of hLookAhead.  In normal
--     operation, a read buffer always has at least one character of space.

data Buffer 
  = Buffer {
        bufBuf   :: RawBuffer,
        bufRPtr  :: !Int,
        bufWPtr  :: !Int,
        bufSize  :: !Int,
        bufState :: BufferState
  }

data BufferState = ReadBuffer | WriteBuffer deriving (Eq)

-- we keep a few spare buffers around in a handle to avoid allocating
-- a new one for each hPutStr.  These buffers are *guaranteed* to be the
-- same size as the main buffer.
data BufferList 
  = BufferListNil 
  | BufferListCons RawBuffer BufferList


bufferIsWritable :: Buffer -> Bool
bufferIsWritable Buffer{ bufState=WriteBuffer } = True
bufferIsWritable _other = False

bufferEmpty :: Buffer -> Bool
bufferEmpty Buffer{ bufRPtr=r, bufWPtr=w } = r == w

-- only makes sense for a write buffer
bufferFull :: Buffer -> Bool
bufferFull b@Buffer{ bufWPtr=w } = w >= bufSize b

--  Internally, we classify handles as being one
--  of the following:

data HandleType
 = ClosedHandle
 | SemiClosedHandle
 | ReadHandle
 | WriteHandle
 | AppendHandle
 | ReadWriteHandle
 deriving Eq

isReadableHandleType :: HandleType -> Bool
isReadableHandleType ReadHandle         = True
isReadableHandleType ReadWriteHandle    = True
isReadableHandleType _                  = False

isWritableHandleType :: HandleType -> Bool
isWritableHandleType AppendHandle    = True
isWritableHandleType WriteHandle     = True
isWritableHandleType ReadWriteHandle = True
isWritableHandleType _               = False

isReadWriteHandleType :: HandleType -> Bool
isReadWriteHandleType ReadWriteHandle{} = True
isReadWriteHandleType _                 = False


-- ---------------------------------------------------------------------------
-- Buffering modes

-- | Three kinds of buffering are supported: line-buffering, 
-- block-buffering or no-buffering.  These modes have the following
-- effects. For output, items are written out, or /flushed/,
-- from the internal buffer according to the buffer mode:
--
--  * /line-buffering/: the entire output buffer is flushed
--    whenever a newline is output, the buffer overflows, 
--    a 'System.IO.hFlush' is issued, or the handle is closed.
--
--  * /block-buffering/: the entire buffer is written out whenever it
--    overflows, a 'System.IO.hFlush' is issued, or the handle is closed.
--
--  * /no-buffering/: output is written immediately, and never stored
--    in the buffer.
--
-- An implementation is free to flush the buffer more frequently,
-- but not less frequently, than specified above.
-- The output buffer is emptied as soon as it has been written out.
--
-- Similarly, input occurs according to the buffer mode for the handle:
--
--  * /line-buffering/: when the buffer for the handle is not empty,
--    the next item is obtained from the buffer; otherwise, when the
--    buffer is empty, characters up to and including the next newline
--    character are read into the buffer.  No characters are available
--    until the newline character is available or the buffer is full.
--
--  * /block-buffering/: when the buffer for the handle becomes empty,
--    the next block of data is read into the buffer.
--
--  * /no-buffering/: the next input item is read and returned.
--    The 'System.IO.hLookAhead' operation implies that even a no-buffered
--    handle may require a one-character buffer.
--
-- The default buffering mode when a handle is opened is
-- implementation-dependent and may depend on the file system object
-- which is attached to that handle.
-- For most implementations, physical files will normally be block-buffered 
-- and terminals will normally be line-buffered.

data BufferMode  
 = NoBuffering  -- ^ buffering is disabled if possible.
 | LineBuffering
                -- ^ line-buffering should be enabled if possible.
 | BlockBuffering (Maybe Int)
                -- ^ block-buffering should be enabled if possible.
                -- The size of the buffer is @n@ items if the argument
                -- is 'Just' @n@ and is otherwise implementation-dependent.
   deriving (Eq, Ord, Read, Show)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% IO mode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
data IOMode             -- alphabetical order of constructors required, assumed Int encoding in comment
  = AppendBinaryMode    -- 0
  | AppendMode          -- 1
  | ReadBinaryMode      -- 2
  | ReadMode            -- 3
  | ReadWriteBinaryMode -- 4
  | ReadWriteMode       -- 5
  | WriteBinaryMode     -- 6
  | WriteMode           -- 7
    deriving (Eq, Ord, Bounded, Enum, Show)

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handle
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
#ifdef __UHC_TARGET_C__
data FHandle    -- opaque, contains FILE*
#else
data GBHandle   -- opaque, contains GB_Chan
#endif

#ifdef __UHC_TARGET_C__

instance Eq FHandle where
    _ == _ = False

instance Show FHandle where
    showsPrec _ h = showString "<handle>"

#else

foreign import prim primEqGBHandleFileno  :: GBHandle -> Int -> Bool
foreign import prim primEqGBHandle  :: GBHandle -> GBHandle -> Bool

instance Eq GBHandle where
    (==) = primEqGBHandle

instance Show GBHandle where
    showsPrec _ h
      = if      primEqGBHandleFileno h 0 then showString "<stdin>"
        else if primEqGBHandleFileno h 1 then showString "<stdout>"
        else if primEqGBHandleFileno h 2 then showString "<stderr>"
        else                showString ("<handle:" ++ {- show n ++ -} ">")
      -- where n = primGBHandleEqFileno h

#endif
%%]

%%[99
data Handle 
  = FileHandle                          -- A normal handle to a file
        FilePath                        -- the file (invariant)
        !(MVar Handle__)

  | DuplexHandle                        -- A handle to a read/write stream
        FilePath                        -- file for a FIFO, otherwise some
                                        --   descriptive string.
        !(MVar Handle__)                -- The read side
        !(MVar Handle__)                -- The write side

  | OldHandle                            
#ifdef __UHC_TARGET_C__
        FHandle                        
#else
        GBHandle                        
#endif

instance Eq Handle where
 (FileHandle _ h1)     == (FileHandle _ h2)     = h1 == h2
 (DuplexHandle _ h1 _) == (DuplexHandle _ h2 _) = h1 == h2
 (OldHandle h1)        == (OldHandle h2)        = h1 == h2
 _                     == _                     = False 

type FD = CInt

data Handle__
  = Handle__ {
      haFD          :: !FD,                  -- file descriptor
      haType        :: HandleType,           -- type (read/write/append etc.)
      haIsBin       :: Bool,                 -- binary mode?
      haIsStream    :: Bool,                 -- Windows : is this a socket?
                                             -- Unix    : is O_NONBLOCK set?
      haBufferMode  :: BufferMode,           -- buffer contains read/write data?
      haBuffer      :: !(IORef Buffer),      -- the current buffer
      haBuffers     :: !(IORef BufferList),  -- spare buffers
      haOtherSide   :: Maybe (MVar Handle__) -- ptr to the write side of a 
                                             -- duplex handle.
    }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Show of Handle
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
-- ---------------------------------------------------------------------------
-- Show instance for Handles

-- handle types are 'show'n when printing error msgs, so
-- we provide a more user-friendly Show instance for it
-- than the derived one.

instance Show HandleType where
  showsPrec _ t =
    case t of
      ClosedHandle      -> showString "closed"
      SemiClosedHandle  -> showString "semi-closed"
      ReadHandle        -> showString "readable"
      WriteHandle       -> showString "writable"
      AppendHandle      -> showString "writable (append)"
      ReadWriteHandle   -> showString "read-writable"

instance Show Handle where 
  showsPrec _ (FileHandle   file _)   = showHandle file
  showsPrec _ (DuplexHandle file _ _) = showHandle file
  showsPrec _ (OldHandle    file)     = shows file

showHandle :: FilePath -> String -> String
showHandle file = showString "{handle: " . showString file . showString "}"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FilePath
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
-- | File and directory names are values of type 'String', whose precise
-- meaning is operating system dependent. Files can be opened, yielding a
-- handle which can then be used to operate on the contents of that file.

type FilePath = String

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% IOError
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
----------------------------------------------------------------
-- IOError
----------------------------------------------------------------

data IOError
  = IOError
      { ioe_handle      :: Maybe Handle   -- the handle used by the action flagging the error
      , ioe_type        :: IOErrorType    -- what kind of (std) error
      , ioe_location    :: String         -- location of the error
      , ioe_description :: String         -- error-specific string
      , ioe_filename    :: Maybe FilePath -- the resource involved.
      } 
      deriving (Eq)

type IOException = IOError

data IOErrorType        -- alphabetical order of constructors required, assumed Int encoding in comment
  = AlreadyExists       -- 0
  | AlreadyInUse        -- 1 -- ResourceBusy
  | DoesNotExist        -- 2 -- NoSuchThing
  | EOF                 -- 3
  | FullError           -- 4
  | IllegalOperation    -- 5
  | InappropriateType   -- 6
  | InvalidArgument     -- 7
  | NoSuchThing			-- 8
  | OtherError    		-- 9
  | PermissionDenied    -- 10
  | ResourceBusy		-- 11
  | ResourceExhausted   -- 12
  | UnsupportedOperation-- 13
  | UserError           -- 14
    deriving (Eq)

instance Show IOErrorType where
  show x = 
    case x of
      AlreadyExists         -> "already exists"
      AlreadyInUse          -> "resource already in use"
      DoesNotExist          -> "does not exist"
      EOF                   -> "end of file"
      IllegalOperation      -> "illegal operation"
      InappropriateType     -> "inappropriate type"
      InvalidArgument       -> "invalid argument"
      NoSuchThing           -> "does not exist"
      OtherError            -> "other error"
      PermissionDenied      -> "permission denied"
      ResourceBusy          -> "resource already in use"
      ResourceExhausted     -> "resource exhausted"
      UnsupportedOperation  -> "unsuppored operation"
      UserError             -> "user error"

instance Show IOError where
  showsPrec p (IOError hdl iot loc s fn) =
    (case fn of
       Nothing -> case hdl of
                      Nothing -> id
                      Just h  -> showsPrec p h . showString ": "
       Just name -> showString name . showString ": ") .
    (case loc of
       "" -> id
       _  -> showString loc . showString ": ") .
    showsPrec p iot .
    (case s of
       "" -> id
       _  -> showString " (" . showString s . showString ")")


userError :: String -> IOError
userError str = IOError Nothing UserError "" str Nothing

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exception
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
type SomeException = SomeException' IOError
%%]

%%[99
----------------------------------------------------------------
-- Exception datatype and operations, must match the list in the RTS
----------------------------------------------------------------

instance Show SomeException where
  showsPrec _ (ArithException e)  = shows e
  showsPrec _ (ArrayException e)  = shows e
  showsPrec _ (AssertionFailed s) = showException "assertion failed" s
  showsPrec _ (AsyncException e)  = shows e
  showsPrec _ BlockedOnDeadMVar   = showString "thread blocked indefinitely"
  showsPrec _ Deadlock            = showString "<<deadlock>>"
  --showsPrec _ (DynException _)    = showString "unknown exception"
  showsPrec _ (ErrorCall s)       = showString s
  showsPrec _ (ExitException err) = showString "exit: " . shows err
  showsPrec _ (IOException err)   = shows err
  showsPrec _ (NoMethodError s)   = showException "undefined member" s
  showsPrec _ NonTermination      = showString "<<loop>>"
  showsPrec _ (PatternMatchFail s) = showException "pattern match failure" s
  showsPrec _ (RecConError s)     = showException "undefined field" s
  showsPrec _ (RecSelError s)     = showException "select of missing field" s
  showsPrec _ (RecUpdError s)     = showException "update of missing field" s

instance Show ArithException where
  showsPrec _ Overflow        = showString "arithmetic overflow"
  showsPrec _ Underflow       = showString "arithmetic underflow"
  showsPrec _ LossOfPrecision = showString "loss of precision"
  showsPrec _ DivideByZero    = showString "divide by zero"
  showsPrec _ Denormal        = showString "denormal"

instance Show ArrayException where
  showsPrec _ (IndexOutOfBounds s) =
    showException "array index out of range" s
  showsPrec _ (UndefinedElement s) =
    showException "undefined array element" s

instance Show AsyncException where
  showsPrec _ (StackOverflow msg)   = showString "stack overflow: " . showString msg
  showsPrec _ HeapOverflow          = showString "heap overflow"
  showsPrec _ ThreadKilled          = showString "thread killed"

showException :: String -> String -> ShowS
showException tag msg =
  showString tag . (if null msg then id else showString ": " . showString msg)

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Catch
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
#ifdef __UHC_TARGET_C__

catch :: IO a -> (IOError -> IO a) -> IO a
catch m h = m

catchException :: IO a -> (SomeException -> IO a) -> IO a
catchException m k = m

#else

foreign import prim primCatchException :: forall a . a -> (([(Int,String)],SomeException) -> a) -> a

catchTracedException :: IO a -> (([(Int,String)],SomeException) -> IO a) -> IO a
catchTracedException (IO m) k = IO $ \s ->
  primCatchException (m s)
                     (\te -> case (k te) of {IO k' -> k' s })

catchException :: IO a -> (SomeException -> IO a) -> IO a
catchException m k =
  catchTracedException m (\(_,e) -> k e)

catch :: IO a -> (IOError -> IO a) -> IO a
catch m h = catchException m $ \e -> case e of
                IOException err -> h err
                _ -> throw e

#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Throw
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
#ifdef __UHC_TARGET_C__

throw :: SomeException -> a
throw e = error (show e)

#endif
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% IOError wrapping
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
#ifdef __UHC_TARGET_C__
ioError :: IOError -> IO a
ioError = error "ioError"

#else
ioError :: IOError -> IO a
ioError e = IO (\s -> throwIOError e)

#endif

ioException :: IOError -> IO a
ioException = ioError

throwIOError :: IOError -> a
throwIOError e = throw (IOException e)

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MVar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
type    MVar' a = MutVar RealWorld (Maybe a)
-- type    MState = State RealWorld
newtype MVar  a = MVar (MVar' a)
%%]

%%[99
instance Eq (MVar a) where
   (MVar v1) == (MVar v2) = sameMutVar v1 v2
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
%%]


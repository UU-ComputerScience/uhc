% $Id$

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%[8 import(Control.Monad.Error,Control.Monad.State, Control.Exception, Data.Maybe)
%%]

%%[8.State import(GRINCCommon, EHCommon, GrinCode, FPath)
data CompileState = CompileState
	{ csUnique    :: Int
	, csMbCode    :: Maybe GrModule 
	, csEntry     :: !HsName
    , csMbOrigNms :: Maybe IdentNameMap
    , csMbHptMap  :: Maybe HptMap
	, csPath      :: FPath
	, csOpts      :: Opts
    , csMsgInfo   :: (Int, Bool)
	}

csGrinCode           = fromJust . csMbCode
csOrigNms            = fromJust . csMbOrigNms
csHptMap             = fromJust . csMbHptMap
csIsParsed           = isJust   . csMbCode
csUpdateGrinCode c s = s { csMbCode = Just c }
csUpdateUnique   u s = s { csUnique = u }
csUpdateHptMap   m s = s { csMbHptMap = Just m }
%%]

%%[8.Errors
newtype CompileError = CompileError String
	deriving (Show)

instance Error CompileError where
	noMsg    = CompileError "internal error"
	strMsg s = CompileError s
%%]

%%[8.CompilerDriver
type CompileAction a = ErrorT CompileError (StateT CompileState IO) a

drive :: CompileState -> (CompileError -> IO a) -> CompileAction a -> IO a
drive initState errorHandler action = do
	result <- doAction action
	case result of
		Right suc -> return suc
		Left  err -> errorHandler err
	where
	doAction = flip evalStateT initState . runErrorT

%%]

%%[8.errorHandling
ignoreErrors :: (Monad m) => a -> b -> m a
-- ignoreErrors = const . return -- does not typecheck in HM (but does in ML-F)
ignoreErrors v e = return v

harden   :: (MonadError e m) => a -> m a -> m a
harden v =  flip catchError (ignoreErrors v)

ignoreErrors_ :: (Monad m) => b -> m ()
ignoreErrors_ = ignoreErrors ()

harden_  :: (MonadError e m) => m() -> m ()
harden_  =  harden ()

force :: a -> CompileAction a
force = liftIO . evaluate 
%%]

%%[8.messages import(System.CPUTime, Numeric)
initMsgInfo :: (Int, Bool) -- indent, FirstMessageInLevel, CPUTime
initMsgInfo = (0, False)

putLn = putStrLn ""
 
putDebugMsg :: String -> CompileAction ()
putDebugMsg msg = harden_ $ do
    { isDebugging <- gets (optDebug . csOpts)
    ; guard isDebugging
    ; (indent, first) <- gets csMsgInfo
    ; when first (liftIO putLn >> modify (\s -> s { csMsgInfo = (indent, False) }))
    ; liftIO $ putStrLn ("[D] " ++ replicate (indent-4) ' ' ++ msg)
    }

putMsg :: Verbosity -> String -> (Maybe String) -> CompileAction ()
putMsg minVerbosity msg mbMsg =  harden_ $ do
    currentVerbosity <- gets (optVerbosity . csOpts)
    guard (currentVerbosity >= minVerbosity)
    (indent, first) <- gets csMsgInfo
    when first (liftIO putLn)
    let msg2    = maybe "" (\m -> " (" ++ m ++ ")") mbMsg
        message = replicate indent ' ' ++ strBlankPad 36 msg ++ msg2
    liftIO $ putStrLn message
    when first (modify (\s -> s { csMsgInfo = (indent, False) }))
    

task_ :: Verbosity -> String -> CompileAction a -> CompileAction ()
task_ minVerbosity td ca = task minVerbosity td ca (const Nothing)

task :: Verbosity -> String -> CompileAction a -> (a -> Maybe String) -> CompileAction ()
task minVerbosity taskDesc ca f = do 
    { startMsg minVerbosity taskDesc
    ; (cpuTime, r) <- cpuUsage ca
    ; finishMsg minVerbosity (f r) cpuTime
    }
    where
    startMsg :: Verbosity -> String -> CompileAction ()
    startMsg minVerbosity msg =  harden_ $ do
        currentVerbosity <- gets (optVerbosity . csOpts)
        guard (currentVerbosity >= minVerbosity)
        (indent, first) <- gets csMsgInfo
        when first (liftIO putLn)
        let message = replicate indent ' ' ++ strBlankPad 36 msg
        liftIO $ putStr message
        modify (\s -> s { csMsgInfo = (indent+4, True) })
    
    finishMsg :: Verbosity -> Maybe String -> Integer -> CompileAction ()
    finishMsg minVerbosity mbMsg cpuUsage =  harden_ $ do
        { currentVerbosity <- gets (optVerbosity . csOpts)
        ; guard (currentVerbosity >= minVerbosity)
        ; (oldIndent, first) <- gets csMsgInfo
        ; doTiming <- gets (optTimeCompile . csOpts)
        ; let indent       =  oldIndent - 4
              timeMsgOld   =  show (cpuUsage `div` fst cpuUsageInfo) ++ " " ++ snd cpuUsageInfo
              timeMsg      =  showFFloat (Just 2) (fromInteger cpuUsage / 1000000000000) " seconds"
              
              formatMsg m  | doTiming   =  if first
                                           then " (" ++ m ++ ", " ++ timeMsg ++ ")"
                                           else replicate indent ' ' ++ m ++ " (" ++ timeMsg ++ ")"
                           | otherwise  =  if first
                                           then " (" ++ m ++ ")"
                                           else replicate indent ' ' ++ m
              defaultMsg   | doTiming   =  if first
                                           then " (" ++ timeMsg ++ ")"
                                           else replicate indent ' ' ++ timeMsg
                           | otherwise  =  ""

        ; when (doTiming || first) $ liftIO (putStrLn $ maybe defaultMsg formatMsg mbMsg)
        ; modify (\s -> s { csMsgInfo = (indent, False) })
        }

cpuUsage :: CompileAction a -> CompileAction (Integer, a)
cpuUsage ca = do
    { start   <- liftIO getCPUTime
    ; result  <- ca
    ; end     <- liftIO getCPUTime
    ; return (end - start, result)
    }


{- returns - a value to remove all tailing zero's. Devide the CPU timing 
             to change the precision so that no decimal point is needed but
             without needless zeros are included (folowing SI-prefixes)
           - a string reprenstation of the precision of the resulting devision
-}
cpuUsageInfo = (\(a, b) -> (10^a, b)) closest
    where
    closest  =  head $ dropWhile (\a -> fst a > prec) table
    prec     =  floor (log (fromInteger cpuTimePrecision) / log 10)
    table    =  [ (12, "seconds")
--                , (11, "deci seconds")
--                , (10, "centi seconds")
                , (9, "milli seconds")
                , (6, "micro seconds")
                , (3, "nano seconds")
                , (0, "pico seconds")
                ]
%%]

%%[8.fixpoint
caFix :: CompileAction Bool -> CompileAction Int
caFix step = caFixCount 1
    where
    caFixCount n = do
        changes <- step 
        if changes then (caFixCount $ n+1) else return n
%%]

% vim:ts=4:et:ai:

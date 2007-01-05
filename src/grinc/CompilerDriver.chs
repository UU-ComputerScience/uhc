%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%[1 module {%{GRIN}CompilerDriver} export( doCompileGrin )
%%]

%%[8 import( System.IO, System.CPUTime, Numeric)
%%]
%%[8 import( Control.Monad.Error, Control.Monad.State, Control.Exception )
%%]
%%[8 import( Data.Maybe, Data.Array.IArray, qualified Data.Map as Map )
%%]
%%[8 import( UU.Parsing, UU.Pretty, EH.Util.CompileRun, EH.Util.FPath )
%%]
%%[8 import( EH.Util.CompileRun, EH.Util.FPath )
%%]
%%[8 import( {%{EH}Base.Common}, {%{EH}Base.Opts}, {%{EH}GrinCode}, {%{EH}GrinCode.Parser}, {%{EH}Scanner.Scanner}, {%{EH}Scanner.Common(grinScanOpts)} )
%%]
%%[8 import( {%{EH}GrinCode.Pretty})
%%]
%%[8 import( {%{GRIN}GRINCCommon} )
%%]

%%[8 import({%{GRIN}GrinCode.Trf.GrInline})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.LowerGrin})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.SplitFetch})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.ReturningCatch})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.DropUnusedExpr})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.SparseCase})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.DropUnusedTags})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.CaseElimination})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.CleanupPass})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.NumberIdents})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.BuildAppBindings})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.RightSkew})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.CopyPropagation})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.DropUnusedBindings})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.NormForHPT})
%%]
%%[8 import({%{GRIN}GrinCode.PointsToAnalysis})
%%]
%%[8 import({%{GRIN}GrinCode.GenSilly(grin2silly)}, {%{GRIN}Silly(SilModule)})
%%]
%%[8 import({%{GRIN}Silly.PrettyC(prettyC)},{%{GRIN}Silly.PrettyLLVM(prettyLL)})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compilerdriver entry point
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.doCompileGrin
doCompileGrin :: IO ()
doCompileGrin
  =  putStrLn "grinc: not available for this version (of ehc). Code generation is added in version 8."
%%]

%%[8 -1.doCompileGrin
doCompileGrin :: Either String (FPath,GrModule)  -> EHCOpts -> IO ()
doCompileGrin input opts
  = drive (initialState opts input) putErrs
      ( do { caLoad (isLeft input) -- from phd boquist (fig 4.1)
           ; caAnalyse
           ; caKnownCalls       -- part I
           ; caOptimizePartly   -- optimisations (small subset)
           ; caNormalize        -- part II
           ; caOptimize         -- optimisations
           ; caFinalize         -- part III
           ; caOutput
           }
      )
      
isLeft = either (const True) (const False)
      
initialState opts (Left fn)          = (initState opts) {gcsPath=mkTopLevelFPath "grin" fn}
initialState opts (Right (fp,grmod)) = (initState opts) {gcsPath=fp, gcsCode=grmod}

initState opts
  = GRINCompileState { gcsUnique     = 5          -- 0,1,2,3,4 are reserved for wildcard, eval, apply, main, mainexcept
                     , gcsCode       = undefined
                     , gcsHptMap     = undefined
                     , gcsPath       = emptyFPath
                     , gcsOpts       = opts
                     , gcsMsgInfo    = initMsgInfo
                     }
putErrs (CompileError e) = putStrLn e >> return ()
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% High level compiler actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
-- create initial GRIN
caLoad doParse = task_ VerboseNormal "Loading"
    ( do { when doParse caParseGrin
         ; caWriteGrin     True             "0-parsed"
         ; transformCode   cleanupPass      "Cleanup pass"
         ; transformTriple numberIdents     "Numbering identifiers"
         ; transformTriple buildAppBindings "Renaming lazy apply tags"
         ; caWriteGrin     True             "1-loaded"
         }
    )

-- create HPT info
caAnalyse = task_ VerboseNormal "Analyzing"
    ( do { caNormForHPT
         ; caRightSkew
         ; caHeapPointsTo
         ; debugging <- gets (ehcOptDebug . gcsOpts)
         ; when debugging (do { {- ((env, heap),_) <- gets gcsHptMap
                              ; let newVar i = show i
                              ; putDebugMsg "*** Equations ***"
                              ; printArray "env:"  newVar aeMod env
                              ; printArray "heap:" show ahMod heap
                              ; putDebugMsg "*** Abstract Values ***"
                              ; printArray "env:"  newVar aeBaseSet env
                              ; printArray "heap:" show ahBaseSet heap
                              ; -} caWriteGrin True "2-analyzed"
                              }
                          )
         }
    )

-- simplification part I
caKnownCalls = task_ VerboseNormal "Removing unknown calls"
    ( do { transformTriple inlineEA "Inlining Eval and Apply calls" 
         ; caRightSkew
         ; caWriteGrin True "3-knownCalls"
         }
    )
-- optionsations part I
caOptimizePartly = task_ VerboseNormal "Optimizing (partly)"
    ( do { transformTriple sparseCase "Removing impossible case alternatives"
         ; transformCode   caseElimination "Removing evaluated and trivial cases"
         ; transformTriple dropUnusedExpr "Remove unused expressions"
         ; caDropUnusedBindings
         ; caWriteGrin True "4-partlyOptimized"
         }
    )
-- simplification part II
caNormalize = task_ VerboseNormal "Normalizing"
    ( do { transformTriple lowerGrin "Lowering Grin"
         ; caWriteGrin True "5-normalized"
         }
    )

-- optionsations part II
caOptimize = task_ VerboseNormal "Optimizing (full)"
    ( do { caCopyPropagation
         ; caWriteGrin True "6-after-cp"
         ; transformTriple dropUnusedExpr "Remove unused expressions"
         ; caWriteGrin True "7-optimized"
         }
    )

-- simplification part III
caFinalize = task_ VerboseNormal "Finalizing"
    ( do { transformTriple splitFetch "Splitting and specializing fetch operations"
         ; transformTriple dropUnusedExpr "Remove unused expressions"
         ; transformCode   dropUnusedTags "Remove unused tags"
         ; transformTriple returnCatch "Ensure code exists after catch statement"
         ; caWriteGrin True "8-final"
         }
    )

-- write final code
caOutput = task_ VerboseNormal "Writing code"
    ( do { outputGrin <- gets (ehcOptDumpTrfGrin . gcsOpts)
         ; maybe (return ()) (caWriteGrin False) outputGrin
         ; options <- gets gcsOpts
         ; when (ehcOptEmitLLVM options)
           (caWriteFinalCode "ll" prettyLL)
         ; when (ehcOptEmitLlc options)
           (caWriteFinalCode "c" prettyC)
         }
    )
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Low level compiler actions: input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
parseGrin :: FPath -> IO GrModule
parseGrin path
  = do{ (fn,fh) <- openFPath path ReadMode
      ; tokens  <- scanHandle grinScanOpts fn fh
      ; code    <- parseIO (pModule) tokens
      ; return code
      }

caParseGrin :: CompileAction ()
caParseGrin 
  = do{ putMsg VerboseALot "Parsing" Nothing
      ; path <- gets gcsPath
      ; code <- liftIO $ parseGrin path
      ; modify (gcsUpdateGrinCode code)
      }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Low level compiler actions: processing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
caDropUnusedBindings :: CompileAction ()
caDropUnusedBindings = do
    { putMsg VerboseALot "Remove unused function bindings" Nothing
    ; code  <- gets gcsCode
    ; (code, dot) <- return $ dropUnusedBindings code
    ; modify (gcsUpdateGrinCode code)
    ; outputCallGraph <- gets (ehcOptDumpCallGraph . gcsOpts)
    ; when outputCallGraph
        (do { input <- gets gcsPath
            ; let output = fpathSetSuff "dot" input
            ; putMsg VerboseALot ("Writing call graph to " ++ fpathToStr output) Nothing
            ; liftIO $ writeToFile dot output
            }
        )
    }

caNormForHPT :: CompileAction ()
caNormForHPT = task VerboseALot "Normalizing"
    ( do { code   <- gets gcsCode
         ; unique <- gets gcsUnique
         ; (unique', code) <- return $ normForHPT unique code
         ; modify (gcsUpdateGrinCode code)
         ; modify (gcsUpdateUnique unique')
         ; return (unique' - unique)
         }
    ) (\i -> Just $ show i ++ " variable(s) introduced")

caHeapPointsTo :: CompileAction ()
caHeapPointsTo = task VerboseALot "Heap-points-to analysis"
    ( do { code    <- gets gcsCode
         ; unique  <- gets gcsUnique
         ; let (iterCount,unique2,hptMap) = heapPointsToAnalysis code unique
         ; modify (gcsUpdateHptMap hptMap)
         ; modify (gcsUpdateUnique unique2)
         ; return iterCount
         }
     ) (\i -> Just $ show i ++ " iteration(s)")


caCopyPropagation1 :: CompileAction Bool
caCopyPropagation1 = do
    code <- gets gcsCode
    (changed, code) <- return $ propagate code
    putDebugMsg (if changed then "Changes" else "No change")
    modify (gcsUpdateGrinCode code)
    return changed

caRightSkew1 :: CompileAction Bool
caRightSkew1 = do
    code <- gets gcsCode
    (code, changed) <- return $ rightSkew code
    modify (gcsUpdateGrinCode code)
    putDebugMsg (if changed then "Changes" else "No change")
    return changed

caCopyPropagation, caRightSkew :: CompileAction ()
caCopyPropagation = task VerboseALot "Copy propagation" (caFix caCopyPropagation1) (\i -> Just $ show i ++ " iteration(s)")
caRightSkew       = task VerboseALot "Unskewing"        (caFix caRightSkew1)       (\i -> Just $ show i ++ " iteration(s)")

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Low level compiler actions: output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
caGrin2Silly :: CompileAction SilModule
caGrin2Silly = do
    { code <- gets gcsCode
    ; hptMap  <- gets gcsHptMap
    ; optJump <- gets (ehcOptGenTailCall  . gcsOpts)
    ; optPar  <- gets (ehcOptGenOwnParams . gcsOpts)
    ; optLoc  <- gets (ehcOptGenOwnLocals . gcsOpts)
    ; return (grin2silly hptMap code optJump optPar optLoc)
    }


caWriteFinalCode :: String -> (Bool -> Bool -> SilModule -> PP_Doc) -> CompileAction ()
caWriteFinalCode suffix ppFun =
  do input <- gets gcsPath
     do { let output = fpathSetSuff suffix input
        ; putMsg VerboseALot ("Writing " ++ fpathToStr output) Nothing
        ; silly <- caGrin2Silly
        ; optTrace    <- gets (ehcOptGenTrace . gcsOpts)
        ; optCaseDef  <- gets (ehcOptGenCaseDefault . gcsOpts)
        ; liftIO $ writePP (ppFun optTrace optCaseDef) silly output
        }

caWriteGrin :: Bool -> String -> CompileAction ()
caWriteGrin debug fn = harden_ $ do -- bug: when writePP throws an exeption harden will block it
    { when debug (gets (ehcOptDebug . gcsOpts) >>= guard)
    ; input <- gets gcsPath
    ; let prefix     = if debug then "debug-" else ""
          fileName   = prefix ++ fpathBase input ++ if null fn then "-out" else fn
          output     = fpathSetSuff "grin" (fpathSetBase fileName input)
          message    = "Writing " ++ fpathToStr output
    ; if debug then putDebugMsg message else putMsg VerboseALot message Nothing
    ; code <- gets gcsCode
    ; options <- gets gcsOpts
    ; liftIO $ writePP ppGrModule code output
    }
%%]




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compilerdriver utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8.State
data GRINCompileState = GRINCompileState
    { gcsUnique    :: Int
    , gcsCode      :: GrModule
    , gcsHptMap    :: HptMap
    , gcsPath      :: FPath
    , gcsOpts      :: EHCOpts
    , gcsMsgInfo   :: (Int, Bool)
    }

gcsUpdateGrinCode c s = s { gcsCode   = c }
gcsUpdateUnique   u s = s { gcsUnique = u }
gcsUpdateHptMap   m s = s { gcsHptMap = m }

gcsGetTriple
  = do{ code   <- gets gcsCode
      ; unique <- gets gcsUnique
      ; hptMap <- gets gcsHptMap
      ; return (code,unique,hptMap)
      }

gcsPutTriple (code,unique,hptMap)
  = modify (\s -> s { gcsCode     = code
                    , gcsUnique   = unique
                    , gcsHptMap   = hptMap
                    }
           )

transformTriple :: ((GrModule,Int,HptMap) -> (GrModule,Int,HptMap)) -> String -> CompileAction ()
transformTriple process message = do
    putMsg VerboseALot message Nothing
    trip <- gcsGetTriple
    gcsPutTriple (process trip)

transformCode :: (GrModule->GrModule) -> String -> CompileAction ()
transformCode process message = do
    putMsg VerboseALot message Nothing
    code <- gets gcsCode
    modify (gcsUpdateGrinCode (process code))


caFix :: CompileAction Bool -> CompileAction Int
caFix step = caFixCount 1
    where
    caFixCount n = do
        changes <- step
        if changes then (caFixCount $ n+1) else return n

printArray s f g a = harden_ $ do
    { isDebugging <- gets (ehcOptDebug . gcsOpts)
    ; guard isDebugging
    ; putDebugMsg s
    ; mapM_ (\(k, v) -> putDebugMsg ("  " ++ f k ++ " = " ++ show (g v))) (assocs a)
    }

%%]


%%[8.Errors
newtype CompileError = CompileError String
    deriving (Show)

instance Error CompileError where
    noMsg    = CompileError "internal error"
    strMsg s = CompileError s
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compilerdriver abstractions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8.CompilerDriver
type CompileAction a = ErrorT CompileError (StateT GRINCompileState IO) a

drive :: GRINCompileState -> (CompileError -> IO a) -> CompileAction a -> IO a
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

%%[8
initMsgInfo :: (Int, Bool) -- indent, FirstMessageInLevel, CPUTime
initMsgInfo = (0, False)

putLn = putStrLn ""

putDebugMsg :: String -> CompileAction ()
putDebugMsg msg = harden_ $ do
    { isDebugging <- gets (ehcOptDebug . gcsOpts)
    ; guard isDebugging
    ; (indent, first) <- gets gcsMsgInfo
    ; when first (liftIO putLn >> modify (\s -> s { gcsMsgInfo = (indent, False) }))
    ; liftIO $ putStrLn ("[D] " ++ replicate (indent-4) ' ' ++ msg)
    }

putMsg :: Verbosity -> String -> (Maybe String) -> CompileAction ()
putMsg minVerbosity msg mbMsg =  harden_ $ do
    currentVerbosity <- gets (ehcOptVerbosity . gcsOpts)
    guard (currentVerbosity >= minVerbosity)
    (indent, first) <- gets gcsMsgInfo
    when first (liftIO putLn)
    let msg2    = maybe "" (\m -> " (" ++ m ++ ")") mbMsg
        message = replicate indent ' ' ++ strBlankPad 36 msg ++ msg2
    liftIO $ putStrLn message
    when first (modify (\s -> s { gcsMsgInfo = (indent, False) }))


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
        currentVerbosity <- gets (ehcOptVerbosity . gcsOpts)
        guard (currentVerbosity >= minVerbosity)
        (indent, first) <- gets gcsMsgInfo
        when first (liftIO putLn)
        let message = replicate indent ' ' ++ strBlankPad 36 msg
        liftIO $ putStr message
        modify (\s -> s { gcsMsgInfo = (indent+4, True) })

    finishMsg :: Verbosity -> Maybe String -> Integer -> CompileAction ()
    finishMsg minVerbosity mbMsg cpuUsage =  harden_ $ do
        { currentVerbosity <- gets (ehcOptVerbosity . gcsOpts)
        ; guard (currentVerbosity >= minVerbosity)
        ; (oldIndent, first) <- gets gcsMsgInfo
        ; doTiming <- gets (ehcOptTimeCompile . gcsOpts)
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
        ; modify (\s -> s { gcsMsgInfo = (indent, False) })
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
                , (9, "milli seconds")
                , (6, "micro seconds")
                , (3, "nano seconds")
                , (0, "pico seconds")
                ]
%%]


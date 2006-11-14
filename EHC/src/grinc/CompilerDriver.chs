%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%[1 module {%{GRIN}CompilerDriver} import(System.IO) export( doCompileGrin )
%%]

%%[8 import(Control.Monad.Error,Control.Monad.State, Control.Exception, Data.Maybe, EH.Util.FPath)
%%]
%%[8 import({%{GRIN}GRINCCommon}, {%{EH}Base.Common}, {%{EH}Scanner.Common(grinScanOpts)}, {%{EH}Base.Opts}, {%{EH}GrinCode}, {%{EH}Scanner.Scanner})
%%]
%%[8 import( UU.Parsing, UU.Pretty, EH.Util.CompileRun )
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Packaging as CompileRun actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compilerdriver entry point
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.doCompileGrin
doCompileGrin :: IO ()
doCompileGrin
  =  putStrLn "grinc: not available for this version (of ehc). Code generation is added in version 8."
%%]

%%[8 -1.doCompileGrin import(qualified Data.Map as Map, {%{GRIN}HeapPointsToFixpoint})
doCompileGrin :: Either String (FPath,GrModule)  -> EHCOpts -> IO ()
doCompileGrin inp opts
  = drive state putErrs
      ( do { load               -- from phd boquist (fig 4.1)
           ; caAnalyse
           ; caKnownCalls       -- part I
           ; caOptimizePartly   -- optimisations (small subset)
           ; caNormalize        -- part II
           ; caOptimize         -- optimisations
           ; caFinalize         -- part III
           ; caOutput
           }
      )
  where (state,load)
          = case inp of
              Left fn
                -> (initState {gcsPath = mkTopLevelFPath "grin" fn},caLoad True)
              Right (fp,grmod)
                -> (initState {gcsPath = fp, gcsMbCode = Just grmod},caLoad False)
        initState = GRINCompileState
            { gcsUnique     = 3                 -- 0,1,2 are reserved for wildcard, eval and apply respectively
            , gcsMbCode     = Nothing
            , gcsEntry      = HNm "main"
            , gcsMbOrigNms  = Nothing
            , gcsMbHptMap   = Nothing
            , gcsPath       = emptyFPath
            , gcsOpts       = opts
            , gcsMsgInfo    = initMsgInfo
            }
        putErrs (CompileError e) = putStrLn e >> return ()
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compilerdriver for GRINC (will become obsolete once included as part of EHC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8.parse import({%{EH}GrinCode.Parser})
parseGrin :: FPath -> EHCOpts -> IO (String, GrModule)
parseGrin fp opts = do
    (fn,fh) <- openFPath fp ReadMode
    tokens  <- scanHandle grinScanOpts fn fh
    gr      <- parseIO (pModule) tokens
    return (fn, gr)

caParseGrin :: CompileAction ()
caParseGrin = do
    putMsg VerboseALot "Parsing" Nothing
    path <- gets gcsPath
    opts <- gets gcsOpts
    (fn, code) <- liftIO $ parseGrin path opts
    modify (gcsUpdateGrinCode code)
%%]

%%[8.cleanup import({%{GRIN}GrinCode.Trf.CleanupPass})
caCleanupPass :: CompileAction ()
caCleanupPass = do
    putMsg VerboseALot "Cleanup pass" Nothing
    code <- gets gcsGrinCode
    code <- return $ cleanupPass code
    modify (gcsUpdateGrinCode code)
%%]

%%[8.dropUnusedBindings import({%{GRIN}GrinCode.Trf.DropUnusedBindings})
caDropUnusedBindings :: CompileAction ()
caDropUnusedBindings = do
    { putMsg VerboseALot "Remove unused function bindings" Nothing
    ; code  <- gets gcsGrinCode
    ; entry <- gets gcsEntry
    ; vm    <- gets gcsOrigNms
    ; (code, dot) <- return $ dropUnusedBindings entry vm code
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
%%]

%%[8.dropUnusedExpr import({%{GRIN}GrinCode.Trf.DropUnusedExpr})
caDropUnusedExpr :: CompileAction ()
caDropUnusedExpr = do
    { putMsg VerboseALot "Remove unused expressions" Nothing
    ; code    <- gets gcsGrinCode
    ; hptMap  <- gets gcsHptMap
    ; code <- return $ dropUnusedExpr hptMap code
    ; modify (gcsUpdateGrinCode code)
    }
%%]

%%[8.dropUnusedTags import({%{GRIN}GrinCode.Trf.DropUnusedTags})
caDropUnusedTags :: CompileAction ()
caDropUnusedTags = do
    putMsg VerboseALot "Remove unused tags" Nothing
    code <- gets gcsGrinCode
    code <- return $ dropUnusedTags code
    modify (gcsUpdateGrinCode code)
%%]

%%[8.addLazyApply import({%{GRIN}GrinCode.Trf.BuildAppBindings})
caAddLazyApplySupport :: CompileAction ()
caAddLazyApplySupport = do
    putMsg VerboseALot "Renaming lazy apply tags" Nothing
    code   <- gets gcsGrinCode
    unique <- gets gcsUnique
    (unique, code) <- return $ buildAppBindings unique code
    modify (gcsUpdateGrinCode code)
    modify (gcsUpdateUnique unique)
%%]

%%[8.returningCatch import({%{GRIN}GrinCode.Trf.ReturningCatch})
caReturningCatch :: CompileAction ()
caReturningCatch = do
    { putMsg VerboseALot "Ensure code exists after catch statement" Nothing
    ; code   <- gets gcsGrinCode
    ; hptMap  <- gets gcsHptMap
    ; unique <- gets gcsUnique
    ; (unique, code) <- return $ returnCatch hptMap unique code
    ; modify (gcsUpdateGrinCode code)
    ; modify (gcsUpdateUnique unique)
    }
%%]

%%[8.numberIdentifiers import({%{GRIN}GrinCode.Trf.NumberIdents}, Data.Array.IArray)
caNumberIdents :: CompileAction ()
caNumberIdents = task VerboseALot "Numbering identifiers"
    ( do { code   <- gets gcsGrinCode
         ; unique <- gets gcsUnique
         ; entry <- gets gcsEntry
         ; (unique, entry, code, varMap, idents) <- return $ numberIdents unique entry code
         ; modify (\s -> s { gcsMbOrigNms = Just varMap
                           , gcsMbCode = Just code
                           , gcsEntry = entry
                           , gcsUnique = unique
                           }
                  )
         ; return idents
         }
    ) (\i -> Just $ show i ++ " identifiers")
%%]

%%[8.nameIdents import({%{GRIN}GrinCode.Trf.NameIdents}, Data.Maybe)
caNameIdents :: CompileAction ()
caNameIdents = do
    putMsg VerboseALot "Naming identifiers" Nothing
    code  <- gets gcsGrinCode
    vm    <- gets gcsOrigNms
    code  <- return $ nameIdents vm code
    modify (\s -> s { gcsEntry     = fst vm ! getNr (gcsEntry s)
                    , gcsMbCode    = Just code
                    }
           )
%%]

%%[8.normForHPT import({%{GRIN}GrinCode.Trf.NormForHPT})
caNormForHPT :: CompileAction ()
caNormForHPT = task VerboseALot "Normalizing"
    ( do { code   <- gets gcsGrinCode
         ; unique <- gets gcsUnique
         ; (unique', code) <- return $ normForHPT unique code
         ; modify (gcsUpdateGrinCode code)
         ; modify (gcsUpdateUnique unique')
         ; return (unique' - unique)
         }
    ) (\i -> Just $ show i ++ " variable(s) introduced")
%%]

%%[8.rightSkew import({%{GRIN}GrinCode.Trf.RightSkew})
caRightSkew1 :: CompileAction Bool
caRightSkew1 = do
    code <- gets gcsGrinCode
    (code, changed) <- return $ rightSkew code
    modify (gcsUpdateGrinCode code)
    putDebugMsg (if changed then "Changes" else "No change")
    return changed

caRightSkew :: CompileAction ()
caRightSkew = task VerboseALot "Unskewing" (caFix caRightSkew1) (\i -> Just $ show i ++ " iteration(s)")
%%]

%%[8.heapPointsTo import({%{GRIN}GrinCode.PointsToAnalysis}, {%{GRIN}GrinCode.AbsEval})
caHeapPointsTo :: (Int, Int) -> CompileAction ()
caHeapPointsTo bounds = task VerboseALot "Heap-points-to analysis"
    ( do { code    <- gets gcsGrinCode
         ; (c,e,h) <- liftIO $ heapPointsToAnalysis bounds code
         ; modify (\s -> s { gcsMbHptMap = Just ((e,h), Map.empty) })
--       ; let n = abstractEvaluation code
         ; return c
         }
     ) (\i -> Just $ show i ++ " iteration(s)")

%%]

%%[8.inline import({%{GRIN}GrinCode.Trf.GrInline})
caInlineEA :: CompileAction Int
caInlineEA = do
    putMsg VerboseALot "Inlining Eval and Apply calls" Nothing
    code   <- gets gcsGrinCode
    hptMap <- gets gcsHptMap
    unique <- gets gcsUnique
    varMap <- gets gcsOrigNms
    (hptMap, unique', renMap, code)   <- return $ inlineEA hptMap unique code
    modify (\s -> s { gcsMbOrigNms  = Just $ mergeRenameMap varMap renMap
                    , gcsUnique     = unique'
                    , gcsMbHptMap   = Just hptMap
                    , gcsMbCode     = Just code
                    }
           )
    return $ unique' - unique
%%]

%%[8.sparseCase import({%{GRIN}GrinCode.Trf.SparseCase})
caSparseCase :: CompileAction ()
caSparseCase = do
    putMsg VerboseALot "Removing impossible case alternatives" Nothing
    code <- gets gcsGrinCode
    hptMap <- gets gcsHptMap
    code <- return $ sparseCase hptMap code
    modify (gcsUpdateGrinCode code)
%%]

%%[8.eliminateCase import({%{GRIN}GrinCode.Trf.CaseElimination})
caEliminateCases :: CompileAction ()
caEliminateCases = do
    putMsg VerboseALot "Removing evaluated and trivial cases" Nothing
    code <- gets gcsGrinCode
    code <- return $ eliminateCases code
    modify (gcsUpdateGrinCode code)
%%]

%%[8.propagate import({%{GRIN}GrinCode.Trf.CopyPropagation})
caCopyPropagation1 :: CompileAction Bool
caCopyPropagation1 = do
    code <- gets gcsGrinCode
    (changed, code) <- return $ propagate code
    putDebugMsg (if changed then "Changes" else "No change")
    modify (gcsUpdateGrinCode code)
    return changed

caCopyPropagation :: CompileAction ()
caCopyPropagation = task VerboseALot "Copy propagation" (caFix caCopyPropagation1) (\i -> Just $ show i ++ " iteration(s)")
%%]

%%[8.lowering import({%{GRIN}GrinCode.Trf.LowerGrin})
caLowerGrin :: CompileAction ()
caLowerGrin = do
    putMsg VerboseALot "Lowering GRIN" Nothing
    code   <- gets gcsGrinCode
    hptMap <- gets gcsHptMap
    unique <- gets gcsUnique
    varMap <- gets gcsOrigNms
    (hptMap, unique, renMap, code) <- return $ lowerGrin hptMap unique code
    modify (\s -> s { gcsMbOrigNms  = Just $ mergeRenameMap varMap renMap
                    , gcsUnique     = unique
                    , gcsMbHptMap   = Just hptMap
                    , gcsMbCode     = Just code
                    }
           )
%%]

%%[8.splittingFetch import({%{GRIN}GrinCode.Trf.SplitFetch})
caSplitFetch :: CompileAction ()
caSplitFetch = do
    { putMsg VerboseALot "Splitting and specializing fetch operations" Nothing
    ; code   <- gets gcsGrinCode
    ; hptMap <- gets gcsHptMap
    ; varMap <- gets gcsOrigNms
    ; unique <- gets gcsUnique
    ; (hptMap, unique', renMap, code)   <- return $ splitFetch hptMap unique code
    ; modify (\s -> s { gcsMbOrigNms  = Just $ mergeRenameMap varMap renMap
                      , gcsUnique     = unique'
                      , gcsMbHptMap   = Just hptMap
                      , gcsMbCode     = Just code
                      }
             )
    }
%%]


%%[8 import({%{GRIN}GrinCode.GenSilly(grin2silly)}, {%{GRIN}Silly(SilModule)})

caGrin2Silly :: CompileAction SilModule
caGrin2Silly = do
    { code <- gets gcsGrinCode
    ; hptMap  <- gets gcsHptMap
    ; optJump <- gets (ehcOptGenTailCall  . gcsOpts)
    ; optPar  <- gets (ehcOptGenOwnParams . gcsOpts)
    ; optLoc  <- gets (ehcOptGenOwnLocals . gcsOpts)
    ; return (grin2silly hptMap code optJump optPar optLoc)
    }
%%]


%%[8 import({%{GRIN}Silly.PrettyC(prettyC)},{%{GRIN}Silly.PrettyLLVM(prettyLL)})

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

caWriteLlc :: CompileAction ()
caWriteLlc = do
    {
     options <- gets gcsOpts
    ; when (ehcOptEmitLlc options)
           (caWriteFinalCode "c" prettyC)
    }

{-
caWriteLlc :: CompileAction ()
caWriteLlc = do
    { input <- gets gcsPath
    ; let output = fpathSetSuff "c" input
    ; options <- gets gcsOpts
    ; when (ehcOptEmitLlc options)
           (do { putMsg VerboseALot ("Writing " ++ fpathToStr output) Nothing
               ; silly <- caGrin2Silly
               ; optTrace    <- gets (ehcOptGenTrace . gcsOpts)
               ; optCaseDef  <- gets (ehcOptGenCaseDefault . gcsOpts)
               ; liftIO $ writePP (prettyC optTrace optCaseDef) silly output
               })
    }
-}
%%]


%%[8.writeGrin import({%{EH}GrinCode.Pretty})
caWriteGrin :: Bool -> String -> CompileAction ()
caWriteGrin debug fn = harden_ $ do -- bug: when writePP throws an exeption harden will block it
    { when debug (gets (ehcOptDebug . gcsOpts) >>= guard)
    ; input <- gets gcsPath
    ; let prefix     = if debug then "debug-" else ""
          fileName   = prefix ++ fpathBase input ++ if null fn then "-out" else fn
          output     = fpathSetSuff "grin" (fpathSetBase fileName input)
          message    = "Writing " ++ fpathToStr output
    ; if debug then putDebugMsg message else putMsg VerboseALot message Nothing
    ; code <- gets gcsGrinCode
    ; options <- gets gcsOpts
    ; liftIO $ writePP ppGrModule code output
    }
%%]

%%[8
-- create initial GRIN
caLoad doParse = task_ VerboseNormal "Loading"
    ( do { when doParse caParseGrin
         ; caWriteGrin True "0-parsed"
         ; caCleanupPass
         ; caNumberIdents
         ; caAddLazyApplySupport
         ; caWriteGrin True "1-loaded"
         }
    )

-- create HPT info
caAnalyse = task_ VerboseNormal "Analyzing"
    ( do { caNormForHPT
         ; caRightSkew
         ; high <- gets gcsUnique
         ; caHeapPointsTo (3,high-1)
         ; debugging <- gets (ehcOptDebug . gcsOpts)
         ; when debugging (do { ((env, heap),_) <- gets gcsHptMap
                              ; vm    <- gets gcsOrigNms
                              ; let newVar i = show (i, getName vm i)
                              ; putDebugMsg "*** Equations ***"
                              ; printArray "env:"  newVar aeMod env
                              ; printArray "heap:" show ahMod heap
                              ; putDebugMsg "*** Abstract Values ***"
                              ; printArray "env:"  newVar aeBaseSet env
                              ; printArray "heap:" show ahBaseSet heap
                              ; caWriteGrin True "2-analyzed"
                              }
                          )
         }
    )

-- simplification part I
caKnownCalls = task_ VerboseNormal "Removing unknown calls"
    ( do { caInlineEA
         ; caRightSkew
         ; caWriteGrin True "3-knownCalls"
         }
    )
-- optionsations part I
caOptimizePartly = task_ VerboseNormal "Optimizing (partly)"
    ( do { caSparseCase
         ; caEliminateCases
         ; caDropUnusedExpr
         ; caDropUnusedBindings
         ; caWriteGrin True "4-partlyOptimized"
         }
    )
-- simplification part II
caNormalize = task_ VerboseNormal "Normalizing"
    ( do { caLowerGrin
         ; caWriteGrin True "5-normalized"
         }
    )

-- optionsations part II
caOptimize = task_ VerboseNormal "Optimizing (full)"
    ( do { caCopyPropagation
         ; caWriteGrin True "6-after-cp"
         ; caDropUnusedExpr
         ; caWriteGrin True "7-optimized"
         }
    )

-- simplification part III
caFinalize = task_ VerboseNormal "Finalizing"
    ( do { caSplitFetch
         ; caDropUnusedExpr
         ; caDropUnusedTags
         ; caReturningCatch
         -- renaming numbered variables back to names wpuld make it impossible to use hptMap in ToSilly
         ; caNameIdents
         ; caWriteGrin True "8-final"
         }
    )

-- write final code
caOutput = task_ VerboseNormal "Writing code"
    ( do { outputGrin <- gets (ehcOptDumpTrfGrin . gcsOpts)
         ; maybe (return ()) (caWriteGrin False) outputGrin
         --; caWriteLlc
         ; options <- gets gcsOpts
         ; when (ehcOptEmitLLVM options)
           (caWriteFinalCode "ll" prettyLL)
         ; when (ehcOptEmitLlc options)
           (caWriteFinalCode "c" prettyC)
         }
    )

printArray s f g a = harden_ $ do
    { isDebugging <- gets (ehcOptDebug . gcsOpts)
    ; guard isDebugging
    ; putDebugMsg s
    ; mapM_ (\(k, v) -> putDebugMsg ("  " ++ f k ++ " = " ++ show (g v))) (assocs a)
    }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compilerdriver utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8.State
data GRINCompileState = GRINCompileState
    { gcsUnique    :: Int
    , gcsMbCode    :: Maybe GrModule
    , gcsEntry     :: !HsName
    , gcsMbOrigNms :: Maybe IdentNameMap
    , gcsMbHptMap  :: Maybe HptMap
    , gcsPath      :: FPath
    , gcsOpts      :: EHCOpts
    , gcsMsgInfo   :: (Int, Bool)
    }

gcsGrinCode           = fromJust . gcsMbCode
gcsOrigNms            = fromJust . gcsMbOrigNms
gcsHptMap             = fromJust . gcsMbHptMap
gcsIsParsed           = isJust   . gcsMbCode
gcsUpdateGrinCode c s = s { gcsMbCode = Just c }
gcsUpdateUnique   u s = s { gcsUnique = u }
gcsUpdateHptMap   m s = s { gcsMbHptMap = Just m }
%%]

%%[8.Errors
newtype CompileError = CompileError String
    deriving (Show)

instance Error CompileError where
    noMsg    = CompileError "internal error"
    strMsg s = CompileError s
%%]

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

%%[8.messages import(System.CPUTime, Numeric)
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

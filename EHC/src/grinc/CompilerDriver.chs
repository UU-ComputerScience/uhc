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
%%[8 import(Debug.Trace)
%%]
%%[8 import( UU.Parsing, EH.Util.Pretty, EH.Util.CompileRun, EH.Util.FPath )
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
%%[8 import({%{GRIN}GrinCode.Trf.TestUnbox})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.Unbox2})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.SplitFetch})
%%]
--%%[8 import({%{GRIN}GrinCode.Trf.ReturningCatch})
--%%]
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
%%[8 import({%{GRIN}GrinCode.Trf.AddFetch})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.RightSkew})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.CopyPropagation})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.DropUnusedBindings})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.DropDeadBindings})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.DropUnreachableBindings})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.NormForHPT})
%%]
%%[8 import({%{GRIN}GrinCode.PointsToAnalysis})
%%]
%%[8 import({%{GRIN}GrinCode.ToSilly(grin2silly)}, {%{GRIN}Silly(SilModule)})
%%]
%%[8 import({%{GRIN}Silly.ToLLVM(silly2llvm)}, {%{GRIN}LLVM(LLVMModule)})
%%]
%%[8 import({%{GRIN}Silly.PrettyC(prettyC)})
%%]
%%[8 import({%{GRIN}LLVM.Pretty(prettyLLVMModule)})
%%]
%%[8 import({%{GRIN}Silly.PrettyS(prettyS)})
%%]
%%[8 import({%{GRIN}Silly.Pretty(pretty)})
%%]
%%[8 import({%{GRIN}Silly.Shortcut(shortcut)})
%%]
%%[8 import({%{GRIN}Silly.EmbedVars(embedVars)})
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
           ; options <- gets gcsOpts
           ; when (ehcOptGrinDebug options) traceHptMap
           ; when (ehcOptEmitLLVM options || ehcOptEmitLlc options)
             caOutput
           }
      )
      
isLeft = either (const True) (const False)
      
initialState opts (Left fn)          = (initState opts) {gcsPath=mkTopLevelFPath "grin" fn}
initialState opts (Right (fp,grmod)) = (initState opts) {gcsPath=fp, gcsGrin=grmod}

initState opts
  = GRINCompileState { gcsUnique     = 3          -- 0,1,2 are reserved for wildcard, main, mainexcept
                     , gcsGrin       = undefined
                     , gcsSilly      = undefined
                     , gcsLLVM       = undefined
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
caLoad doParse = task_ VerboseNormal                "Loading"
    ( do { when doParse caParseGrin
         ; caWriteGrin           True               "10-parsed"
         ; transformCode         cleanupPass        "Cleanup pass"
         ; caWriteGrin           True               "11-cleaned"
         ; transformCodeUnq      buildAppBindings   "Renaming lazy apply tags"
         ; caWriteGrin           True               "12-renamed"
         ; transformCodeUnq      addFetch           "Adding fetches"
         ; caWriteGrin           True               "13-fetchadded"
         ; transformCodeUnq      numberIdents       "Numbering identifiers"
         ; caWriteGrin           True               "19-numbered"
         }
    )

-- create HPT info
caAnalyse = task_ VerboseNormal                     "Analyzing"
    ( do { transformCodeUnq      normForHPT         "Normalizing"
         ; caWriteGrin           True               "21-normalized"
         ; transformCodeIterated rightSkew          "Unskewing"
         ; caWriteGrin           True               "22-unskewed"
         ; transformCode         dropUnreachableBindings "Dropping unreachable bindings"
         ; caWriteGrin           True               "23-reachable"
         ; caHeapPointsTo
         ; caWriteGrin           True               "29-analyzed"
         }
    )

-- simplification part I
caKnownCalls = task_ VerboseNormal                  "Removing unknown calls"
    ( do { transformCodeUnqHpt   inlineEA           "Inlining Eval and Apply calls" 
         ; transformCodeIterated rightSkew          "Unskewing"
         ; caWriteGrin           True               "31-evalinlined"
         ; transformCodeUsingHpt dropDeadBindings   "Remove dead bindings"
         ; caWriteGrin           True               "32-undead"
         ; doUnbox <- gets (ehcOptGenUnbox . gcsOpts)
         ; when doUnbox (transformCodeUsingHpt unbox2 "Unboxing Int and Char")
         ; caWriteGrin           True               "39-unboxed"
         }
    )
-- optimisations part I
caOptimizePartly = task_ VerboseNormal              "Optimizing (partly)"
    ( do { transformCodeUsingHpt sparseCase         "Removing impossible case alternatives"
         ; caWriteGrin           True               "41-sparseCaseRemoved"
         ; transformCode         caseElimination    "Removing evaluated and trivial cases"
         ; caWriteGrin           True               "42-evaluatedCaseRemoved"
         ; transformCodeUsingHpt dropUnusedExpr     "Remove unused expressions"
         ; caWriteGrin           True               "43-unusedExprRemoved"
         ; transformCode         dropUnusedBindings "Dropping unused bindings"
         ; caWriteGrin           True               "49-partlyOptimized"
         }
    )
-- simplification part II
caNormalize = task_ VerboseNormal                   "Normalizing"
    ( do { transformCodeUnqHpt   lowerGrin          "Lowering Grin"
         ; caWriteGrin           True               "51-lowered"
         ; doUnbox <- gets (ehcOptGenUnbox . gcsOpts)
         ; when doUnbox (transformCodeUsingHpt unbox2 "Unboxing Int and Char")
         ; caWriteGrin           True               "59-unboxedagain"
         }
    )

-- optimisations part II
caOptimize = task_ VerboseNormal                    "Optimizing (full)"
    ( do { transformCodeIterated propagate          "Copy propagation"
         ; caWriteGrin           True               "61-after-cp"
         ; transformCodeUsingHpt dropUnusedExpr     "Remove unused expressions"
         ; caWriteGrin           True               "69-optimized"
         }
    )

-- simplification part III
caFinalize = task_ VerboseNormal                    "Finalizing"
    ( do { transformCodeUnqHpt   splitFetch         "Splitting and specializing fetch operations"
         ; caWriteGrin           True               "71-fetchSplitted"
         ; doUnbox <- gets (ehcOptGenUnbox . gcsOpts)
         ; when doUnbox (transformCodeUsingHpt unbox2 "Unboxing Int and Char")
         ; caWriteGrin           True               "72-unboxedoncemore"
         ; when doUnbox (transformCodeUnqHpt testUnbox "Testing Unboxed values")
         ; caWriteGrin           True               "73-unboxtested"
         ; transformCode         caseElimination    "Removing evaluated and trivial cases"
         ; transformCodeUsingHpt dropUnusedExpr     "Remove unused expressions"
         ; transformCodeUsingHpt dropUnusedExpr     "Remove unused expressions"
         ; caWriteGrin           True               "76-unusedExprRemoved"
         ; transformCode         dropUnusedTags     "Remove unused tags"
         ; caWriteGrin           True               "77-unusedTagsRemoved"
         ; transformCodeIterated propagate          "Copy propagation"
--       ; transformCodeUnqHpt   returnCatch        "Ensure code exists after catch statement"
         ; caWriteGrin           True               "79-final"
         }
    )

-- write final code
caOutput = task_ VerboseNormal "Writing code"
    ( do { options <- gets gcsOpts
    
         ; caGrin2Silly
         ; caWriteSilly "sil1" pretty
         ; transformSilly   shortcut      "Shortcut single-use variables"
         ; caWriteSilly "sil2" pretty
         ; transformSilly   embedVars     "Embed Variables"
         ; caWriteSilly "sil3" pretty
         ; transformSilly   shortcut      "Shortcut single-use variables"
         ; when (ehcOptEmitLLVM options)
            (do { caSilly2LLVM
                ; caWriteLLVM "ll" (const prettyLLVMModule)
                }
            )
         ; when (ehcOptEmitLlc options)
           (caWriteSilly "c" prettyC)
         ; when (ehcOptEmitLlc options)
           (caWriteSilly "s" prettyS)
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
      ; modify (gcsUpdateGrin code)
      }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Low level compiler actions: processing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8

caHeapPointsTo :: CompileAction ()
caHeapPointsTo = task VerboseALot "Heap-points-to analysis"
    ( do { code    <- gets gcsGrin
         ; unique  <- gets gcsUnique
         ; let (iterCount,hptMap) = heapPointsToAnalysis code unique
         ; modify (gcsUpdateHptMap hptMap)
         ; return iterCount
         }
     ) (\i -> Just $ show i ++ " iteration(s)")

caGrin2Silly :: CompileAction ()
caGrin2Silly = do
    { code <- gets gcsGrin
    ; hptMap  <- gets gcsHptMap
    ; opts    <- gets gcsOpts
    ; let silly = grin2silly hptMap code opts
    ; modify (gcsUpdateSilly silly)
    }

caSilly2LLVM :: CompileAction ()
caSilly2LLVM = do
    { code <- gets gcsSilly
    ; let llvm = silly2llvm code
    ; modify (gcsUpdateLLVM llvm)
    }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Low level compiler actions: output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
caWriteFile :: String -> (EHCOpts -> a -> PP_Doc) -> a -> CompileAction()
caWriteFile suffix ppFun struct =
  do { input <- gets gcsPath
     ; opts  <- gets gcsOpts
     ; do { let output = fpathSetSuff suffix input
          ; putMsg VerboseALot ("Writing " ++ fpathToStr output) Nothing
          ; liftIO $ writePP (ppFun opts) struct output
          }
     }

caWriteLLVM  :: String -> (EHCOpts -> LLVMModule -> PP_Doc) -> CompileAction()
caWriteLLVM suffix ppFun =
  do { llvm <- gets gcsLLVM
     ; caWriteFile suffix ppFun llvm
     } 
     
caWriteSilly :: String -> (EHCOpts -> SilModule -> PP_Doc) -> CompileAction ()
caWriteSilly suffix ppFun =
  do { silly <- gets gcsSilly
     ; caWriteFile suffix ppFun silly
     }

caWriteGrin :: Bool -> String -> CompileAction ()
caWriteGrin debug fn = harden_ $ do -- bug: when writePP throws an exeption harden will block it
    { when debug (gets (ehcOptGrinDebug . gcsOpts) >>= guard)
    ; input <- gets gcsPath
    ; let prefix     = if debug then "debug-" else ""
          fileName   = prefix ++ fpathBase input ++ if null fn then "-out" else fn
          output     = fpathSetSuff "grin" (fpathSetBase fileName input)
          message    = "Writing " ++ fpathToStr output
    ; if debug then putDebugMsg message else putMsg VerboseALot message Nothing
    ; code <- gets gcsGrin
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
    , gcsGrin      :: GrModule
    , gcsSilly     :: SilModule
    , gcsLLVM      :: LLVMModule
    , gcsHptMap    :: HptMap
    , gcsPath      :: FPath
    , gcsOpts      :: EHCOpts
    , gcsMsgInfo   :: (Int, Bool)
    }

gcsUpdateGrin   x s = s { gcsGrin   = x }
gcsUpdateSilly  x s = s { gcsSilly  = x }
gcsUpdateLLVM   x s = s { gcsLLVM   = x }
gcsUpdateUnique x s = s { gcsUnique = x }
gcsUpdateHptMap x s = s { gcsHptMap = x }

gcsGetCodeUnq
  = do{ code   <- gets gcsGrin
      ; unique <- gets gcsUnique
      ; return (code,unique)
      }

gcsGetCodeHpt
  = do{ code   <- gets gcsGrin
      ; hpt    <- gets gcsHptMap
      ; return (code,hpt)
      }

gcsGetCodeUnqHpt
  = do{ code   <- gets gcsGrin
      ; unique <- gets gcsUnique
      ; hptMap <- gets gcsHptMap
      ; return (code,unique,hptMap)
      }

gcsPutCodeUnq (code,unique)
  = modify (\s -> s { gcsGrin   = code
                    , gcsUnique = unique
                    }
           )

gcsPutCodeUnqHpt (code,unique,hptMap)
  = modify (\s -> s { gcsGrin   = code
                    , gcsUnique = unique
                    , gcsHptMap = hptMap
                    }
           )

traceHptMap :: CompileAction ()
traceHptMap
  = do { hptMap <- gets gcsHptMap
       ; trace (showHptMap hptMap) (return ())
       }

transformCode :: (GrModule->GrModule) -> String -> CompileAction ()
transformCode process message 
  = do { putMsg VerboseALot message Nothing
       ; grin <- gets gcsGrin
       ; modify (gcsUpdateGrin (process grin))
       }

transformCodeUsingHpt :: ((GrModule,HptMap)->GrModule) -> String -> CompileAction ()
transformCodeUsingHpt process message 
  = do { putMsg VerboseALot message Nothing
       ; ch <- gcsGetCodeHpt
       ; modify (gcsUpdateGrin (process ch))
       }

transformCodeUnq :: ((GrModule,Int) -> (GrModule,Int)) -> String -> CompileAction ()
transformCodeUnq process message 
  = do { putMsg VerboseALot message Nothing
       ; cu <- gcsGetCodeUnq
       ; gcsPutCodeUnq (process cu)
       }

transformCodeUnqHpt :: ((GrModule,Int,HptMap) -> (GrModule,Int,HptMap)) -> String -> CompileAction ()
transformCodeUnqHpt process message 
  = do { putMsg VerboseALot message Nothing
       ; trip <- gcsGetCodeUnqHpt
       ; gcsPutCodeUnqHpt (process trip)
       }

transformCodeIterated :: (GrModule->(GrModule,Bool)) -> String -> CompileAction ()
transformCodeIterated process message 
  = task VerboseALot message (caFixCount 1) (\i -> Just $ show i ++ " iteration(s)")
     where
     caFixCount n = do
         code <- gets gcsGrin
         (code, changed) <- return $ process code
         putDebugMsg (if changed then "Changes" else "No change")
         modify (gcsUpdateGrin code)
         if changed then (caFixCount $ n+1) else return n

transformSilly :: (EHCOpts->SilModule->SilModule) -> String -> CompileAction ()
transformSilly process message 
  = do { putMsg VerboseALot message Nothing
       ; silly <- gets gcsSilly
       ; options <- gets gcsOpts
       ; modify (gcsUpdateSilly (process options silly))
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
    { isDebugging <- gets (ehcOptGrinDebug . gcsOpts)
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


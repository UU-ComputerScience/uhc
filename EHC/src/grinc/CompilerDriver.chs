%%[1 module {%{GRIN}CompilerDriver} export(doCompileGrin)
%%]

%%[8 import(System.IO, System.CPUTime, Numeric)
%%]
%%[8 import(Control.Monad.Error, Control.Monad.State, Control.Exception)
%%]
%%[8 import(Data.Maybe, Data.Array.IArray, qualified Data.Map as Map, qualified Data.Set as Set)
%%]
%%[8 import(Debug.Trace)
%%]
%%[8 import(UU.Parsing)
%%]
%%[8 import(EH.Util.Pretty, EH.Util.CompileRun, EH.Util.FPath)
%%]
%%[8 import({%{EH}Base.Common}, {%{EH}Base.Opts}, {%{EH}Scanner.Scanner}, {%{EH}Scanner.Common(grinScanOpts)})
%%]
%%[8 import({%{EH}GrinCode}, {%{EH}GrinCode.Parser}, {%{EH}GrinCode.Pretty})
%%]
%%[8 import({%{GRIN}GRINCCommon})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.DropUnreachableBindings})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.CleanupPass})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.BuildAppBindings})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.Inline})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.FlattenSeq})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.SetGrinInvariant})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.ModeCheck})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.EvalStored})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.NumberIdents})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.DropUnusedExpr})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.NormForHPT})
%%]
%%[8 import({%{GRIN}GrinCode.PointsToAnalysis})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.InlineEA})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.DropDeadBindings})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.EmptyAlts})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.LateInline})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.ImpossibleCase})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.CaseElimination})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.MergeCase})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.LowerGrin})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.CopyPropagation})
%%]
%%[8 import({%{GRIN}GrinCode.Trf.SplitFetch})
%%]
%%[8 import({%{GRIN}GrinCode.ToSilly(grin2silly)})
%%]
%%[8 import({%{GRIN}Silly(SilModule)})
%%]
%%[8 import({%{GRIN}Silly.Shortcut(shortcut)})
%%]
%%[8 import({%{GRIN}Silly.EmbedVars(embedVars)})
%%]
%%[8 import({%{GRIN}Silly.Pretty(pretty)})
%%]
%%[8 import({%{GRIN}Silly.PrettyC(prettyC)})
%%]
%%[8 import({%{GRIN}Silly.PrettyS(prettyS)})
%%]
%%[8 import({%{GRIN}Silly.ToLLVM(silly2llvm)})
%%]
%%[8 import({%{GRIN}LLVM(LLVMModule)})
%%]
%%[8 import({%{GRIN}LLVM.Pretty(prettyLLVMModule)})
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
  = drive (initialState opts input) putErrs $
        do 
         { when (either (const True) (const False) input) caParseGrin  ; caWriteGrin "-110-parsed"
         ; transformCode         (dropUnreachableBindings False) 
                                             "DropUnreachableBindings" ; caWriteGrin "-111-reachable"
         ; transformCode         cleanupPass        "CleanupPass"      ; caWriteGrin "-112-cleaned"
         ; transformCodeUnq      buildAppBindings   "BuildAppBindings" ; caWriteGrin "-113-appsbound"
         ; transformCodeInline                      "Inline"
         ; transformCode         grFlattenSeq       "Flatten"          ; caWriteGrin "-114-inlined"
         ; transformCode         setGrinInvariant   "SetGrinInvariant" ; caWriteGrin "-115-invariant"
         ; checkCode             checkMode          "CheckGrinInvariant"
         ; transformCode         evalStored         "EvalStored"       ; caWriteGrin "-116-evalstored"
         ; transformCodeUnq      numberIdents       "NumberIdents"     ; caWriteGrin "-119-numbered"
         ; transformCodeIterated dropUnusedExpr     "DropUnusedExpr"   ; caWriteGrin "-120-unusedExprDropped"
         ; transformCodeUnq      normForHPT         "NormForHPT"
         ; transformCode         grFlattenSeq       "Flatten"          ; caWriteGrin "-121-normalized"
         ; caHeapPointsTo                                              ; caWriteHptMap "-130-hpt"
         ; transformCodeUnqHpt   inlineEA           "InlineEA" 
         ; transformCode         grFlattenSeq       "Flatten"          ; caWriteGrin "-131-evalinlined"
         ; transformCodeUsingHpt dropDeadBindings   "DropDeadBindings" ; caWriteGrin "-132-undead"
         ; transformCode         emptyAlts          "EmptyAlts"        ; caWriteGrin "-133-emptyAlts"
         ; transformCode         (dropUnreachableBindings True) 
                                             "DropUnreachableBindings" ; caWriteGrin "-134-reachable"
         ; transformCodeUnq      lateInline         "LateInline"
         ; transformCode         grFlattenSeq       "Flatten"          ; caWriteGrin "-135-lateinlined"
         ; transformCodeUsingHpt impossibleCase     "ImpossibleCase"   ; caWriteGrin "-141-possibleCase"
         ; transformCode         caseElimination    "CaseElimination"  ; caWriteGrin "-143-evaluatedCaseRemoved"
         ; transformCodeIterated dropUnusedExpr     "DropUnusedExpr"   ; caWriteGrin "-144-unusedExprDropped"
		 ; transformCode         mergeCase          "MergeCase"        ; caWriteGrin "-145-caseMerged"         
         ; transformCodeUnqHpt   lowerGrin          "LowerGrin"        ; caWriteGrin "-151-lowered"
         ; transformCodeIterated propagate          "Propagate"        ; caWriteGrin "-161-after-cp"
         ; transformCodeIterated dropUnusedExpr     "DropUnusedExpr"   ; caWriteGrin "-169-unusedExprDropped"
         ; transformCodeUnqHpt   splitFetch         "SplitFetch"       ; caWriteGrin "-171-splitFetch"
         ; transformCode         caseElimination    "CaseElimination"  ; caWriteGrin "-172-evaluatedCaseRemoved"
         ; transformCodeIterated dropUnusedExpr     "DropUnusedExpr"   ; caWriteGrin "-176-unusedExprDropped"
         ; transformCodeIterated propagate          "Propagate"        ; caWriteGrin "-179-final"
                                                                       ; caWriteHptMap "-180-hpt"
         ; options <- gets gcsOpts
         ; when (ehcOptEmitLLVM options || ehcOptEmitC options)
           ( do { caGrin2Silly                                         ; caWriteSilly "-201" "sil" pretty ehcOptDumpGrinStages
                ; transformSilly shortcut           "Shortcut"         ; caWriteSilly "-202" "sil" pretty ehcOptDumpGrinStages
                ; transformSilly embedVars          "EmbedVars"        ; caWriteSilly "-203" "sil" pretty ehcOptDumpGrinStages
                ; transformSilly shortcut           "Shortcut"         ; caWriteSilly "-204" "sil" pretty ehcOptDumpGrinStages
                ; when (ehcOptEmitLLVM options) 
                  (do { caSilly2LLVM
                      ; caWriteLLVM
                      }
                   )
                ; caWriteSilly "" "c" prettyC ehcOptEmitC
--              ; caWriteSilly "" "s" prettyS ehcOptEmitC
                }
           )
         }
      
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
    ; opts    <- gets gcsOpts
    ; let llvm = silly2llvm opts code
    ; modify (gcsUpdateLLVM llvm)
    }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Low level compiler actions: output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
caWriteFile :: String -> String -> (EHCOpts -> a -> PP_Doc) -> a -> CompileAction()
caWriteFile extra suffix ppFun struct =
  do { input <- gets gcsPath
     ; opts  <- gets gcsOpts
     ; do { let fileName  = fpathBase input ++ extra
                output    = fpathSetSuff suffix (fpathSetBase fileName input)
          ; putMsg VerboseALot ("Writing " ++ fpathToStr output) Nothing
          ; liftIO $ writePP (ppFun opts) struct output
          }
     }

caWriteLLVM  :: CompileAction()
caWriteLLVM  =
  do { llvm <- gets gcsLLVM
     ; caWriteFile "" "ll" (const prettyLLVMModule) llvm
     } 

caWriteGrin :: String -> CompileAction ()
caWriteGrin extra
  = do { opts <- gets gcsOpts
       ; when (ehcOptDumpGrinStages opts)
           (do { grin <- gets gcsGrin
               ; caWriteFile extra "grin" (const ppGrModule) grin
               }
           )
       }
     
caWriteSilly :: String -> String -> (EHCOpts -> SilModule -> PP_Doc) -> (EHCOpts->Bool) -> CompileAction ()
caWriteSilly extra suffix ppFun cond =
  do { opts <- gets gcsOpts
     ; when (cond opts)
            ( do { silly <- gets gcsSilly
                 ; caWriteFile extra suffix ppFun silly
                }
            )
     }

caWriteHptMap :: String -> CompileAction ()
caWriteHptMap fn
  = do { opts <- gets gcsOpts
       ; when (ehcOptDumpGrinStages opts)
           ( do { hptMap <- gets gcsHptMap
                ; input <- gets gcsPath
                ; let fileName   = fpathBase input ++ fn
                      output = fpathSetSuff "txt" (fpathSetBase fileName input)
                ; liftIO $ writeToFile (showHptMap hptMap) output
                }
           )
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

checkCode :: (GrModule->[String]) -> String -> CompileAction ()
checkCode process message
  = do { putMsg VerboseALot message Nothing
       ; grin <- gets gcsGrin
       ; let errors = checkMode grin
       ; when (not (null errors)) (error (unlines errors))
       }





transformCodeInline :: String -> CompileAction ()
transformCodeInline message 
  = do { putMsg VerboseALot message Nothing
       ; grin <- gets gcsGrin
%%[[8
       ; let code = grInline True grin
%%][20
       ; let (code,_) = grInline True Set.empty Map.empty grin 
%%]]
       ; modify (gcsUpdateGrin code)
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
    { dumpStages <- gets (ehcOptDumpGrinStages . gcsOpts)
    ; guard dumpStages
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







-- Idiom for doing a transformation only when the --priv=1 option is in effect:

         ; options <- gets gcsOpts
         ; when (ehcOptPriv options)
                ( do { transformCode         evalStored         "EvalStored"
                     ; caWriteGrin "-116-evalstored"
                     }
                )

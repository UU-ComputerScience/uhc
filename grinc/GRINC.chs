% $Id$

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module Main import(System.IO, System.Environment, System.Console.GetOpt, Control.Monad.Error, Control.Monad.State)
%%]

%%[8 import(UU.Parsing, UU.Pretty, EHCommon, EHScanner, GrinCode)
%%]

%%[8 import (FPath,GRINCCommon, CompilerDriver)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.main
main :: IO ()
main
  =  putStrLn "grinc: not available for this version (of ehc). Code generation is added in version 8."
%%]

%%[8.main -1.main
main :: IO ()
main
  =  do  {  args <- getArgs
         ;  let  oo@(o,n,errs)  = getOpt Permute cmdLineOpts args
                 opts           = foldr ($) defaultOpts o
         ;  if optHelp opts
            then  putStrLn (usageInfo "Usage: grinc [options] [file]\n\noptions:" cmdLineOpts)
            else  if null errs
                  then  doCompileRun (if null n then "" else head n) opts
                  else  mapM_ (\o -> putStr $ "grinc: " ++ o) errs
         }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compiler driver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8.utils
openFPath :: FPath -> IOMode -> IO (String, Handle)
openFPath fp mode | fpathIsEmpty fp = case mode of 
                                        ReadMode      -> return ("<stdin>" ,stdin )
                                        WriteMode     -> return ("<stdout>",stdout)
                                        AppendMode    -> return ("<stdout>",stdout)
                                        ReadWriteMode -> error "cannot use stdin/stdout with random access"
                  | otherwise       = do
                                        let fNm = fpathToStr fp
                                        h <- openFile fNm mode
                                        return (fNm,h)


writePP ::  (a -> PP_Doc) -> a -> FPath -> IO ()
writePP f text fp = writeToFile (show.f $ text) fp

writeToFile str fp 
  = do { (fn, fh) <- openFPath fp WriteMode
       ; hPutStrLn fh str
       ; hClose fh
       }
%%]

%%[8.parse import(GRIParser)
parseGrin :: FPath -> Opts -> IO (String, GrModule)
parseGrin fp opts = do
    (fn,fh) <- openFPath fp ReadMode
    tokens  <- scanHandle scanOpts fn fh
    gr      <- parseIO (pModule) tokens
    return (fn, gr)

caParseGrin :: CompileAction ()
caParseGrin = do
    putMsg VerboseALot "Parsing" Nothing
    path <- gets csPath
    opts <- gets csOpts
    (fn, code) <- liftIO $ parseGrin path opts
    modify (csUpdateGrinCode code)
%%]

%%[8.dropEvalAndApply import(Trf.CleanupPass)
caCleanupPass :: CompileAction ()
caCleanupPass = do
    putMsg VerboseALot "Cleanup pass" Nothing
    code <- gets csGrinCode
    code <- return $ cleanupPass code
    modify (csUpdateGrinCode code)
%%]

%%[8.dropUnusedBindings import(Trf.DropUnusedBindings)
caDropUnusedBindings :: CompileAction ()
caDropUnusedBindings = do
    { putMsg VerboseALot "Remove unused function bindings" Nothing
    ; code  <- gets csGrinCode
    ; entry <- gets csEntry
    ; vm    <- gets csOrigNms
    ; (code, dot) <- return $ dropUnusedBindings entry vm code
    ; modify (csUpdateGrinCode code)
    ; outputCallGraph <- gets (optWriteCallGraph . csOpts)
    ; when outputCallGraph
        (do { input <- gets csPath
            ; let output = fpathSetSuff "dot" input
            ; putMsg VerboseALot ("Writing call graph to " ++ fpathToStr output) Nothing
            ; liftIO $ writeToFile dot output
            }
        )
    }
%%]

%%[8.dropUnusedCatch import(Trf.DropUnusedCatch)
caDropUnusedCatch :: CompileAction ()
caDropUnusedCatch = do
    putMsg VerboseALot "Removing unused catch statements" Nothing
    code <- gets csGrinCode
    hptMap <- gets csHptMap
    code <- return $ dropUnusedCatch hptMap code
    modify (csUpdateGrinCode code)
%%]


%%[8.dropUnusedExpr import(Trf.DropUnusedExpr)
caDropUnusedExpr :: CompileAction ()
caDropUnusedExpr = do
    putMsg VerboseALot "Remove unused expressions" Nothing
    code <- gets csGrinCode
    code <- return $ dropUnusedExpr code
    modify (csUpdateGrinCode code)
%%]

%%[8.dropUnusedTags import(Trf.DropUnusedTags)
caDropUnusedTags :: CompileAction ()
caDropUnusedTags = do
    putMsg VerboseALot "Remove unused tags" Nothing
    code <- gets csGrinCode
    code <- return $ dropUnusedTags code
    modify (csUpdateGrinCode code)
%%]

%%[8.addLazyApply import(Trf.BuildAppBindings)
caAddLazyApplySupport :: CompileAction ()
caAddLazyApplySupport = do
    putMsg VerboseALot "Renaming lazy apply tags" Nothing
    code   <- gets csGrinCode
    unique <- gets csUnique
    (unique, code) <- return $ buildAppBindings unique code
    modify (csUpdateGrinCode code)
    modify (csUpdateUnique unique)
%%]


%%[8.numberIdentifiers import(Trf.NumberIdents, Data.Array.IArray)
caNumberIdents :: CompileAction ()
caNumberIdents = task VerboseALot "Numbering identifiers"
    ( do { code   <- gets csGrinCode
         ; unique <- gets csUnique
         ; entry <- gets csEntry
         ; (unique, entry, code, varMap, idents) <- return $ numberIdents unique entry code
         ; modify (\s -> s { csMbOrigNms = Just varMap
                           , csMbCode = Just code
                           , csEntry = entry
                           , csUnique = unique
                           }
                  )
         ; return idents
         }
    ) (\i -> Just $ show i ++ " identifiers")
%%]

%%[8.nameIdents import(Trf.NameIdents, Data.Maybe)
caNameIdents :: CompileAction ()
caNameIdents = do
    putMsg VerboseALot "Naming identifiers" Nothing
    code  <- gets csGrinCode
    vm    <- gets csOrigNms
    code  <- return $ nameIdents vm code
    modify (\s -> s { csEntry     = fst vm ! getNr (csEntry s)
                    , csMbCode    = Just code
                    }
           )
%%]

%%[8.normForHPT import(Trf.NormForHPT)
caNormForHPT :: CompileAction ()
caNormForHPT = task VerboseALot "Normalizing"
    ( do { code   <- gets csGrinCode
         ; unique <- gets csUnique
         ; (unique', code) <- return $ normForHPT unique code
         ; modify (csUpdateGrinCode code)
         ; modify (csUpdateUnique unique')
         ; return (unique' - unique)
         }
    ) (\i -> Just $ show i ++ " variable(s) introduced")
%%]

%%[8.rightSkew import(Trf.RightSkew)
caRightSkew1 :: CompileAction Bool
caRightSkew1 = do 
    code <- gets csGrinCode
    (code, changed) <- return $ rightSkew code
    modify (csUpdateGrinCode code)
    putDebugMsg (if changed then "Changes" else "No change")
    return changed

caRightSkew :: CompileAction ()
caRightSkew = task VerboseALot "Unskewing" (caFix caRightSkew1) (\i -> Just $ show i ++ " iteration(s)")
%%]

%%[8.heapPointsTo import(GrPointsToAnalysis)
caHeapPointsTo :: (Int, Int) -> CompileAction ()
caHeapPointsTo bounds = task VerboseALot "Heap-points-to analysis" 
    ( do { code    <- gets csGrinCode
         ; (c,e,h) <- liftIO $ heapPointsToAnalysis bounds code
         ; modify (\s -> s { csMbHptMap = Just ((e,h), Map.empty) })
         ; return c
         }
     ) (\i -> Just $ show i ++ " iteration(s)")
           
%%]

%%[8.inline import(Trf.GrInline)
caInlineEA :: CompileAction Int
caInlineEA = do
    putMsg VerboseALot "Inlining Eval and Apply calls" Nothing
    code   <- gets csGrinCode
    hptMap <- gets csHptMap
    unique <- gets csUnique
    varMap <- gets csOrigNms
    (hptMap, unique', renMap, code)   <- return $ inlineEA hptMap unique code
    modify (\s -> s { csMbOrigNms  = Just $ mergeRenameMap varMap renMap
                    , csUnique     = unique'
                    , csMbHptMap   = Just hptMap
                    , csMbCode     = Just code
                    }
           )
    return $ unique' - unique
%%]

%%[8.sparseCase import(Trf.SparseCase)
caSparseCase :: CompileAction ()
caSparseCase = do
    putMsg VerboseALot "Removing impossible case alternatives" Nothing
    code <- gets csGrinCode
    hptMap <- gets csHptMap
    code <- return $ sparseCase hptMap code
    modify (csUpdateGrinCode code)
%%]

%%[8.eliminateCase import(Trf.CaseElimination)
caEliminateCases :: CompileAction ()
caEliminateCases = do
    putMsg VerboseALot "Removing evaluated and trivial cases" Nothing
    code <- gets csGrinCode
    code <- return $ eliminateCases code
    modify (csUpdateGrinCode code)
%%]

%%[8.propagate import(Trf.CopyPropagation)
caCopyPropagation1 :: CompileAction Bool
caCopyPropagation1 = do
    code <- gets csGrinCode
    (changed, code) <- return $ propagate code
    putDebugMsg (if changed then "Changes" else "No change")
    modify (csUpdateGrinCode code)
    return changed

caCopyPropagation :: CompileAction ()
caCopyPropagation = task VerboseALot "Copy propagation" (caFix caCopyPropagation1) (\i -> Just $ show i ++ " iteration(s)")
%%]

%%[8.lowering import(Trf.LowerGrin)
caLowerGrin :: CompileAction ()
caLowerGrin = do
    putMsg VerboseALot "Lowering GRIN" Nothing
    code   <- gets csGrinCode
    hptMap <- gets csHptMap
    unique <- gets csUnique
    varMap <- gets csOrigNms
    (hptMap, unique, renMap, code) <- return $ lowerGrin hptMap unique code
    modify (\s -> s { csMbOrigNms  = Just $ mergeRenameMap varMap renMap
                    , csUnique     = unique
                    , csMbHptMap   = Just hptMap
                    , csMbCode     = Just code
                    }
           )
%%]

%%[8.splittingFetch import(Trf.SplitFetch)
caSplitFetch :: CompileAction ()
caSplitFetch = do
    { putMsg VerboseALot "Splitting and specializing fetch operations" Nothing
    ; code   <- gets csGrinCode
    ; hptMap <- gets csHptMap
    ; varMap <- gets csOrigNms
    ; unique <- gets csUnique
    ; (hptMap, unique', renMap, code)   <- return $ splitFetch hptMap unique code
    ; modify (\s -> s { csMbOrigNms  = Just $ mergeRenameMap varMap renMap
                      , csUnique     = unique'
                      , csMbHptMap   = Just hptMap
                      , csMbCode     = Just code
                      }
             )
    }
%%]

%%[8.writeCmm import(Cmm.FromGrin, Cmm.CmmCodePretty)
caGrin2Cmm :: CompileAction CmmUnit
caGrin2Cmm = do 
    { code <- gets csGrinCode
    ; entry <- gets csEntry
    ; doTrace <- gets (optTrace . csOpts)
    ; return (grin2cmm entry code doTrace)
    }

caWriteCmm :: CompileAction ()
caWriteCmm = do
    { input <- gets csPath
    ; let output = fpathSetSuff "cmm" input
    ; options <- gets csOpts
    ; putMsg VerboseALot ("Writing " ++ fpathToStr output) Nothing
    ; cmm <- caGrin2Cmm
    ; liftIO $ writePP pp cmm output
    }
%%]

    -- fpathToStr
    -- fpathBase

%%[8.writeGrin import(GrinCodePretty)
caWriteGrin :: Bool -> String -> CompileAction ()
caWriteGrin debug fn = harden_ $ do -- bug: when writePP throws an exeption harden will block it
    { when debug (gets (optDebug . csOpts) >>= guard)
    ; input <- gets csPath
    ; let prefix     = if debug then "debug." else ""
          fileName   = prefix ++ if null fn then fpathBase input ++ "-out" else fn
          output   =  fpathSetBase fileName input 
          message  =  "Writing " ++ fpathToStr output
    ; if debug then putDebugMsg message else putMsg VerboseALot message Nothing
    ; code <- gets csGrinCode
    ; options <- gets csOpts
    ; liftIO $ writePP (ppGrModule Nothing) code output
    }
%%]

%%[8 import("qualified Data.Map as Map", HeapPointsToFixpoint)
doCompileRun :: String -> Opts -> IO ()
doCompileRun fn opts = let input     = mkTopLevelFPath "grin" fn
                           initState = CompileState
                               { csUnique     = 3                 -- 0,1,2 are reserved (resp: __, eval, apply)
                               , csMbCode     = Nothing
                               , csEntry      = HNm "main"
                               , csMbOrigNms  = Nothing
                               , csMbHptMap   = Nothing
                               , csPath       = input
                               , csOpts       = opts
                               , csMsgInfo    = initMsgInfo
                               }
                           putErrs (CompileError e) = putStrLn e >> return ()
                       in drive initState putErrs ( do { caLoad             -- from phd boquist (fig 4.1)
                                                       ; caAnalyse          
                                                       ; caKnownCalls       -- part I
                                                       ; caOptimizePartly   -- optimisations (small subset)
                                                       ; caNormalize        -- part II
                                                       ; caOptimize         -- optimisations
                                                       ; caFinalize         -- part III
                                                       ; caOutput
                                                       }
                                                  )

-- create initial GRIN
caLoad = task_ VerboseNormal "Loading" 
    ( do { caParseGrin
         ; caCleanupPass
         ; caNumberIdents
         ; caAddLazyApplySupport
         ; caWriteGrin True "0-loaded"
         }
    )

-- create HPT info
caAnalyse = task_ VerboseNormal "Analysing"
    ( do { caNormForHPT
         ; caRightSkew
         ; high <- gets csUnique
         ; caHeapPointsTo (3,high-1)
         ; debugging <- gets (optDebug . csOpts)
         ; when debugging (do { ((env, heap),_) <- gets csHptMap
                              ; vm    <- gets csOrigNms
                              ; let newVar i = show (i, getName vm i)
                              ; putDebugMsg "*** Equations ***"
                              ; printArray "env:"  newVar aeMod env
                              ; printArray "heap:" show ahMod heap
                              ; putDebugMsg "*** Abstract Values ***"
                              ; printArray "env:"  newVar aeBaseSet env
                              ; printArray "heap:" show ahBaseSet heap
                              ; caWriteGrin True "0-analyzed"
                              }
                          )
         }
    )
    
-- simplification part I
caKnownCalls = task_ VerboseNormal "Removing unknown calls"
    ( do { caInlineEA
         ; caRightSkew
         ; caWriteGrin True "1-knownCalls"
         }
    )     
-- optionsations part I
caOptimizePartly = task_ VerboseNormal "Optimizing (partly)"
    ( do { caSparseCase
         ; caEliminateCases
         ; caDropUnusedCatch
         ; caDropUnusedBindings
         ; caWriteGrin True "2-partlyOptimized"
         }
    )
-- simplification part II
caNormalize = task_ VerboseNormal "Normalizing" 
    ( do { caLowerGrin
         ; caWriteGrin True "3-normalized"
         }
    )     

-- optionsations part II
caOptimize = task_ VerboseNormal "Optimizing (full)"
    ( do { caCopyPropagation
         ; caDropUnusedExpr
         ; caWriteGrin True "4-optimized"
         }
    )

-- simplification part III
caFinalize = task_ VerboseNormal "Finalizing"
    ( do { caSplitFetch
         ; caDropUnusedExpr
         ; caDropUnusedTags
         ; caNameIdents
         ; caWriteGrin True "5-final"
         }
    )

-- write final code
caOutput = task_ VerboseNormal "Writing code"
    ( do { outputGrin <- gets (optWriteGrin . csOpts)
         ; maybe (return ()) (caWriteGrin False) outputGrin
         ; caWriteCmm
         }
    )

printArray s f g a = harden_ $ do
    { isDebugging <- gets (optDebug . csOpts)
    ; guard isDebugging
    ; putDebugMsg s 
    ; mapM_ (\(k, v) -> putDebugMsg ("  " ++ f k ++ " = " ++ show (g v))) (assocs a)
    }
%%]

% vim:ts=4:et:ai:

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module Main
%%]

%%[1 import(System.Console.GetOpt)
%%]
%%[1.fastseq import(qualified EH.Util.FastSeq as Seq)
%%]
%%[1 import(qualified {%{EH}Config} as Cfg)
%%]
%%[1 import({%{EH}EHC.Common})
%%]
%%[8 import({%{EH}EHC.Environment})
%%]

%%[8 -1.fastseq
%%]

-- HS semantics
%%[1.HSSem import(qualified {%{EH}HS.MainAG} as HSSem)
%%]
-- EH semantics
%%[1.EHSem import(qualified {%{EH}EH.MainAG} as EHSem)
%%]

-- parsing later put in {%{EH}EHC.CompilePhase.Parsers}
%%[1.scannercommon import({%{EH}Scanner.Common})
%%]
%%[1.parsinglib import(UU.Parsing, UU.Parsing.Offside)
%%]
%%[1.parse.EHPrs.HSPrs import(qualified {%{EH}EH.Parser} as EHPrs, qualified {%{EH}HS.Parser} as HSPrs)
%%]

%%[8 -(1.scannercommon 1.parsinglib 1.parse.EHPrs.HSPrs 1.EHSem 1.HSSem)
%%]

-- compiler driver modules
%%[8 import({%{EH}EHC.CompileUnit},{%{EH}EHC.CompileRun})
%%]
%%[8 import({%{EH}EHC.InitialSetup})
%%]
%%[8 import({%{EH}EHC.CompilePhase.TopLevelPhases})
%%]
%%[20 import({%{EH}EHC.CompilePhase.Module})
%%]

-- general imports
%%[8 import(qualified Debug.Trace)
%%]
%%[8 import(qualified Data.Map as Map)
%%]

-- module
%%[20 import({%{EH}Module}(modBuiltin))
%%]

-- Misc
%%[(8 codegen) import({%{EH}Base.Target})
%%]
%%[(102 codegen) import({%{EH}Core.Trf.Strip})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main, compiling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.main
main :: IO ()
main
  =  do  {  args      <- getArgs
         ;  progName  <- getProgName
         ;  let  opts1          = defaultEHCOpts
%%[[8
                                    { ehcOptEnvironment     = defaultEHCEnvironment
%%[[99
                                    , ehcProgName           = p
%%]]
                                    }
%%]]
%%[[99
                                where p = mkFPath progName
%%]]
                 oo@(o,n,errs)  = getOpt Permute ehcCmdLineOpts args
                 opts2          = foldl (flip ($)) opts1 o
         ;  case ehcOptImmQuit opts2 of
              Just immq     -> handleImmQuitOption immq opts2
              _ | null errs ->
%%[[1
                               doCompileRun (if null n then "" else head n) opts2
%%][8
                               unless (null n) (doCompileRun n opts2)
%%][99
                               do { mbEnv <- importEHCEnvironment (mkEhcenvKey (Cfg.verFull Cfg.version) (fpathToStr $ ehcProgName opts2) Cfg.ehcDefaultVariant)
                                  ; let opts3 = maybe opts2 (\e -> opts2 {ehcOptEnvironment = e}) mbEnv
                                  -- ; putStrLn (show mbEnv)
                                  ; unless (null n) (doCompileRun n opts3)
                                  }
%%]]
                | otherwise -> putStr (head errs)
         }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Default EHC Environment
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
defaultEHCEnvironment :: EHCEnvironment
defaultEHCEnvironment
  = EHCEnvironment Cfg.ehcDefaultVariant Cfg.ehcDefaultInplaceInstallDir
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handling of immediate quit options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
handleImmQuitOption :: ImmediateQuitOption -> EHCOpts -> IO ()
handleImmQuitOption immq opts
  = case immq of
      ImmediateQuitOption_Help
        -> do { progName  <- getProgName
%%[[1
              ; putStrLn (usageInfo ("version: " ++ Cfg.verInfo Cfg.version ++ ", aspects: " ++ ehcOptAspects opts
                                    ++ "\n\nUsage: " ++ progName ++ " [options] [file[.eh|.hs]]\n\noptions:"
                                    ) ehcCmdLineOpts)
%%][8
              ; putStrLn (usageInfo (  "version: " ++ Cfg.verInfo Cfg.version ++ ", aspects: " ++ ehcOptAspects opts
                                    ++ "\n\nUsage: " ++ progName
                                    ++ " [options] [file[.eh|.hs] ...]\n\noptions:"
                                    )
                                    ehcCmdLineOpts)
%%[[(8 codegen)
              ; putStrLn ("Transformations:\n" ++ (unlines . map (\(n,t) -> "  " ++ n ++ ": " ++ t) $ cmdLineTrfs))
%%][100
%%]]
%%]]
              }
      ImmediateQuitOption_Version
        -> putStrLn (Cfg.verInfo Cfg.version)
      ImmediateQuitOption_Meta_Variant
        -> putStrLn Cfg.ehcDefaultVariant
%%[[1
      ImmediateQuitOption_Meta_Targets
        -> putStr ""
      ImmediateQuitOption_Meta_TargetDefault
        -> putStr "no-target"
%%][(8 codegen)
      ImmediateQuitOption_Meta_Targets
        -> putStr showSupportedTargets
      ImmediateQuitOption_Meta_TargetDefault
        -> putStr (show defaultTarget)
%%]]
%%[[99
      ImmediateQuitOption_NumericVersion
        -> putStrLn (Cfg.verNumeric Cfg.version)
      ImmediateQuitOption_Meta_ExportEnv mvEnvOpt
        -> exportEHCEnvironment
             (mkEhcenvKey (Cfg.verFull Cfg.version) (fpathToStr $ ehcProgName opts) Cfg.ehcDefaultVariant)
             (env {ehcenvInstallRoot = installRootDir, ehcenvVariant = variant})
        where env = ehcOptEnvironment opts
              (installRootDir,variant)
                = case fmap (wordsBy (`elem` ",;")) mvEnvOpt of
                    Just (d:v:_) -> (d,v)
                    Just (d:_)   -> (d,ehcenvVariant env)
                    _            -> (ehcenvInstallRoot env,ehcenvVariant env)
      ImmediateQuitOption_Meta_DirEnv
        -> do { d <- ehcenvDir (mkEhcenvKey (Cfg.verFull Cfg.version) (fpathToStr $ ehcProgName opts) Cfg.ehcDefaultVariant)
              ; putStrLn d
              }
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Suffix search path
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Order is significant.

%%[8
type FileSuffMp = [(String,EHCompileUnitState)]

fileSuffMpHs :: FileSuffMp
fileSuffMpHs
  = [ ( "hs"  , ECUSHaskell HSStart )
%%[[99
    , ( "lhs" , ECUSHaskell LHSStart )
%%]]
    , ( "eh"  , ECUSEh EHStart )
%%[[20
    , ( "hi"  , ECUSHaskell HIStart )
%%]]
%%[[(8 grin)
    -- currently not supported
    , ( "grin", ECUSGrin )
%%]]
%%[[(94 codegen)
    , ( "c"   , ECUSC CStart )
%%]]
    ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Show sizes, mem usage
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(102 codegen)
showSizeCore :: Core.CModule -> String
showSizeCore x = fevShow "Core" x

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: compilation of module(s)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compiler driver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.doCompile
doCompileRun :: String -> EHCOpts -> IO ()
doCompileRun filename opts
  =  do  {  (fn,fh) <-  if null filename
                        then  return ("<stdin>",stdin)
                        else  do  {  h <- openFile filename ReadMode
                                  ;  return (filename,h)
                                  }
         ;  let isHS = isSuffixOf ".hs" fn
         ;  when
              (ehcStopAtPoint opts >= CompilePoint_Parse)
              (do { tokens <- offsideScanHandle (if isHS then hsScanOpts else ehScanOpts) fn fh
                  ; resd <-
                      if isHS
                      then do { let steps = parseOffside (HSPrs.pAGItf) tokens
                              ; (resd,_) <- evalStepsIO show steps
                              ; if ehcStopAtPoint opts >= CompilePoint_AnalHS
                                then do { let res   = HSSem.sem_AGItf resd
                                              wrRes = HSSem.wrap_AGItf res
                                                        (HSSem.Inh_AGItf
                                                          { HSSem.opts_Inh_AGItf = opts
                                                          })
                                        ; putStrLn (disp (ppErrL $ Seq.toList $ HSSem.errSq_Syn_AGItf $ wrRes) 1000 "")
                                        ; when (ehcOptShowHS opts)
                                               (putStrLn (disp (HSSem.pp_Syn_AGItf wrRes) 1000 ""))
                                        ; return (HSSem.eh_Syn_AGItf wrRes)
                                        }
                                else return undefined
                              }
                      else do { let steps = parseOffside (EHPrs.pAGItf) tokens
                              ; (resd,_) <- evalStepsIO show steps
                              ; return resd
                              }
                  ; when
                      (ehcStopAtPoint opts >= CompilePoint_AnalEH)
                      (do { let res   = EHSem.sem_AGItf resd
                                wrRes = EHSem.wrap_AGItf res (EHSem.Inh_AGItf {EHSem.opts_Inh_AGItf = opts})
                          ; when (ehcOptShowEH opts)
                                 (putStrLn (disp (EHSem.pp_Syn_AGItf wrRes) 70 ""))
                          ; when (ehcOptShowAst opts)
                                 (putStrLn (disp (EHSem.ppAST_Syn_AGItf wrRes) 1000 ""))
%%[[(1 hmtyinfer)
                          ; when (ehcOptShowTopTyPP opts)
                                 (putStr (disp (EHSem.topTyPP_Syn_AGItf wrRes) 1000 ""))
%%]]
%%[[7_2
                          ; when (not (null filename) && ehcOptUniqueness opts)
                                 (writeFile (filename ++ ".html") (EHSem.ppHTML_Syn_AGItf wrRes))
%%]]
                          })
                  })
         }
%%]

%%[8.doCompile -1.doCompile
doCompileRun :: [String] -> EHCOpts -> IO ()
doCompileRun fnL@(fn:_) opts
  = do { let fpL@(fp:_)     = map (mkTopLevelFPath "hs") fnL
             topModNmL@(topModNm:_)
                            = map (mkHNm . fpathBase) fpL
             searchPath     = [emptyFileLoc]
                              ++ ehcOptImportFileLocPath opts
%%[[99
                              ++ [ mkPkgFileLoc p $ filePathUnPrefix
                                   $ Cfg.mkDirbasedLibVariantTargetPkgPrefix (filelocDir d) "" (show (ehcOptTarget opts)) p
                                 | d <- ehcOptLibFileLocPath opts
                                 , p <- ehcOptLibPackages opts
                                 ]
                              ++ [ mkPkgFileLoc p $ filePathUnPrefix
                                   $ Cfg.mkDirbasedTargetVariantPkgPrefix (ehcenvInstallRoot $ ehcOptEnvironment opts) (ehcenvVariant (ehcOptEnvironment opts)) (show (ehcOptTarget opts)) p
                                 | p <- (   ehcOptLibPackages opts
                                         ++ (if ehcOptHideAllPackages opts then [] else Cfg.ehcAssumedPackages)
                                        )
                                 ]
%%]]
             opts2          = opts { ehcOptImportFileLocPath = searchPath }
{- this does not work in ghc 6.8.2
             crsi           = emptyEHCompileRunStateInfo
                                { crsiOpts		 =	 opts2
                                , crsiHSInh      =   initialHSSem opts2
                                , crsiEHInh      =   initialEHSem opts2 fp
%%[[(8 codegen)
                                , crsiCoreInh    =   initialCore2GrSem opts2
%%]]
%%[[20
                                , crsiHIInh      =   initialHISem opts2
                                , crsiHSModInh   =   initialHSSemMod opts2
%%]]
                                }
-}
             crsi           =   (EHCompileRunStateInfo opts2
                                                       uidStart uidStart
                                                       (initialHSSem opts2) (initialEHSem opts2 fp)
%%[[(8 codegen)
                                                       (initialCore2GrSem opts2)
%%]]
%%[[20
                                                       Nothing
                                                       (initialHISem opts2) (initialHSSemMod opts2)
                                                       Map.empty Map.empty defaultOptim
%%]]
%%[[(20 codegen)
                                                       Map.empty
%%]]
%%[[99
                                                       []
%%]]
                                )
             initialState   = mkEmptyCompileRun topModNm crsi
{-
-}
%%[[8
             comp mbFp nm
               = do { mbFoundFp <- cpFindFileForFPath fileSuffMpHs searchPath (Just nm) mbFp
                    ; when (isJust mbFoundFp)
                           (cpEhcModuleCompile1 nm)
                    }
%%][20
             imp :: Maybe FPath -> Maybe (HsName,(FPath,FileLoc)) -> HsName -> EHCompilePhase (HsName,Maybe (HsName,(FPath,FileLoc)))
             imp mbFp mbPrev nm
%%[[20
               = do { fpsFound <- cpFindFilesForFPath False fileSuffMpHs searchPath (Just nm) mbFp
%%][99
               = do { let searchPath' = adaptedSearchPath mbPrev
                    ; fpsFound <- cpFindFilesForFPathInLocations filelocDir const False fileSuffMpHs searchPath' (Just nm) mbFp
%%]]
                    ; when (ehcOptVerbosity opts >= VerboseDebug)
                           (do { lift $ putStrLn $ show nm ++ ": " ++ show (fmap fpathToStr mbFp) ++ ": " ++ show (map fpathToStr fpsFound)
%%[[99
                               ; lift $ putStrLn $ "searchPath: " ++ show searchPath'
%%]]
                               })
                    ; when (isJust mbFp)
                           (cpUpdCU nm (ecuSetIsTopMod True))
                    ; case fpsFound of
                        (fp:_)
                          -> do { nm' <- cpEhcModuleCompile1 (Just HSOnlyImports) nm
                                ; cr <- get
                                ; let (ecu,_,_,_) = crBaseInfo nm' cr
                                ; return (nm',Just (nm',(fp, ecuFileLocation ecu)))
                                }
                        _ -> return (nm,Nothing)
                    }
%%[[99
               where -- strip tail part corresponding to module name, and use it to search as well
                     adaptedSearchPath (Just (prevNm,(prevFp,prevLoc)))
                       = case (fpathMbDir (mkFPath prevNm), fpathMbDir prevFp, prevLoc) of
                           (_, _, p@(FileLoc (FileLocKind_Pkg _) _))
                             -> p : searchPath
                           (Just n, Just p, _)
                             -> mkDirFileLoc (filePathUnPrefix prefix) : searchPath
                             where (prefix,_) = splitAt (length p - length n) p
                           _ -> searchPath
                     adaptedSearchPath _ = searchPath
%%]]
%%]]
       ; when (ehcOptVerbosity opts >= VerboseDebug)
              (putStrLn $ "search path: " ++ show searchPath)
%%[[8
       ; _ <- runStateT (cpSeq [ comp (Just fp) topModNm
                               ]) initialState
%%][20
       ; _ <- runStateT (do { topModNmL' <- zipWithM (\fp topModNm -> imp (Just fp) Nothing topModNm) fpL topModNmL
                            ; cpImportGatherFromMods (imp Nothing) (map fst topModNmL')
                            ; cpCheckMods' [modBuiltin]
                            ; cpEhcCheckAbsenceOfMutRecModules
                            ; cpEhcFullProgCompileAllModules
%%[[100
                            ; unless (ehcOptKeepIntermediateFiles opts2) cpRmFilesToRm
%%]]
                            })
                        initialState
%%]]
       ; return ()
       }

%%]


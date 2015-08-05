%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}EHC.Main}
%%]

%%[1 import(System.Console.GetOpt, System.IO, System.Exit, System.Process, System.Environment)
%%]
%%[8 import(qualified Control.Exception as CE)
%%]
%%[99 import(System.Directory)
%%]
%%[1.fastseq import(qualified UHC.Util.FastSeq as Seq)
%%]
%%[1 import(qualified {%{EH}Config} as Cfg)
%%]
%%[50 import(qualified {%{EH}SourceCodeSig} as Sig)
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
%%[1.EHSem import(qualified {%{EH}EH.Main} as EHSem)
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
%%[50 import({%{EH}EHC.CompilePhase.Module})
%%]

-- general imports
%%[8 import(qualified Debug.Trace)
%%]
%%[8 import(qualified Data.Map as Map, qualified Data.Set as Set)
%%]
%%[8 import(Control.Monad.State, Control.Monad.Error)
%%]
%%[8 import(UHC.Util.Lens)
%%]

-- Build function state
%%[8 import({%{EH}EHC.BuildFunction})
%%]

-- module
%%[50 import({%{EH}Module.ImportExport}(modBuiltin), {%{EH}Module.ImportExport})
%%]

-- packages
%%[99 import({%{EH}Base.PackageDatabase},{%{EH}Base.Parser2})
%%]

-- Misc
%%[8 import({%{EH}Base.Target}, {%{EH}Base.Optimize}(allOptimizeMp))
%%]
%%[(102 codegen) import({%{EH}Core.Trf.Strip})
%%]

-- Config
%%[103 import(qualified {%{EH}ConfigCabal} as Cfg (getDataDir))
%%]

-- Utils
%%[1 import({%{EH}EHC.Main.Utils})
%%]
%%[8 import({%{EH}EHC.Main.Compile})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main, compiling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.main export(mainEHC)
mainEHC :: EHCOpts -> IO ()
mainEHC opts0
  =  do  {  args      <- getArgs
         ;  progName  <- getProgName
%%[[99
         ;  curDir    <- getCurrentDirectory
%%]]
%%[[103
         -- a non-empty data dir means we are running as cabal installed exec
         ;  mbDataDir <- Cfg.getDataDir >>= \d -> return $ if null d then Nothing else Just d
%%]]
         ;  let  opts1          = opts0
%%[[8
                                    { ehcOptEnvironment     = defaultEHCEnvironment
%%[[99
                                    , ehcProgName           = p
                                    , ehcCurDir             = curDir
%%]]
                                    }
%%]]
%%[[99
                                where p = mkFPath progName
%%][101
                                where p = mkFPath "uhc"     -- hardbaked name
%%][103
                                where p = mkFPath "uhcl"     -- hardbaked name
%%]]
%%[[1
                 oo@(o,n,errs)  = ehcCmdLineOptsApply [] args opts1
%%][103
                 oo@(o,n,errs)  = ehcCmdLineOptsApply (maybe [] (\d -> [\o -> o {ehcOptCfgInstallRoot = Just d}]) mbDataDir) args opts1
%%]]
                 opts2          = maybe opts1 id o
%%[[(8 codegen)
         ;  case opts2 of
              o | isNotOk (ehcOptMbTarget       o) -> err $ "non existent target `"        ++ fromNotOk (ehcOptMbTarget       o) ++ "'"
                | isNotOk (ehcOptMbTargetFlavor o) -> err $ "non existent target flavor `" ++ fromNotOk (ehcOptMbTargetFlavor o) ++ "'"
                where err x
                        = do { hPutStrLn stderr ("option error: " ++ x)
                             ; exitFailure
                             }
              _ -> return ()
%%]]
%%[[1
         ;  let opts3 = opts2
%%][99
         ;  userDir <- ehcenvDir (envkey opts2)
         ;  let opts3 = opts2 { ehcOptUserDir = userDir
                              , ehcOptOutputDir =
                                  let outputDir = maybe "." id (ehcOptOutputDir opts2)
                                  in  case ehcOptPkgOpt opts2 of
                                        Just (PkgOption {pkgoptName=s})
                                          -> case parsePkgKey s of
                                               Just k  -> Just $
                                                          outputDir ++ "/" ++
                                                          mkInternalPkgFileBase k (Cfg.installVariant opts2)
                                                                                (ehcOptTarget opts2) (ehcOptTargetFlavor opts2)
                                               _       -> ehcOptOutputDir opts2
                                        _ -> ehcOptOutputDir opts2
                              }
%%]]
         ;  case ehcOptImmQuit opts3 of
              Just immq     -> let
%%[[1
                                   inputSuffixes = ["hs", "eh"]
%%][8
                                   inputSuffixes = catMaybes [ s | (s,_,vis) <- mkFileSuffMpHs opts3, vis ]
%%]]
                               in  handleImmQuitOption ehcCmdLineOpts inputSuffixes immq opts3
              _ | null errs ->
%%[[1
                               doCompileRun (if null n then "" else head n) opts3
%%][8
                               unless (null n) (doCompileRun n opts3)
%%][9999
                               do { mbEnv <- importEHCEnvironment (envkey opts3)
                                  ; let opts4 = opts3 -- maybe opts3 (\e -> opts3 {ehcOptEnvironment = e}) mbEnv
                                  -- ; putStrLn (show mbEnv)
                                  ; unless (null n) (doCompileRun n opts3)
                                  }
%%]]
                | otherwise -> do { putStr (head errs)
                                  ; exitFailure
                                  }
         }
%%[[99
  where envkey opts = mkEhcenvKey (Cfg.verFull Cfg.version) (fpathToStr $ ehcProgName opts) Cfg.ehcDefaultVariant
%%]]
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
%%% Show sizes, mem usage
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(102 codegen)
showSizeCore :: AST_Core -> String
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
              (do { tokens <- offsideScanHandle (if isHS then (hsScanOpts opts) else (ehScanOpts opts)) fn fh
                  ; resd <-
                      if isHS
                      then do { let steps = parseOffside (HSPrs.pAGItf opts) tokens
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
                                else return (panic "EHC.doCompileRun")
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
doCompilePrepare :: [String] -> EHCOpts -> IO (Maybe (EHCOpts,[FPath],[HsName],EHCompileRun))
doCompilePrepare fnL@(fn:_) opts
  = do { let fpL@(fp:_)             = map (mkTopLevelFPath "hs") fnL
             topModNmL@(topModNm:_) = map (mkHNm . fpathBase) fpL
%%[[99
             -- installVariant         = Cfg.installVariant opts
       -- ; installRoot <- Cfg.installRootM opts
       -- ; userDir <- ehcenvDir (Cfg.verFull Cfg.version)
       -- ; let opts2 = opts -- {ehcOptUserDir = userDir}
       ; pkgDb1 <- pkgDbFromDirs opts
                    ({-
                        [ filePathCoalesceSeparator $ filePathUnPrefix
                          $ Cfg.mkDirbasedInstallPrefix (filelocDir d) Cfg.INST_LIB_PKG "" (show (ehcOptTarget opts)) ""
                        | d <- ehcOptPkgdirLocPath opts
                        ]
                     ++ [ filePathUnPrefix
                          $ Cfg.mkDirbasedTargetVariantPkgPrefix installRoot installVariant (show (ehcOptTarget opts)) ""
                        ]
                     -}
                     {-
                     -}
                        [ filePathUnPrefix d
                        | d <- nub $ ehcOptPkgdirLocPath opts ++ [Cfg.mkInstallPkgdirUser opts, Cfg.mkInstallPkgdirSystem opts]
                        ]
                    )
       ; let (pkgDb2,pkgErrs) = pkgDbSelectBySearchFilter (pkgSearchFilter Just PackageSearchFilter_ExposePkg (map tup123to1 $ pkgExposedPackages pkgDb1)
                                                           ++ sort (ehcOptPackageSearchFilter opts)
                                                          ) pkgDb1
             pkgDb3 = pkgDbFreeze pkgDb2
       -- ; putStrLn $ "db1 " ++ show pkgDb1
       -- ; putStrLn $ "db2 " ++ show pkgDb2
       -- ; putStrLn $ "db3 " ++ show pkgDb3
       -- ; putStrLn (show $ ehcOptPackageSearchFilter opts)
%%]]
%%[[99
       ; ehcioinfo <- newEHCIOInfo
%%]]
       ; let searchPath     = [emptyFileLoc]
                              ++ ehcOptImportFileLocPath opts
%%[[99
                              {-
                              ++ [ mkPkgFileLoc (p, Nothing) $ filePathUnPrefix
                                   $ Cfg.mkDirbasedLibVariantTargetPkgPrefix (filelocDir d) "" (show (ehcOptTarget opts)) p
                                 | d <- ehcOptLibFileLocPath opts
                                 , p <- ehcOptLibPackages opts
                                 ]
                              ++ [ mkPkgFileLoc p $ filePathUnPrefix
                                   $ Cfg.mkDirbasedTargetVariantPkgPrefix installRoot installVariant (show (ehcOptTarget opts)) p
                                 | p <- (   ehcOptLibPackages opts
                                         ++ (if ehcOptHideAllPackages opts then [] else Cfg.ehcAssumedPackages)
                                        )
                                 ]
                              -}
                              ++ [fileLocPkgDb]
%%]]
             opts3          = opts { ehcOptImportFileLocPath = searchPath
%%[[99
                                    , ehcOptPkgDb = pkgDb3
%%]]
                                    }
{- this does not work in ghc 6.8.2
             crsi           = emptyEHCompileRunStateInfo
                                { _crsiOpts       =   opts3
                                , _crsiHSInh      =   initialHSSem opts3
                                , _crsiEHInh      =   initialEHSem opts3 fp
%%[[(8 codegen)
                                , _crsiCoreInh    =   initialCore2GrSem opts3
%%]]
%%[[50
                                -- , crsiHIInh      =   initialHISem opts3
                                , crsiHSModInh   =   initialHSSemMod opts3
%%]]
                                }
-}
             crsi           =   (EHCompileRunStateInfo opts3
                                                       uidStart uidStart
                                                       (initialHSSem opts3)
                                                       (initialEHSem opts3 fp)
                                                       (mkFileSuffMpHs opts3)
%%[[(8 codegen)
                                                       (initialCore2GrSem opts3)
%%]]
%%[[(8 corerun)
                                                       initialCore2CoreRunSem
%%]]
%%[[50
                                                       Nothing
                                                       -- (initialHISem opts3)
                                                       (initialHSSemMod opts3)
                                                       Map.empty Map.empty defaultOptim
%%]]
%%[[(50 codegen)
                                                       Map.empty
%%]]
%%[[99
                                                       ehcioinfo []
%%]]
                                                       emptyBState
                                )
             initialState   = mkEmptyCompileRun topModNm crsi
       ; return $ Just (opts3,fpL,topModNmL,initialState)
       }

doCompileRun :: [String] -> EHCOpts -> IO ()
doCompileRun fnL@(fn:_) opts
  = do { mbPrep <- doCompilePrepare fnL opts
       ; if isJust mbPrep
         then do { let ( opts
                        , fpL@(fp:_)
                        , topModNmL@(topModNm:_)
                        , initialState
                        ) = fromJust mbPrep
                       searchPath = ehcOptImportFileLocPath opts
                       fileSuffMpHs = initialState ^. crStateInfo ^. crsiFileSuffMp
                 ; when (ehcOptVerbosity opts >= VerboseDebug)
                        (putStrLn $ "search path: " ++ show searchPath)
%%[[8
                 ; _ <- if ehcOptAltDriver opts
                        then run initialState $ compileN_Alternate [fp] [topModNm]
                        else run initialState $ compile1 opts fileSuffMpHs searchPath (Just fp) topModNm
%%][50
                 ; _ <- if False -- ehcOptPriv opts
                        then run initialState $ compile2 opts fileSuffMpHs searchPath fpL topModNmL
                        else if ehcOptAltDriver opts
                        then run initialState $ compileN_Alternate fpL topModNmL
                        else run initialState $ compileN  opts fileSuffMpHs searchPath fpL topModNmL
%%]]
                 ; return ()
                 }
         else exitFailure
       }
  where -- run s c = {- runErrorT $ -} runStateT (runCompilePhaseT c) s
        run s c = runStateT (runCompilePhaseT c) s
        -- init (to be moved elsewhere, TBD)
        -- initOther fileSuffMpHs = crsiFileSuffMp =: fileSuffMpHs
%%[[8
%%][50
        -- experimental stuff trying to deal with orphan instances, ignore
        -- compile2 :: EHCCompileRunner m => EHCOpts -> FileSuffMp -> FileLocPath -> [FPath] -> [HsName] -> EHCompilePhaseT m ()
        compile2 :: EHCOpts -> FileSuffMp -> FileLocPath -> [FPath] -> [HsName] -> EHCompilePhase ()
        compile2 opts fileSuffMpHs searchPath fpL topModNmL
          = do { 
               -- start with directly importing top modules, providing the filepath directly
                 topModNmL' <- toplayer fpL topModNmL
               ; cpPP "topModNmL'"
               ; oneModNmL <- onelayer
               ; cpPP "oneModNmL"
               ; return ()
               }
          where toplayer fpL topModNmL
                  = zipWithM (\fp topModNm -> import1 opts fileSuffMpHs searchPath (ECUS_Haskell HSOnlyImports) (Just fp) Nothing topModNm) fpL topModNmL
                onelayer
                  = do { cr <- get
                       ; let modNmS = Map.keysSet $ _crCUCache cr
                             ms = Set.unions
                                    [ case cuState e of
                                        -- ECUS_Haskell HIOnlyImports -> ecuTransClosedOrphanModS ecu
                                        _                         -> ecuImpNmS e
                                    | m <- Set.toList modNmS, let e = crCU m cr
                                    ]
                                  `Set.difference` modNmS
                       ; sequence -- or: cpSeq + return ()
                           [ do { i@(m',_) <- import1 opts fileSuffMpHs searchPath (ECUS_Haskell HSOnlyImports) Nothing Nothing m
                                -- ; cpEhcFullProgModuleDetermineNeedsCompile m'
                                ; return i
                                }
                           | m <- Set.toList ms
                           ]
                       }
                
                -- dbg
                {-
                showCompileOrder
                  = do { cr <- get
                       ; liftIO $ putStrLn $ "compile order: " ++ show (_crCompileOrder cr)
                       }
                -}
%%]]

%%]

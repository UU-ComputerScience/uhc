module Main
where
import System.Console.GetOpt
import System.IO
import System.Exit
import System.Process
import System.Environment
import qualified EH101.Config as Cfg
import EH101.EHC.Common
import EH101.EHC.Environment
import EH101.EHC.CompileUnit
import EH101.EHC.CompileRun
import EH101.EHC.InitialSetup
import EH101.EHC.CompilePhase.TopLevelPhases
import qualified Debug.Trace
import qualified Data.Map as Map
import qualified Data.Set as Set
import EH101.Base.Target
import EH101.Base.Optimize (allOptimizeMp)
import EH101.EHC.CompilePhase.Module
import EH101.Module (modBuiltin)
import EH101.Base.PackageDatabase
import EH101.Base.Parser2







{-# LINE 80 "src/ehc/EHC.chs" #-}
main :: IO ()
main
  =  do  {  args      <- getArgs
         ;  progName  <- getProgName
         ;  let  opts1          = defaultEHCOpts
                                    { ehcOptEnvironment     = defaultEHCEnvironment
                                    , ehcProgName           = p
                                    }
                                where p = mkFPath "uhc"     -- hardbaked name
                 oo@(o,n,errs)  = getOpt Permute ehcCmdLineOpts args
                 opts2          = foldl (flip ($)) opts1 o
         ;  case opts2 of
              o | isNotOk (ehcOptMbTarget       o) -> err $ "non existent target `"        ++ fromNotOk (ehcOptMbTarget       o) ++ "'"
                | isNotOk (ehcOptMbTargetFlavor o) -> err $ "non existent target flavor `" ++ fromNotOk (ehcOptMbTargetFlavor o) ++ "'"
                where err x
                        = do { hPutStrLn stderr ("option error: " ++ x)
                             ; exitFailure
                             }
              _ -> return ()
         ;  userDir <- ehcenvDir (envkey opts2)
         ;  let opts3 = opts2 { ehcOptUserDir = userDir
                              , ehcOptOutputDir =
                                  let outputDir = maybe "." id (ehcOptOutputDir opts2)
                                  in  case ehcOptPkg opts2 of
                                        Just (PkgOption_Build s)
                                          -> case parsePkgKey s of
                                               Just k  -> Just $
                                                          outputDir ++ "/" ++
                                                          mkInternalPkgFileBase k (Cfg.installVariant opts2)
                                                                                (ehcOptTarget opts2) (ehcOptTargetFlavor opts2)
                                               _       -> ehcOptOutputDir opts2
                                        _ -> ehcOptOutputDir opts2
                              }
         ;  case ehcOptImmQuit opts3 of
              Just immq     -> handleImmQuitOption immq opts3
              _ | null errs ->
                               unless (null n) (doCompileRun n opts3)
                | otherwise -> do { putStr (head errs)
                                  ; exitFailure
                                  }
         }
  where envkey opts = mkEhcenvKey (Cfg.verFull Cfg.version) (fpathToStr $ ehcProgName opts) Cfg.ehcDefaultVariant

{-# LINE 155 "src/ehc/EHC.chs" #-}
defaultEHCEnvironment :: EHCEnvironment
defaultEHCEnvironment
  = EHCEnvironment Cfg.ehcDefaultVariant Cfg.ehcDefaultInplaceInstallDir

{-# LINE 165 "src/ehc/EHC.chs" #-}
handleImmQuitOption :: ImmediateQuitOption -> EHCOpts -> IO ()
handleImmQuitOption immq opts
  = case immq of
      ImmediateQuitOption_Help
        -> do {
                let progName = fpathToStr (ehcProgName opts)
              ; putStrLn (usageInfo (  "version: " ++ Cfg.verInfo Cfg.version ++ ", aspects: " ++ ehcOptAspects opts
                                    ++ "\n\nUsage: " ++ progName ++ " [options] [file[.eh|.hs] ...]\n\noptions:"
                                    )
                                    ehcCmdLineOpts)
              }
        where
      ImmediateQuitOption_Version
        -> putStrLn (Cfg.verInfo Cfg.version)
      ImmediateQuitOption_Meta_Variant
        -> putStrLn Cfg.ehcDefaultVariant
      ImmediateQuitOption_Meta_Targets
        -> putStr showSupportedTargets
      ImmediateQuitOption_Meta_TargetDefault
        -> putStr (show defaultTarget)
      ImmediateQuitOption_Meta_Optimizations
        -> putStr (showStringMapKeys allOptimizeMp " ")
      ImmediateQuitOption_VersionDotted
        -> putStrLn (Cfg.verFull Cfg.version)
      ImmediateQuitOption_VersionAsNumber
        -> putStrLn (Cfg.verAsNumber Cfg.version)
{-
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
-}
      ImmediateQuitOption_Meta_Pkgdir_System
        -> do { let d = Cfg.mkInstallPkgdirSystem opts
              ; putStrLn d
              }
      ImmediateQuitOption_Meta_Pkgdir_User
        -> do { let d = Cfg.mkInstallPkgdirUser opts
              ; putStrLn d
              }

{-# LINE 241 "src/ehc/EHC.chs" #-}
type FileSuffMp = [(FileSuffix,EHCompileUnitState)]

mkFileSuffMpHs :: EHCOpts -> FileSuffMp
mkFileSuffMpHs opts
  = [ ( Just "hs"  , ECUSHaskell HSStart )
    , ( Just "lhs" , ECUSHaskell LHSStart )
    , ( Just "eh"  , ECUSEh EHStart )
    , ( Just "hi"  , ECUSHaskell HIStart )
    -- currently not supported
    , ( Just "grin", ECUSGrin )
    ]
    ++ (if targetIsOnUnixAndOrC (ehcOptTarget opts) then [ ( Just "c"   , ECUSC CStart ) ] else [])

{-# LINE 264 "src/ehc/EHC.chs" #-}
-- Suffix map for empty suffix, defaults to .hs
fileSuffMpHsNoSuff :: FileSuffMp
fileSuffMpHsNoSuff
  = [ ( Nothing  , ECUSHaskell HSStart )
    ]

{-# LINE 344 "src/ehc/EHC.chs" #-}
doCompilePrepare :: [String] -> EHCOpts -> IO (Maybe (EHCOpts,[FPath],[HsName],EHCompileRun))
doCompilePrepare fnL@(fn:_) opts
  = do { let fpL@(fp:_)             = map (mkTopLevelFPath "hs") fnL
             topModNmL@(topModNm:_) = map (mkHNm . fpathBase) fpL
             installRoot            = Cfg.installRoot    opts
             installVariant         = Cfg.installVariant opts
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
                                                           ++ ehcOptPackageSearchFilter opts
                                                          ) pkgDb1
             pkgDb3 = pkgDbFreeze pkgDb2
       -- ; putStrLn $ "db1 " ++ show pkgDb1
       -- ; putStrLn $ "db2 " ++ show pkgDb2
       -- ; putStrLn $ "db3 " ++ show pkgDb3
       -- ; putStrLn (show $ ehcOptPackageSearchFilter opts)
       ; ehcioinfo <- newEHCIOInfo
       ; let searchPath     = [emptyFileLoc]
                              ++ ehcOptImportFileLocPath opts
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
             opts3          = opts { ehcOptImportFileLocPath = searchPath
                                    , ehcOptPkgDb = pkgDb3
                                    }
{- this does not work in ghc 6.8.2
             crsi           = emptyEHCompileRunStateInfo
                                { crsiOpts       =   opts3
                                , crsiHSInh      =   initialHSSem opts3
                                , crsiEHInh      =   initialEHSem opts3 fp
                                , crsiCoreInh    =   initialCore2GrSem opts3
                                -- , crsiHIInh      =   initialHISem opts3
                                , crsiHSModInh   =   initialHSSemMod opts3
                                }
-}
             crsi           =   (EHCompileRunStateInfo opts3
                                                       uidStart uidStart
                                                       (initialHSSem opts3) (initialEHSem opts3 fp)
                                                       (initialCore2GrSem opts3)
                                                       Nothing
                                                       -- (initialHISem opts3)
                                                       (initialHSSemMod opts3)
                                                       Map.empty Map.empty defaultOptim
                                                       Map.empty
                                                       ehcioinfo []
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
                       fileSuffMpHs = mkFileSuffMpHs opts
                 ; when (ehcOptVerbosity opts >= VerboseDebug)
                        (putStrLn $ "search path: " ++ show searchPath)
                 ; _ <- if False -- ehcOptPriv opts
                        then runStateT (compile2 opts fileSuffMpHs searchPath fpL topModNmL) initialState
                        else runStateT (compile opts fileSuffMpHs searchPath fpL topModNmL) initialState
                 ; return ()
                 }
         else exitFailure
       }
  where compile2 :: EHCOpts -> FileSuffMp -> FileLocPath -> [FPath] -> [HsName] -> EHCompilePhase ()
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
                  = zipWithM (\fp topModNm -> imp1 opts fileSuffMpHs searchPath HSOnlyImports (Just fp) Nothing topModNm) fpL topModNmL
                onelayer
                  = do { cr <- get
                       ; let modNmS = Map.keysSet $ crCUCache cr
                             ms = Set.unions
                                    [ case cuState e of
                                        -- ECUSHaskell HIOnlyImports -> ecuTransClosedOrphanModS ecu
                                        _                         -> ecuImpNmS e
                                    | m <- Set.toList modNmS, let e = crCU m cr
                                    ]
                                  `Set.difference` modNmS
                       ; sequence -- or: cpSeq + return ()
                           [ do { i@(m',_) <- imp1 opts fileSuffMpHs searchPath HSOnlyImports Nothing Nothing m
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
                       ; lift $ putStrLn $ "compile order: " ++ show (crCompileOrder cr)
                       }
                -}

        compile :: EHCOpts -> FileSuffMp -> FileLocPath -> [FPath] -> [HsName] -> EHCompilePhase ()
        compile opts fileSuffMpHs searchPath fpL topModNmL
          = do {
               -- start with directly importing top modules, providing the filepath directly
                 topModNmL' <- zipWithM (\fp topModNm -> imp HSOnlyImports (Just fp) Nothing topModNm) fpL topModNmL

               -- follow the import relation to chase modules which have to be analysed
               ; cpImportGatherFromModsWithImp
                   (if ehcOptPriv opts
                    then \ecu -> case ecuState ecu of
                                   -- ECUSHaskell HIStart -> Set.toList $ ecuTransClosedOrphanModS ecu
                                   ECUSHaskell HIOnlyImports -> [] -- Set.toList $ ecuTransClosedOrphanModS ecu
                                   _ -> ecuImpNmL ecu
                    else ecuImpNmL
                   )
                   (imp HSOnlyImports Nothing) (map fst topModNmL')

               -- import orphans
               ; when (ehcOptPriv opts)
                      (do {
                          -- import orphans
                            importAlso HSOnlyImports ecuTransClosedOrphanModS

                          -- import used remaining modules, but just minimally
                          ; importAlso HMOnlyMinimal (Set.unions . Map.elems . ecuTransClosedUsedModMp)
                          })

               -- check module import relationship
               ; cpCheckMods' [modBuiltin]

               -- inhibit mutual recursiveness
               ; cpEhcCheckAbsenceOfMutRecModules

               -- and compile it all
               ; cpEhcFullProgCompileAllModules
               -- cleanup
               ; unless (ehcOptKeepIntermediateFiles opts) cpRmFilesToRm
               }
          where -- abbrev for imp1
                imp = imp1 opts fileSuffMpHs searchPath

                -- import others, but then in a (slightly) different way
                importAlso :: HSState -> (EHCompileUnit -> Set.Set HsName) -> EHCompilePhase ()
                importAlso how getNms
                  = do { cr <- get
                       ; let allAnalysedModS = Map.keysSet $ crCUCache cr
                             allNewS         = Set.unions [ getNms $ crCU m cr | m <- Set.toList allAnalysedModS ] `Set.difference` allAnalysedModS
                       ; cpImportGatherFromModsWithImp
                           (const [])
                           (imp how Nothing) (Set.toList allNewS)
                       }

        imp1 :: EHCOpts -> FileSuffMp -> FileLocPath -> HSState -> Maybe FPath -> Maybe (HsName,(FPath,FileLoc)) -> HsName -> EHCompilePhase (HsName,Maybe (HsName,(FPath,FileLoc)))
        imp1 opts fileSuffMpHs searchPath desiredState mbFp mbPrev nm
          = do { let isTopModule = isJust mbFp
                     fileSuffMpHs' = (if isTopModule then fileSuffMpHsNoSuff else []) ++ fileSuffMpHs
               ; let searchPath' = adaptedSearchPath mbPrev
               ; fpsFound <- cpFindFilesForFPathInLocations (fileLocSearch opts) (\(x,_,_) -> x) False fileSuffMpHs' searchPath' (Just nm) mbFp
               ; when (ehcOptVerbosity opts >= VerboseDebug)
                      (do { lift $ putStrLn $ show nm ++ ": " ++ show (fmap fpathToStr mbFp) ++ ": " ++ show (map fpathToStr fpsFound)
                          ; lift $ putStrLn $ "searchPath: " ++ show searchPath'
                          })
               ; when isTopModule
                      (cpUpdCU nm (ecuSetIsTopMod True))
               -- ; cpUpdCU nm (ecuSetIsTopMod isTopModule)      -- ???? not equivalent to above
               ; cpUpdCU nm (ecuSetTarget (ehcOptTarget opts))
               ; case fpsFound of
                   (fp:_)
                     -> do { nm' <- cpEhcModuleCompile1 (Just desiredState) nm
                           ; cr <- get
                           ; let (ecu,_,_,_) = crBaseInfo nm' cr
                           ; return (nm',Just (nm',(fp, ecuFileLocation ecu)))
                           }
                   _ -> return (nm,Nothing)
               }
          where -- strip tail part corresponding to module name, and use it to search as well
                adaptedSearchPath (Just (prevNm,(prevFp,prevLoc)))
                  = case (fpathMbDir (mkFPath prevNm), fpathMbDir prevFp, prevLoc) of
                      (_, _, p) | filelocIsPkg p
                        -> p : searchPath
                      (Just n, Just p, _)
                        -> mkDirFileLoc (filePathUnPrefix prefix) : searchPath
                        where (prefix,_) = splitAt (length p - length n) p
                      _ -> searchPath
                adaptedSearchPath _ = searchPath


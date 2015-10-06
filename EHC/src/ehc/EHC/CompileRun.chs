%%[0 hs
{-# LANGUAGE TemplateHaskell #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile Run
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

An EHC compile run maintains info for one compilation invocation

%%[8 module {%{EH}EHC.CompileRun}
%%]

-- general imports
%%[8 import(qualified Data.Map as Map,qualified Data.Set as Set)
%%]
%%[8888 import(System.IO, System.Exit, System.Environment, System.Process)
%%]
%%[99 import(UHC.Util.Time, System.CPUTime, System.Locale, Data.IORef, System.IO.Unsafe)
%%]
%%[99 import(System.Directory)
%%]
%%[8 import(Control.Monad.State hiding (get), qualified Control.Monad.State as MS)
%%]
%%[8888 import(UHC.Util.Error, Control.Monad.Fix)
%%]
%%[8888 import(Control.Exception as CE)
%%]
%%[8 import(UHC.Util.Lens)
%%]
%%[99 import(UHC.Util.FPath)
%%]
%%[99 import({%{EH}Base.PackageDatabase})
%%]
%%[50 import(qualified {%{EH}Config} as Cfg)
%%]

%%[(8 codegen) hs import({%{EH}CodeGen.ValAccess} as VA)
%%]

%%[8 import({%{EH}EHC.Common}, {%{EH}EHC.FileSuffMp})
%%]
%%[8 import({%{EH}EHC.CompileUnit})
%%]
%%[50 import({%{EH}EHC.CompileGroup})
%%]
-- State, also build function state
%%[8 import({%{EH}EHC.CompileRun.Base}) export(module {%{EH}EHC.CompileRun.Base})
%%]
-- Language syntax: Core
%%[(8 codegen) import( qualified {%{EH}Core} as Core)
%%]
-- Language syntax: CoreRun
%%[(8 corerun) import( qualified {%{EH}CoreRun} as CoreRun)
%%]
-- Language syntax: TyCore
%%[(8 codegen tycore) import(qualified {%{EH}TyCore} as C)
%%]
-- Language semantics: HS, EH
%%[8 import(qualified {%{EH}EH.Main} as EHSem, qualified {%{EH}HS.MainAG} as HSSem)
%%]
-- Language semantics: Core
%%[(8 core) import(qualified {%{EH}Core.ToGrin} as Core2GrSem)
%%]

-- HI Syntax and semantics, HS module semantics
%%[5050 import(qualified {%{EH}HI} as HI)
%%]
%%[50 import(qualified {%{EH}HS.ModImpExp} as HSSemMod)
%%]
-- module admin
%%[50 import({%{EH}Module.ImportExport})
%%]

-- build call
%%[8888 import({%{EH}EHC.BuildFunction.Run})
%%]

-- Misc
%%[102 import({%{EH}Debug.HighWaterMark})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Time
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(newEHCIOInfo)
newEHCIOInfo :: IO (IORef EHCIOInfo)
newEHCIOInfo
  = do t <- getEHCTime
       newIORef (EHCIOInfo t t)
%%]

%%[99
%%]

%%[9999 export(unsafeTimedPerformIO)
-- | Unsafe do IO with timing info
unsafeTimedPerformIO
  :: IORef EHCIOInfo
     -> IO a
     -> ( a
        , EHCTime				-- last time being called
        , EHCTimeDiff			-- delta since that time
        )
unsafeTimedPerformIO inforef io
  = unsafePerformIO $
    do info <- readIORef inforef
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Update: state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Update: options, additional exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(cpUpdOpts)
cpUpdOpts :: EHCCompileRunner m => (EHCOpts -> EHCOpts) -> EHCompilePhaseT m ()
cpUpdOpts upd
  = cpUpdSI $ crsiOpts ^$= upd -- (\crsi -> crsi {crsiOpts = upd $ crsiOpts crsi})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: stop at phase
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(cpStopAt)
cpStopAt :: EHCCompileRunner m => CompilePoint -> EHCompilePhaseT m ()
cpStopAt atPhase
  = do { cr <- MS.get
       ; let (_,opts) = crBaseInfo' cr
       ; unless (atPhase < ehcStopAtPoint opts)
                cpSetStopAllSeq
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Partition imports into newer + older
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
crPartitionNewerOlderImports :: HsName -> EHCompileRun m -> ([EHCompileUnit],[EHCompileUnit])
crPartitionNewerOlderImports modNm cr
  = partition isNewer $ map (flip crCU cr) $ ecuImpNmL ecu
  where ecu = crCU modNm cr
        t   = panicJust "crPartitionNewerOlderImports1" $ _ecuMbHIInfoTime ecu
        isNewer ecu'
            | isJust mbt = t' `diffClockTimes` t > noTimeDiff
            | otherwise  = False
            where t' = panicJust "crPartitionNewerOlderImports2" $ _ecuMbHIInfoTime ecu'
                  mbt = _ecuMbHIInfoTime ecu'
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Partition imports into main and non main module names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(crPartitionMainAndImported)
-- | Partition modules into main and non main (i.e. imported) module names
crPartitionMainAndImported :: EHCompileRun m -> [HsName] -> ([HsName], [HsName])
crPartitionMainAndImported cr modNmL = partition (\n -> ecuHasMain $ crCU n cr) modNmL
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module needs recompilation?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(crModNeedsCompile)
crModNeedsCompile :: HsName -> EHCompileRun m -> Bool
crModNeedsCompile modNm cr
  = ecuIsMainMod ecu -- ecuIsTopMod ecu
    || not (  ehcOptCheckRecompile opts
           && ecuCanUseHIInsteadOfHS ecu
           && null newer
           )
  where ecu = crCU modNm cr
        (newer,_) = crPartitionNewerOlderImports modNm cr
        opts = _crStateInfo cr ^. crsiOpts
%%]
    || (ehcOptCheckVersion opts
        && (  not (ecuCanUseHIInsteadOfHS ecu)
           || not (null newer)
       )   )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compilation can actually be done?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[5050 export(crEcuCanCompile)
crEcuCanCompile :: HsName -> EHCompileRun m -> Bool
crEcuCanCompile modNm cr
  = isJust (_ecuMbSrcTime ecu) && _ecuDirIsWritable ecu
  where ecu = crCU modNm cr
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Set 'main'-ness of module, checking whethere there are not too many modules having a main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(crSetAndCheckMain)
crSetAndCheckMain :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
crSetAndCheckMain modNm
  = do { cr <- MS.get
       ; let (crsi,opts) = crBaseInfo' cr
             mkerr lim ns = cpSetLimitErrs 1 "compilation run" [rngLift emptyRange Err_MayOnlyHaveNrMain lim ns modNm]
       ; case crsiMbMainNm crsi of
           Just n | n /= modNm          -> mkerr 1 [n]
           _ | ehcOptDoExecLinking opts -> cpUpdSI (\crsi -> crsi {crsiMbMainNm = Just modNm})
             | otherwise                -> return ()
                                           -- mkerr 0 []
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Meta info extraction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(cpGetMetaInfo)
-- | Extract various pieces of meta info (such as timestamps) of files needed further in the compilation process
cpGetMetaInfo :: EHCCompileRunner m => [GetMeta] -> HsName -> EHCompilePhaseT m ()
cpGetMetaInfo gm modNm
  =  do  {  cr <- MS.get
         ;  let (ecu,_,opts,fp) = crBaseInfo modNm cr
         ;  when (GetMeta_Src `elem` gm) $
                 tm opts ecu ecuStoreSrcTime        (ecuSrcFilePath ecu)
                 -- void $ bcall $ ModfTimeOfFile (mkPrevFileSearchKeyWithName modNm) ASTType_HS (_ecuASTFileContent ecu, ASTFileUse_Src) ASTFileTiming_Current
                 
         ;  when (GetMeta_HI `elem` gm)
                 (tm opts ecu ecuStoreHIInfoTime
%%[[50
                                              (fpathSetSuff "hi"        fp     )
%%][99
                                              (mkInOrOutputFPathFor (InputFrom_Loc $ ecuFileLocation ecu) opts modNm fp "hi")
%%]]
                 )
%%[[(50 codegen grin)
         ;  when (GetMeta_Grin `elem` gm)
                 (tm opts ecu ecuStoreGrinTime      (fpathSetSuff "grin"      fp     ))
%%]]
%%[[(50 codegen)
         ;  when (GetMeta_Core `elem` gm) $
                 tm opts ecu ecuStoreCoreTime      (fpathSetSuff Cfg.suffixDotlessBinaryCore fp)
                 -- dfltPrev ASTType_Core modNm ecu
%%]]
%%[[(50 corerun)
         ;  when (GetMeta_CoreRun `elem` gm) $
                 tm opts ecu ecuStoreCoreRunTime   (fpathSetSuff Cfg.suffixDotlessBinaryCoreRun fp)
                 -- dfltPrev ASTType_CoreRun modNm ecu
%%]]
%%[[50
         ;  when (GetMeta_Dir `elem` gm) $
                 wr opts ecu ecuStoreDirIsWritable fp
                 -- void $ bcall $ DirOfModIsWriteable modNm
%%]]
         }
  where -- dfltPrev astty modNm ecu = void $ bcall $ ModfTimeOfFile (mkPrevFileSearchKeyWithName modNm) astty (ASTFileContent_Binary, ASTFileUse_Cache) ASTFileTiming_Prev

        tm :: EHCCompileRunner m => EHCOpts -> EHCompileUnit -> (ClockTime -> EHCompileUnit -> EHCompileUnit) -> FPath -> EHCompilePhaseT m ()
        tm opts ecu store fp
          = do { let n = fpathToStr fp
               ; nExists <- liftIO $ doesFileExist n
               ; when (ehcOptVerbosity opts >= VerboseDebug)
                      (do { liftIO $ putStrLn ("meta info of: " ++ show (ecuModNm ecu) ++ ", file: " ++ n ++ ", exists: " ++ show nExists)
                          })
               ; when nExists
                      (do { t <- liftIO $ fpathGetModificationTime fp
                          ; when (ehcOptVerbosity opts >= VerboseDebug)
                                 (do { liftIO $ putStrLn ("time stamp of: " ++ show (ecuModNm ecu) ++ ", time: " ++ show t)
                                     })
                          ; cpUpdCU modNm $ store t
                          })
               }
%%[[50
        wr opts ecu store fp
          = do { pm <- liftIO $ getPermissions (maybe "." id $ fpathMbDir fp)
               -- ; liftIO $ putStrLn (fpathToStr fp ++ " writ " ++ show (writable pm))
               ; cpUpdCU modNm $ store (writable pm)
               }
%%]]
%%]



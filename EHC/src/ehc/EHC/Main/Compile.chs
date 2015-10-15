%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main compile stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{EH}EHC.Main.Compile}
%%]

%%[8 import(System.Console.GetOpt, System.IO, System.Exit, System.Process, System.Environment)
%%]
%%[8 import(qualified Control.Exception as CE)
%%]
%%[99 import(System.Directory)
%%]
%%[8888 import(qualified {%{EH}Config} as Cfg)
%%]
%%[5050 import(qualified {%{EH}SourceCodeSig} as Sig)
%%]
%%[8 import({%{EH}EHC.Common})
%%]
%%[8 import({%{EH}EHC.Environment})
%%]
%%[8 import (UHC.Util.Lens)
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

-- alternate driver
%%[8 import({%{EH}EHC.BuildFunction.Run})
%%]

-- general imports
%%[8 import(qualified Debug.Trace)
%%]
%%[8 import(qualified Data.Map as Map, qualified Data.Set as Set)
%%]
%%[8 import(Control.Monad.State, UHC.Util.Error)
%%]

-- module
%%[50 import({%{EH}Module.ImportExport}(modBuiltin), {%{EH}Module.ImportExport})
%%]

-- packages
%%[99 import({%{EH}Base.PackageDatabase},{%{EH}Base.Parser2})
%%]

-- CoreRun running
%%[(8 corerun) import({%{EH}EHC.CompilePhase.Run})
%%]

-- Misc
%%[8 import({%{EH}Base.Target}, {%{EH}Base.Optimize}(allOptimizeMp))
%%]
%%[(102 codegen) import({%{EH}Core.Trf.Strip})
%%]

-- Config
%%[103103 import(qualified {%{EH}ConfigCabal} as Cfg (getDataDir))
%%]

-- Utils
%%[8 import({%{EH}EHC.Main.Utils})
%%]

-- debugging
%%[8 import({%{EH}Base.Trace})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compiler driver reusable fragments for Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8.compile1 export(compile1)
compile1 :: EHCOpts -> FileSuffMp -> FileLocPath -> Maybe FPath -> HsName -> EHCompilePhase ()
compile1 opts fileSuffMpHs searchPath mbFp nm
  = do { mbFoundFp <- cpFindFileForFPath (map tup123to12 fileSuffMpHs) searchPath (Just nm) mbFp
       ; when (isJust mbFoundFp)
              (cpEhcModuleCompile1 nm)
       }
%%]

%%[8 export(compileN_Alternate)
-- | Alternate compiler driver using (20150806 under construction) new build system
compileN_Alternate :: EHCCompileRunner m => [FPath] -> [HsName] -> EHCompilePhaseT m ()
compileN_Alternate fpL topModNmL@(modNm:_) = do
    cpMsg modNm VerboseDebug $ "compileN_Alternate topModNmL: " ++ show topModNmL
    crsi <- bcall $ CRSI
    let opts    = crsi ^. crsiOpts
        astpipe = astpipeForEHCOpts opts
        bglob   = BuildGlobal astpipe
    case (ehcOptTarget opts, fpL, topModNmL) of
%%[[(8 corerun)
{-
      (Target_None_Core_AsIs, (fp:_), (modNm:_)) | CoreOpt_Run `elem` ehcOptCoreOpts opts -> do
          (_ :: AST_Core) <- bcall $ ASTP ((modNm, ASTFileNameOverride_FPathAsTop fp), Nothing) astpipe
          -- cr <- get
          -- cpTr TraceOn_BldResult ["compileN_Alternate", show $ Map.keys $ cr ^. crCUCache, show $ cr ^. crNmForward]
          (bcall $ ActualModNm modNm) >>= cpRunCoreRun
-}
      (Target_None_Core_AsIs, (fp:_), (modNm:_)) | CoreOpt_Run `elem` ehcOptCoreOpts opts -> do
          let modSearchKey = PrevFileSearchKey (FileSearchKey modNm $ ASTFileNameOverride_FPathAsTop fp) Nothing
          maybeM (bcall $ BuildPlanPMb bglob modSearchKey astpipe) (return ()) $ \astplan -> do
            -- cpRunCoreRun4 bglob modSearchKey astplan
            cpRunCoreRun5 bglob modSearchKey astplan
%%]]
      (_, fpL, topModNmL) -> do
          zipWithM (\fp topModNm -> bcall $ EcuOfPrevNameAndPath (PrevFileSearchKey (FileSearchKey topModNm $ ASTFileNameOverride_FPathAsTop fp) Nothing)) fpL topModNmL
          return ()
%%]

%%[50 -8.compile1 export(compileN)
compileN :: EHCCompileRunner m => EHCOpts -> FileSuffMp -> FileLocPath -> [FPath] -> [HsName] -> EHCompilePhaseT m ()
compileN opts fileSuffMpHs searchPath fpL topModNmL@(modNm:_)
  = do { cpMsg modNm VerboseDebug $ "compileN topModNmL: " ++ show topModNmL

       -- check module import relationship for builtin module
       ; cpCheckModsModWith (const emptyModMpInfo) [modBuiltin]
       
       -- start with directly importing top modules, providing the filepath directly
       ; topModNmL' <- zipWithM (\fp topModNm -> imp (ECUS_Haskell HSOnlyImports) (Just fp) Nothing topModNm) fpL topModNmL
       
       -- follow the import relation to chase modules which have to be analysed
       ; cpImportGatherFromModsWithImp
           ({- if ehcOptPriv opts
            then \ecu -> case ecuState ecu of
                           -- ECUS_Haskell HIStart -> Set.toList $ ecuTransClosedOrphanModS ecu
                           ECUS_Haskell HIOnlyImports -> [] -- Set.toList $ ecuTransClosedOrphanModS ecu
                           _ -> ecuImpNmL ecu
            else -} ecuImpNmL
           )
           (imp (ECUS_Haskell HSOnlyImports) Nothing)
           (map fst topModNmL')
       
       -- import orphans
       {-
       ; when (ehcOptPriv opts)
              (do { 
                  -- import orphans
                    importAlso (ECUS_Haskell HSOnlyImports) ecuTransClosedOrphanModS
                  
                  -- import used remaining modules, but just minimally                          
                  ; importAlso (ECUS_Haskell HMOnlyMinimal) (Set.unions . Map.elems . ecuTransClosedUsedModMp)
                  })
       -}
       
       -- inhibit mutual recursiveness
       ; cpEhcCheckAbsenceOfMutRecModules
       
       -- and compile it all
       ; cpEhcFullProgCompileAllModules
%%[[100
       -- cleanup
       ; unless (ehcOptKeepIntermediateFiles opts) cpRmFilesToRm
%%]]
       }
  where -- abbrev for import1
        imp = import1 opts fileSuffMpHs searchPath
        
        -- import others, but then in a (slightly) different way
        {-
        importAlso how getNms
          = do { cr <- get
               ; let allAnalysedModS = Map.keysSet $ _crCUCache cr
                     allNewS         = Set.unions [ getNms $ crCU m cr | m <- Set.toList allAnalysedModS ] `Set.difference` allAnalysedModS
               ; cpImportGatherFromModsWithImp
                   (const [])
                   (imp how Nothing) (Set.toList allNewS)
               }
        -}
%%]


%%[50 export(import1)
import1
  :: EHCCompileRunner m
  => EHCOpts
     -> FileSuffMp
     -> FileLocPath
     -> EHCompileUnitState
     -> Maybe FPath
     -> Maybe PrevSearchInfo
     -> HsName
     -> EHCompilePhaseT m (HsName,Maybe PrevSearchInfo)
import1 opts fileSuffMpHs searchPath desiredState mbFp mbPrev nm
  = do { let isTopModule = isJust mbFp
             fileSuffMpHs' = map tup123to12 $ (if isTopModule then fileSuffMpHsNoSuff else []) ++ fileSuffMpHs
%%[[50
       ; fpsFound <- cpFindFilesForFPath False fileSuffMpHs' searchPath (Just nm) mbFp
%%][99
       ; let searchPath' = prevSearchInfoAdaptedSearchPath mbPrev searchPath
       ; fpsFound <- cpFindFilesForFPathInLocations (fileLocSearch opts) tup1234to1 False fileSuffMpHs' searchPath' (Just nm) mbFp
%%]]
       ; when (ehcOptVerbosity opts >= VerboseDebug)
              (do { liftIO $ putStrLn $ show nm ++ ": " ++ show (fmap fpathToStr mbFp) ++ ": " ++ show (map fpathToStr fpsFound)
%%[[99
                  ; liftIO $ putStrLn $ "searchPath: " ++ show searchPath'
%%]]
                  })
       ; when isTopModule
              (cpUpdCU nm (ecuSetIsTopMod True))
%%[[99
       ; cpUpdCU nm (ecuSetTarget (ehcOptTarget opts))
%%]]
       ; case fpsFound of
           (fp:_)
             -> do { nm' <- cpEhcModuleCompile1 (Just desiredState) nm
                   ; cr <- get
                   ; let (ecu,_,_,_) = crBaseInfo nm' cr
                         newPrev = Just (nm',(fp, ecuFileLocation ecu))
                   ; cpUpdCU nm' $ ecuMbPrevSearchInfo ^= newPrev
                   ; return (nm', newPrev)
                   }
           _ -> return (nm,Nothing)
       }
%%]



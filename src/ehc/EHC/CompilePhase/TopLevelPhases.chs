%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 haddock
Top level combinations, stratified into 6 levels,
where higher numbered levels use lower numbered levels.

Levels:
1: processing building blocks
2: ehc compilation phases, including progress messages, stopping when asked for
3: ehc grouping of compilation phases for a single module
4: single module compilation
5: full program phases
6: full program compilation

Naming convention for functions:
level 1    : with prefix 'cpProcess'
level 2..6 : with prefix 'cpEhc'
%%]

%%[8 module {%{EH}EHC.CompilePhase.TopLevelPhases}
%%]

-- general imports
%%[8 import(qualified Data.Map as Map,qualified Data.Set as Set)
%%]

%%[8 import({%{EH}Base.Optimize})
%%]
%%[(8 codegen) import({%{EH}Base.Target})
%%]

%%[8 import({%{EH}EHC.Common})
%%]
%%[8 import({%{EH}EHC.CompileUnit})
%%]
%%[8 import({%{EH}EHC.CompileRun})
%%]
%%[50 import({%{EH}EHC.CompileGroup})
%%]

%%[8 import({%{EH}EHC.CompilePhase.Parsers})
%%]
%%[8 import({%{EH}EHC.CompilePhase.Translations})
%%]
%%[8 import({%{EH}EHC.CompilePhase.Output})
%%]
%%[8 import({%{EH}EHC.CompilePhase.Transformations},{%{EH}EHC.CompilePhase.TransformGrin})
%%]
%%[8 import({%{EH}EHC.CompilePhase.Semantics})
%%]
%%[8 import({%{EH}EHC.CompilePhase.FlowBetweenPhase})
%%]
%%[8 import({%{EH}EHC.CompilePhase.CompileC})
%%]
%%[(8 codegen grin) import({%{EH}EHC.CompilePhase.CompileLLVM})
%%]
%%[(8 codegen java) import({%{EH}EHC.CompilePhase.CompileJVM})
%%]
%%[(8 codegen jscript) import({%{EH}EHC.CompilePhase.CompileJScript})
%%]
%%[99 import({%{EH}Base.PackageDatabase})
%%]
%%[(99 codegen) import({%{EH}EHC.CompilePhase.Link})
%%]
%%[50 import({%{EH}EHC.CompilePhase.Module})
%%]
%%[99 import({%{EH}Base.Pragma})
%%]
%%[99 import({%{EH}EHC.CompilePhase.Cleanup})
%%]

-- Language syntax: Core
%%[(50 codegen) import(qualified {%{EH}Core} as Core(cModMerge))
%%]
%%[(50 codegen) import({%{EH}Core.Utils} (cModMerge2))
%%]
-- Language syntax: TyCore
%%[(8 codegen tycore) import(qualified {%{EH}TyCore.Full2} as C)
%%]
-- Language syntax: Grin
%%[(50 codegen grin) import(qualified {%{EH}GrinCode} as Grin(grModMerge))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Per full program compile actions: level 6
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 codegen grin) haddock
Top level entry point into compilation by the compiler driver, apart from import analysis, wich is assumed to be done.
%%]

%%[50
-- | top lever driver for after all per-module work has been done, and whole program stuff like combining/linking can start
cpEhcFullProgLinkAllModules :: [HsName] -> EHCompilePhase ()
cpEhcFullProgLinkAllModules modNmL
 = do { cr <- get
      ; let (mainModNmL,impModNmL) = splitMain cr modNmL
            (_,opts) = crBaseInfo' cr   -- '
      ; when (not $ null modNmL)
             (cpMsg (head modNmL) VerboseDebug ("Main mod split: " ++ show mainModNmL ++ ": " ++ show impModNmL))
      ; case mainModNmL of
          [mainModNm]
            | ehcOptDoLinking opts
                -> case () of
                     () | ehcOptOptimizationScope opts >= OptimizationScope_WholeCore
                            -> cpSeq (  hpt
                                     ++ [ when (targetIsGrinBytecode (ehcOptTarget opts)) (cpProcessBytecode mainModNm)
%%[[99
                                        , cpCleanupGrin [mainModNm]
%%]]
                                        ]

                                     ++ exec
                                     )
                        | targetDoesHPTAnalysis (ehcOptTarget opts)
                            -> cpSeq $ hpt ++ exec
                        | otherwise
                            -> cpSeq exec
                        where exec = [ cpEhcExecutablePerModule FinalCompile_Exec impModNmL mainModNm ]
                              hpt  = [ cpEhcFullProgPostModulePhases opts modNmL (impModNmL,mainModNm)
                                     , cpEhcCorePerModulePart2 mainModNm
                                     ]
            | otherwise
                -> cpSetLimitErrs 1 "compilation run" [rngLift emptyRange Err_MayNotHaveMain mainModNm]
          _ | ehcOptDoLinking opts
                -> cpSetLimitErrs 1 "compilation run" [rngLift emptyRange Err_MustHaveMain]
            | otherwise
%%[[50
                -> return ()
%%][99
                -> case ehcOptPkg opts of
                     Just (PkgOption_Build pkg)
                       | targetAllowsOLinking (ehcOptTarget opts)
                         -> cpLinkO impModNmL pkg
%%[[(99 jazy)
                       | targetAllowsJarLinking (ehcOptTarget opts)
                         -> cpLinkJar Nothing impModNmL (JarMk_Pkg pkg)
%%]]
                     _ -> return ()
%%]]
      }
  where splitMain cr = partition (\n -> ecuHasMain $ crCU n cr)
%%]

%%[50 export(cpEhcCheckAbsenceOfMutRecModules)
cpEhcCheckAbsenceOfMutRecModules :: EHCompilePhase ()
cpEhcCheckAbsenceOfMutRecModules
 = do { cr <- get
      ; let mutRecL = filter (\ml -> length ml > 1) $ crCompileOrder cr
      ; when (not $ null mutRecL)
             (cpSetLimitErrs 1 "compilation run" [rngLift emptyRange Err_MutRecModules mutRecL]
             )
      }
%%]

%%[50 export(cpEhcFullProgCompileAllModules)
cpEhcFullProgCompileAllModules :: EHCompilePhase ()
cpEhcFullProgCompileAllModules
 = do { cr <- get
      ; let modNmLL = crCompileOrder cr
            modNmL = map head modNmLL
      ; cpSeq (   []
%%[[99
               ++ (let modNmL2 = filter (\m -> let (ecu,_,_,_) = crBaseInfo m cr in not $ filelocIsPkg $ ecuFileLocation ecu) modNmL
                       nrMods = length modNmL2
                   in  zipWith (\m i -> cpUpdCU m (ecuStoreSeqNr (EHCCompileSeqNr i nrMods)) ) modNmL2 [1..nrMods]
                  )
%%]]
               ++ [cpEhcFullProgModuleCompileN modNmL]
%%[[(50 codegen grin)
               ++ [cpEhcFullProgLinkAllModules modNmL]
%%]]
              )
      }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Per full program compile actions: level 5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 codegen grin) haddock
Post per module phases, as part of a full program compilation.
Post processing involves the following:
1. if doing full program analysis, merge the already compiled Core/Grin representations, and do the same work as for 1 module but now for the merged Core/Grin
2. compile+link everything together
%%]

%%[(50 codegen grin)
cpEhcFullProgPostModulePhases, cpEhcGrinFullProgPostModulePhases, cpEhcCoreFullProgPostModulePhases
  :: EHCOpts -> [HsName] -> ([HsName],HsName) -> EHCompilePhase ()

cpEhcFullProgPostModulePhases opts modNmL modSpl
  = (if  ehcOptOptimizationScope opts >= OptimizationScope_WholeCore
    then cpEhcCoreFullProgPostModulePhases
    else cpEhcGrinFullProgPostModulePhases
    ) opts modNmL modSpl

cpEhcGrinFullProgPostModulePhases opts modNmL (impModNmL,mainModNm)
  = cpSeq ([ cpSeq [cpEnsureGrin m | m <- modNmL]
           , mergeIntoOneBigGrin
%%[[99
           , cpCleanupGrin impModNmL -- clean up unused Grin (moved here from cpCleanupCU)
%%]]
           ]
           ++ (if ehcOptDumpGrinStages opts then [cpOutputGrin False "-fullgrin" mainModNm] else [])
           ++ [ cpMsg mainModNm VerboseDebug ("Full Grin generated, from: " ++ show impModNmL)
              ]
          )
  where mergeIntoOneBigGrin
          = do { cr <- get
               ; cpUpdCU mainModNm (ecuStoreGrin (Grin.grModMerge [ panicJust "cpEhcGrinFullProgPostModulePhases.mergeIntoOneBigGrin" $ ecuMbGrin $ crCU m cr
                                                                  | m <- modNmL
                                                                  ]
                                   )             )
               }

cpEnsureGrin :: HsName -> EHCompilePhase ()
cpEnsureGrin nm
-- read Grin, or, if not available, read and process Core
  = do { cpGetPrevGrin nm
       ; cr <- get
       ; when (isNothing $ ecuMbGrin $ crCU nm cr)
         $ do { cpGetPrevCore nm ; cpProcessCoreFold nm ; cpProcessCoreRest nm }
       }

cpEhcCoreFullProgPostModulePhases opts modNmL (impModNmL,mainModNm)
  = cpSeq ([ cpSeq [cpGetPrevCore m | m <- modNmL]
           , mergeIntoOneBigCore
           , cpProcessCoreFold mainModNm -- redo folding for replaced main module
           ]
           ++ (if ehcOptDumpCoreStages opts then [cpOutputCore False "" "full.core" mainModNm] else [])
           ++ [ cpMsg mainModNm VerboseDebug ("Full Core generated, from: " ++ show impModNmL)
              ]
          )
  where mergeIntoOneBigCore
          = do { cr <- get
               ; cpUpdCU mainModNm 
                 $ ecuStoreCore 
                 $ cModMerge2 ([ mOf m cr | m <- impModNmL ], mOf mainModNm cr)
%%[[99
               ; cpCleanupCore impModNmL -- clean up Core and CoreSem (it can still be read through cr in the next statement)
%%]]
               }
          where mOf m cr = panicJust "cpEhcCoreFullProgPostModulePhases.mergeIntoOneBigCore" $ ecuMbCore $ crCU m cr
%%]

%%[50 haddock
Per module compilation of (import) ordered sequence of module, as part of a full program compilation
%%]

%%[50
cpEhcFullProgModuleCompileN :: [HsName] -> EHCompilePhase ()
cpEhcFullProgModuleCompileN modNmL
  = cpSeq (merge (map cpEhcFullProgModuleCompile1    modNmL)
                 (map cpEhcFullProgBetweenModuleFlow modNmL)
          )
  where merge (c1:cs1) (c2:cs2) = c1 : c2 : merge cs1 cs2
        merge []       cs       = cs
        merge cs       []       = cs
%%]

%%[50 haddock
Find out whether a compilation is needed, and if so, can be done.
%%]

%%[50
cpEhcFullProgModuleDetermineNeedsCompile :: HsName -> EHCompilePhase ()
cpEhcFullProgModuleDetermineNeedsCompile modNm
  = do { cr <- get
       ; let (ecu,_,opts,_) = crBaseInfo modNm cr
             needsCompile = crModNeedsCompile modNm cr
             canCompile   = crModCanCompile modNm cr
       ; when (ehcOptVerbosity opts >= VerboseDebug)
              (lift $ putStrLn
                (  show modNm
                ++ ", src fpath: " ++ show (ecuSrcFilePath ecu)
                ++ ", fileloc: " ++ show (ecuFileLocation ecu)
                ++ ", needs compile: " ++ show needsCompile
                ++ ", can compile: " ++ show canCompile
                ++ ", can use HI instead of HS: " ++ show (ecuCanUseHIInsteadOfHS ecu)
                ++ ", has main: " ++ show (ecuHasMain ecu)
                ++ ", is main: " ++ show (ecuIsMainMod ecu)
                ++ ", is top: " ++ show (ecuIsTopMod ecu)
                ++ ", valid HI: " ++ show (ecuIsValidHIInfo ecu)
                ++ ", HS newer: " ++ show (ecuIsHSNewerThanHI ecu)
                ))
       ; cpUpdCU modNm (ecuSetNeedsCompile (needsCompile && canCompile))
       }
%%]

%%[50 haddock
Compilation of 1 module, as part of a full program compilation
%%]

%%[50
cpEhcFullProgModuleCompile1 :: HsName -> EHCompilePhase ()
cpEhcFullProgModuleCompile1 modNm
  = do { cr <- get
       ; let (_,opts) = crBaseInfo' cr   -- '
       ; when (ehcOptVerbosity opts >= VerboseALot)
              (lift $ putStrLn ("====================== Compile1: " ++ show modNm ++ "======================"))
       ; cpEhcFullProgModuleDetermineNeedsCompile modNm
       ; cr <- get
       ; let (ecu,_,_,_) = crBaseInfo modNm cr
             targ = if ecuNeedsCompile ecu then HSAllSem else HIAllSem
       ; cpEhcModuleCompile1 (Just targ) modNm
       ; return ()
       }
%%]

%%[50 haddock
Flow of info between modules, as part of a full program compilation
%%]

%%[50
cpEhcFullProgBetweenModuleFlow :: HsName -> EHCompilePhase ()
cpEhcFullProgBetweenModuleFlow modNm
  = do { cr <- get
       ; case ecuState $ crCU modNm cr of
           ECUSHaskell HSAllSem -> return ()
           ECUSHaskell HIAllSem -> cpFlowHISem modNm
           _                    -> return ()
%%[[99
       ; cpCleanupFlow modNm
%%]]
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Per module compile actions: level 4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8.cpEhcModuleCompile1.sig export(cpEhcModuleCompile1)
cpEhcModuleCompile1 :: HsName -> EHCompilePhase ()
cpEhcModuleCompile1 modNm
%%]
%%[50 -8.cpEhcModuleCompile1.sig export(cpEhcModuleCompile1)
cpEhcModuleCompile1 :: Maybe HSState -> HsName -> EHCompilePhase HsName
cpEhcModuleCompile1 targHSState modNm
%%]
%%[8
  = do { cr <- get
       ; let (ecu,_,opts,fp) = crBaseInfo modNm cr
%%[[8
             defaultResult = ()
%%][50
             defaultResult = modNm
%%]]
%%[[50
       ; when (ehcOptVerbosity opts >= VerboseALot)
              (lift $ putStrLn ("====================== Module: " ++ show modNm ++ " ======================"))
       ; when (ehcOptVerbosity opts >= VerboseDebug)
              (lift $ putStrLn ("State: in: " ++ show (ecuState ecu) ++ ", to: " ++ show targHSState))
%%]]
%%[[8
       ; case (ecuState ecu,panic "CompilePhase.TopLevelPhases.cpEhcModuleCompile1") of
%%][50
       ; case (ecuState ecu,targHSState) of
%%]]
%%]
%%[50
           (ECUSHaskell st,Just HSOnlyImports)
             |    st == HSStart
%%[[99
               || st == LHSStart
%%]]
             -> do { cpEhcHaskellModulePrepareHS1 modNm
                   ; modNm2 <- cpEhcHaskellImport stnext
%%[[99
                                                  (pkgExposedPackages $ ehcOptPkgDb opts)
%%]]
                                                  modNm
                   ; cpEhcHaskellModulePrepareHS2 modNm2
                   ; cpMsg modNm2 VerboseNormal ("Imports of " ++ hsstateShowLit st ++ "Haskell")
                   ; when (ehcOptVerbosity opts >= VerboseDebug)
                          (do { cr <- get
                              ; let (ecu,_,opts,fp) = crBaseInfo modNm2 cr
                              ; lift $ putStrLn ("After HS import: nm=" ++ show modNm ++ ", newnm=" ++ show modNm2 ++ ", fp=" ++ show fp ++ ", imp=" ++ show (ecuImpNmL ecu))
                              })
                   ; cpUpdCU modNm2 (ecuStoreState (ECUSHaskell stnext))
                   ; cpStopAt CompilePoint_Imports
                   ; return modNm2
                   }
             where stnext = hsstateNext st
           (ECUSHaskell HIStart,Just HSOnlyImports)
             -> do { cpMsg modNm VerboseNormal ("Imports of HI")
                   ; cpEhcHaskellModulePrepareHI modNm
                   ; cpUpdCU modNm (ecuStoreState (ECUSHaskell (hsstateNext HIStart)))
                   ; when (ehcOptVerbosity opts >= VerboseDebug)
                          (do { cr <- get
                              ; let (ecu,_,opts,fp) = crBaseInfo modNm cr
                              ; lift $ putStrLn ("After HI import: nm=" ++ show modNm ++ ", fp=" ++ show fp ++ ", imp=" ++ show (ecuImpNmL ecu))
                              })
                   ; return defaultResult
                   }
           (ECUSHaskell st,Just HSOnlyImports)
             |    st == HSOnlyImports
               || st == HIOnlyImports
%%[[99
               || st == LHSOnlyImports
%%]]
             -> return defaultResult
           (ECUSHaskell st,Just HSAllSem)
             |    st == HSOnlyImports
%%[[99
               || st == LHSOnlyImports
%%]]
             -> do { cpMsg modNm VerboseMinimal ("Compiling " ++ hsstateShowLit st ++ "Haskell")
                   ; cpEhcHaskellModuleAfterImport (ecuIsTopMod ecu) opts st
%%[[99
                                                   (pkgExposedPackages $ ehcOptPkgDb opts)
%%]]
                                                   modNm
                   ; cpUpdCU modNm (ecuStoreState (ECUSHaskell HSAllSem))
                   ; return defaultResult
                   }
           (ECUSHaskell st,Just HIAllSem)
             |    st == HSOnlyImports
               || st == HIOnlyImports
%%[[99
               || st == LHSOnlyImports
%%]]
             -> do { cpMsg modNm VerboseNormal "Reading HI"
%%[[(50 codegen grin)
                   ; cpUpdateModOffMp [modNm]
%%]]
                   ; cpUpdCU modNm (ecuStoreState (ECUSHaskell HIAllSem))
                   ; return defaultResult
                   }
%%]]
%%]
%%[8
           (ECUSHaskell HSStart,_)
             -> do { cpMsg modNm VerboseMinimal "Compiling Haskell"
                   ; cpEhcHaskellModulePrepare modNm
                   ; cpEhcHaskellParse
%%[[8
                                       False False
%%][99
                                       (ehcOptCPP opts) False
                                       (pkgExposedPackages $ ehcOptPkgDb opts)
%%]]
                                       modNm
                   ; cpEhcHaskellModuleCommonPhases True True opts modNm
                   ; when (ehcOptWholeProgHPTAnalysis opts)
                          (cpEhcCoreGrinPerModuleDoneFullProgAnalysis modNm)
                   ; cpUpdCU modNm (ecuStoreState (ECUSHaskell HSAllSem))
                   ; return defaultResult
                   }
%%[[50
           (_,Just HSOnlyImports)
             -> return defaultResult
%%]]
           (ECUSEh EHStart,_)
             -> do { cpMsg modNm VerboseMinimal "Compiling EH"
                   ; cpUpdOpts (\o -> o {ehcOptHsChecksInEH = True})
                   ; cpEhcEhParse modNm
%%[[50   
                   ; cpGetDummyCheckEhMod modNm
%%]]   
                   ; cpEhcEhModuleCommonPhases True True True opts modNm
                   
                   ; when (ehcOptWholeProgHPTAnalysis opts)
                          (cpEhcCoreGrinPerModuleDoneFullProgAnalysis modNm)
                   ; cpUpdCU modNm (ecuStoreState (ECUSEh EHAllSem))
                   ; return defaultResult
                   }
%%[[90
           (ECUSC CStart,_)
             | targetIsOnUnixAndOrC (ehcOptTarget opts)
             -> do { cpSeq [ cpMsg modNm VerboseMinimal "Compiling C"
                           , cpCompileWithGCC FinalCompile_Module [] modNm
                           , cpUpdCU modNm (ecuStoreState (ECUSC CAllSem))
                           ]
                   ; return defaultResult
                   }
             | otherwise
             -> do { cpMsg modNm VerboseMinimal "Skipping C"
                   ; return defaultResult
                   }
%%]]
%%[[(8 codegen grin)
           (ECUSGrin,_)
             -> do { cpMsg modNm VerboseMinimal "Compiling Grin"
                   ; cpParseGrin modNm
                   ; cpProcessGrin modNm
                   ; cpProcessBytecode modNm 
                   ; return defaultResult
                   }
                   
%%]]
           _ -> return defaultResult
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Per module compile actions: level 3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 haddock
EH common phases: analysis + core + grin
%%]

%%[8
cpEhcEhModuleCommonPhases :: Bool -> Bool -> Bool -> EHCOpts -> HsName -> EHCompilePhase ()
cpEhcEhModuleCommonPhases isMainMod isTopMod doMkExec opts modNm
  = cpSeq ([ cpEhcEhAnalyseModuleDefs modNm
%%[[(8 codegen)
           , cpEhcCorePerModulePart1 modNm
%%]]
           ]
%%[[(8 codegen grin)
           ++ (if ehcOptWholeProgHPTAnalysis opts
               then []
               else [cpEhcCoreGrinPerModuleDoneNoFullProgAnalysis opts isMainMod isTopMod doMkExec modNm]
              )
%%]]
          )
%%]

%%[8 haddock
HS common phases: HS analysis + EH common
%%]

%%[8
cpEhcHaskellModuleCommonPhases :: Bool -> Bool -> EHCOpts -> HsName -> EHCompilePhase ()
cpEhcHaskellModuleCommonPhases isTopMod doMkExec opts modNm
  = cpSeq [ cpEhcHaskellAnalyseModuleDefs modNm
          , do { cr <- get
               ; let (ecu,_,_,_) = crBaseInfo modNm cr
               ; cpEhcEhModuleCommonPhases
%%[[8
                   isTopMod
%%][50
                   (ecuIsMainMod ecu)
%%]]
                   isTopMod doMkExec opts modNm
               }
          ]       
%%]

%%[50 haddock
Post module import common phases: Parse + Module analysis + HS common
%%]

%%[50
cpEhcHaskellModuleAfterImport
  :: Bool -> EHCOpts -> HSState
%%[[99
     -> [PkgModulePartition]
%%]]
     -> HsName -> EHCompilePhase ()
cpEhcHaskellModuleAfterImport
     isTopMod opts hsst
%%[[99
     pkgKeyDirL
%%]]
     modNm
  = cpSeq [ cpEhcHaskellParse False (hsstateIsLiteral hsst)
%%[[99
                              pkgKeyDirL
%%]]
                              modNm
          , cpEhcHaskellAnalyseModuleItf modNm
          , cpEhcHaskellModuleCommonPhases isTopMod False opts modNm
          , cpEhcHaskellModulePostlude modNm
          ]       
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Per module compile actions: level 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 haddock
Prepare module for compilation.
This should be the first step before compilation of a module and is meant to obtain cached info from a previous compilation.
%%]

%%[8.cpEhcHaskellModulePrepare
cpEhcHaskellModulePrepare :: HsName -> EHCompilePhase ()
cpEhcHaskellModulePrepare _ = return ()
%%]

We need to know meta info in a more staged manner.
To be able to cpp preprocess first we need to know whether a Haskell file exists.
(1) we get the timestamp, so we know the file exists, so we can preprocess.
(..) then happens other stuff, getting the real module name, getting the import list.
(2) only then we can get info about derived files because the location is based on the real module name.
    Previous info also has to be obtained again.

%%[50 -8.cpEhcHaskellModulePrepare
cpEhcHaskellModulePrepareHS1 :: HsName -> EHCompilePhase ()
cpEhcHaskellModulePrepareHS1 modNm
  = cpGetMetaInfo [GetMeta_HS,GetMeta_Dir] modNm

cpEhcHaskellModulePrepareHS2 :: HsName -> EHCompilePhase ()
cpEhcHaskellModulePrepareHS2 modNm
  = cpSeq [ cpGetMetaInfo [GetMeta_HS, GetMeta_HI, GetMeta_Core, GetMeta_Grin, GetMeta_Dir] modNm
          , cpGetPrevHI modNm
          -- , cpFoldHI modNm
          , cpFoldHIInfo modNm
          ]

cpEhcHaskellModulePrepareHI :: HsName -> EHCompilePhase ()
cpEhcHaskellModulePrepareHI modNm
  = cpSeq [ cpGetMetaInfo [GetMeta_HI, GetMeta_Core, GetMeta_Grin] modNm
          , cpGetPrevHI modNm
          -- , cpFoldHI modNm
          , cpFoldHIInfo modNm
          ]

cpEhcHaskellModulePrepare :: HsName -> EHCompilePhase ()
cpEhcHaskellModulePrepare modNm
  = cpSeq [ cpEhcHaskellModulePrepareHS1 modNm
          , cpEhcHaskellModulePrepareHS2 modNm
          ]
%%]

%%[50
cpEhcHaskellModulePostlude :: HsName -> EHCompilePhase ()
cpEhcHaskellModulePostlude modNm
  = cpSeq [ cpOutputHI "hi" modNm
%%[[99
          , cpCleanupCU modNm
%%]]
          ]
%%]

%%[50 haddock
Get import information from module source text.
%%]

%%[50
cpEhcHaskellImport
  :: HSState
%%[[99
     -> [PkgModulePartition]
%%]]
     -> HsName -> EHCompilePhase HsName
cpEhcHaskellImport
     hsst
%%[[99
     pkgKeyDirL
%%]]
     modNm
  = do { cr <- get
       ; let (_,opts) = crBaseInfo' cr
       
       -- 1st, parse
       ; cppAndParse modNm
%%[[99
           (ehcOptCPP opts)
%%]]
       ; cpStepUID
       
       -- and then get pragmas and imports
%%[[50
       ; foldAndImport modNm
%%][99
       ; modNm' <- foldAndImport modNm
       ; cr2 <- get
       ; let (ecu,_,opts2,_) = crBaseInfo modNm' cr2
       
       -- if we find out that CPP should have invoked, we backtrack to the original runstate and redo the above with CPP
       ; if not (ehcOptCPP opts2) && Set.member Pragma_CPP (ecuPragmas ecu)
         then do { put cr
                 ; cppAndParse modNm True
                 ; cpStepUID
                 ; foldAndImport modNm
                 }
         else return modNm'
%%]]
       }
  where cppAndParse modNm
%%[[50
          = cpSeq [ cpParseHsImport modNm
                  ]
%%][99
          doCPP
          = cpSeq [ when doCPP (cpPreprocessWithCPP pkgKeyDirL modNm)
                  , cpParseHsImport (hsstateIsLiteral hsst) modNm
                  ]
%%]]
        foldAndImport modNm
          = do { cpFoldHsMod modNm
               ; cpGetHsImports modNm
               }
%%]

%%[50 haddock
Parse a Haskell module
%%]

%%[8
cpEhcHaskellParse
  :: Bool -> Bool
%%[[99
     -> [PkgModulePartition]
%%]]
     -> HsName -> EHCompilePhase ()
cpEhcHaskellParse
     doCPP litmode
%%[[99
     pkgKeyDirL
%%]]
     modNm
  = cpSeq (
%%[[8
             [ cpParseHs modNm ]
%%][99
             (if doCPP then [cpPreprocessWithCPP pkgKeyDirL modNm] else [])
          ++ [ cpParseHs litmode modNm ]
%%]]
          ++ [ cpMsg modNm VerboseALot "Parsing done"
             , cpStopAt CompilePoint_Parse
             ]
          )
%%]

%%[8
cpEhcEhParse :: HsName -> EHCompilePhase ()
cpEhcEhParse modNm
  = cpSeq [ cpParseEH modNm
          , cpStopAt CompilePoint_Parse
          ]
%%]

%%[50 haddock
Analyse a module for
  (1) module information (import, export, etc),
%%]

%%[50
cpEhcHaskellAnalyseModuleItf :: HsName -> EHCompilePhase ()
cpEhcHaskellAnalyseModuleItf modNm
  = cpSeq [ cpStepUID, cpFoldHsMod modNm, cpGetHsMod modNm
%%[[99
          , cpCleanupHSMod modNm
%%]]
          , cpCheckMods [modNm]
%%[[(50 codegen grin)
          , cpUpdateModOffMp [modNm]
%%]]
          ]
%%]

%%[8 haddock
Analyse a module for
  (2) names + dependencies
%%]

%%[8
cpEhcHaskellAnalyseModuleDefs :: HsName -> EHCompilePhase ()
cpEhcHaskellAnalyseModuleDefs modNm
  = cpSeq [ cpStepUID
          , cpProcessHs modNm
          , cpMsg modNm VerboseALot "Name+dependency analysis done"
          , cpStopAt CompilePoint_AnalHS
          ]
%%]

%%[8 haddock
Analyse a module for
  (3) types
%%]

%%[8
cpEhcEhAnalyseModuleDefs :: HsName -> EHCompilePhase ()
cpEhcEhAnalyseModuleDefs modNm
  = cpSeq [ cpStepUID, cpProcessEH modNm
          , cpMsg modNm VerboseALot "Type analysis done"
          , cpStopAt CompilePoint_AnalEH
          ]
%%]

%%[(8 codegen) haddock
Part 1 Core processing, on a per module basis, part1 is done always
%%]

%%[(8 codegen)
cpEhcCorePerModulePart1 :: HsName -> EHCompilePhase ()
cpEhcCorePerModulePart1 modNm
  = do { cr <- get
       ; let (_,_,opts,_) = crBaseInfo modNm cr
%%[[8
             earlyMerge = False
%%][50
             earlyMerge = ehcOptOptimizationScope opts >= OptimizationScope_WholeCore
%%]]
       ; cpSeq
           (  [ cpStepUID ]
%%[[(8 tycore)
           ++ (if ehcOptTyCore opts
               then [ cpProcessTyCoreBasic modNm
                    , cpMsg modNm VerboseALot "TyCore (basic) done"
                    , cpTranslateTyCore2Core modNm
                    , cpStepUID
                    ]
               else []
              )
%%]]
           ++ [ cpProcessCoreBasic modNm
              , cpMsg modNm VerboseALot "Core (basic) done"
              , when (not earlyMerge) $ cpProcessCoreRest modNm
              , cpStopAt CompilePoint_Core
              ]
           )
       }
%%]

%%[(8 codegen) haddock
Part 2 Core processing, part2 is done either for individual modules or after full program analysis
%%]

%%[(8 codegen)
cpEhcCorePerModulePart2 :: HsName -> EHCompilePhase ()
cpEhcCorePerModulePart2 modNm
  = do { cr <- get
       ; let (_,_,opts,_) = crBaseInfo modNm cr
%%[[8
             earlyMerge = False
%%][50
             earlyMerge = ehcOptOptimizationScope opts >= OptimizationScope_WholeCore
%%]]
       ; cpSeq [ when earlyMerge $ cpProcessCoreRest modNm
               , when (targetIsGrin (ehcOptTarget opts)) (cpProcessGrin modNm)
               ]
       }
%%]

%%[(8 codegen grin) haddock
Core+grin processing, on a per module basis, may only be done when no full program analysis is done
%%]

%%[(8 codegen grin)
cpEhcCoreGrinPerModuleDoneNoFullProgAnalysis :: EHCOpts -> Bool -> Bool -> Bool -> HsName -> EHCompilePhase ()
cpEhcCoreGrinPerModuleDoneNoFullProgAnalysis opts isMainMod isTopMod doMkExec modNm
  = cpSeq (  [ cpEhcCorePerModulePart2 modNm
%%[[50
             , cpMsg modNm VerboseDebug "cpFlowOptim"
             , cpFlowOptim modNm
%%]]
%%[[99
             , cpCleanupGrin [modNm]
%%]]
             , when doesGrin (cpProcessBytecode modNm)
             ]
          ++ (if not isMainMod || doMkExec
              then let how = if doMkExec then FinalCompile_Exec else FinalCompile_Module
                   in  [cpEhcExecutablePerModule how [] modNm]
              else []
             )
          ++ [ cpMsg modNm VerboseALot ("Core" ++ (if doesGrin then "+Grin" else "") ++ " done")
             , cpMsg modNm VerboseDebug ("isMainMod: " ++ show isMainMod)
             ]
          )
  where doesGrin = targetIsGrinBytecode (ehcOptTarget opts)
%%]

%%[(8 codegen grin) haddock
Core+grin processing, on a per module basis, may only be done when full program analysis is done
%%]

%%[(8 codegen grin)
cpEhcCoreGrinPerModuleDoneFullProgAnalysis :: HsName -> EHCompilePhase ()
cpEhcCoreGrinPerModuleDoneFullProgAnalysis modNm
  = cpSeq (  [ cpEhcCorePerModulePart2 modNm
             , cpEhcExecutablePerModule FinalCompile_Exec [] modNm
             , cpMsg modNm VerboseALot "Full Program Analysis (Core+Grin) done"
             ]
          )
%%]

%%[(8 codegen) haddock
Make final executable code, either still partly or fully (i.e. also linking)
%%]

%%[(8 codegen)
cpEhcExecutablePerModule :: FinalCompileHow -> [HsName] -> HsName -> EHCompilePhase ()
cpEhcExecutablePerModule how impModNmL modNm
  = cpSeq [ cpCompileWithGCC how impModNmL modNm
          , cpCompileWithLLVM modNm
%%[[(8 jazy)
          , cpCompileJazyJVM how impModNmL modNm
%%]]
%%[[(8 jscript)
          , cpCompileJScript how impModNmL modNm
%%]]
          ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Per module compile actions: level 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
cpProcessHs :: HsName -> EHCompilePhase ()
cpProcessHs modNm 
  = cpSeq [ cpFoldHs modNm
%%[[50
          , cpFlowHsSem1 modNm
%%]]
          , cpTranslateHs2EH modNm
%%[[99
          , cpCleanupHS modNm
%%]]
          ]
%%]

%%[8
cpProcessEH :: HsName -> EHCompilePhase ()
cpProcessEH modNm
  = do { cr <- get
       ; let (_,_,opts,_) = crBaseInfo modNm cr
       ; cpSeq [ cpFoldEH modNm
%%[[99
               , cpCleanupFoldEH modNm
%%]]
               , cpFlowEHSem1 modNm
               , cpTranslateEH2Output modNm
%%[[(8 codegen)
               ,
%%[[(8 tycore)
                 if ehcOptTyCore opts
                 then cpTranslateEH2TyCore modNm
                 else 
%%]]
                      cpTranslateEH2Core modNm
%%]]
%%[[99
               , cpCleanupEH modNm
%%]]
               ]
       }
%%]

%%[(8 codegen tycore)
-- | TBD: finish it, only a sketch now
cpProcessTyCoreBasic :: HsName -> EHCompilePhase ()
cpProcessTyCoreBasic modNm 
  = do { cr <- get
       ; let (_,_,opts,_) = crBaseInfo modNm cr
             check m  = do { cr <- get
                           ; let (ecu,_,opts,_) = crBaseInfo m cr
                                 cMod   = panicJust "cpProcessTyCoreBasic" $ ecuMbTyCore ecu
                                 errs   = C.tcCheck opts C.emptyCheckEnv cMod
                           ; cpSetLimitErrsWhen 500 "Check TyCore" errs
                           }
       ; cpSeq [ cpTransformTyCore modNm
               , when (ehcOptTyCore opts)
                      (do { cpOutputTyCore "tycore" modNm
                          ; check modNm
                          })
               ]
        }
%%]

%%[(8 codegen)
-- unprocessed core -> folded core
cpProcessCoreBasic :: HsName -> EHCompilePhase ()
cpProcessCoreBasic modNm 
  = do { cr <- get
       ; let (_,_,opts,_) = crBaseInfo modNm cr
       ; cpSeq [ cpTransformCore modNm
%%[[50
               , cpFlowHILamMp modNm
%%]]
               -- , when (ehcOptEmitCore opts) (cpOutputCore True "" "core" modNm)
               , cpOutputCore True "" "core" modNm
%%[[(8888 codegen java)
               , when (ehcOptEmitJava opts) (cpOutputJava         "java" modNm)
%%]]
               , cpProcessCoreFold modNm
               ]
        }
%%]

%%[(8 codegen)
-- unfolded core -> folded core
-- (called on merged core, and on core directly generated from cached grin)
cpProcessCoreFold :: HsName -> EHCompilePhase ()
cpProcessCoreFold modNm
  = cpSeq [ cpFoldCore modNm
%%[[50
          , cpFlowCoreSem modNm
%%]]
          ]
%%]

%%[(8 codegen)
-- folded core -> grin, jazy, and the rest
cpProcessCoreRest :: HsName -> EHCompilePhase ()
cpProcessCoreRest modNm
  = do { cr <- get
       ; let (_,_,opts,_) = crBaseInfo modNm cr
       ; cpSeq (   [ cpTranslateCore2Grin modNm ]
                -- ++ (if ehcOptWholeProgHPTAnalysis opts then [ cpOutputGrin True "" modNm ] else [])
                ++ (if targetIsGrin (ehcOptTarget opts) then [ cpOutputGrin True "" modNm ] else [])
%%[[(8 jazy)
                ++ [ cpTranslateCore2Jazy modNm ]
%%]]
%%[[(8 jscript)
                ++ [ cpTranslateCore2JScript modNm ]
%%]]
%%[[99
                ++ [ cpCleanupCore [modNm] ]
%%]]
               )
       }
          
%%]

%%[(8 codegen grin)
cpProcessGrin :: HsName -> EHCompilePhase ()
cpProcessGrin modNm 
  = do { cr <- get
       ; let (_,_,opts,_) = crBaseInfo modNm cr
       ; cpSeq (   (if ehcOptDumpGrinStages opts then [cpOutputGrin False "-000-initial" modNm] else [])
                ++ [cpTransformGrin modNm]
                ++ (if ehcOptDumpGrinStages opts then [cpOutputGrin False "-099-final" modNm]  else [])
                ++ (if ehcOptEmitBytecode opts then [cpTranslateGrin2Bytecode modNm] else [])
                ++ (if targetDoesHPTAnalysis (ehcOptTarget opts) then [cpTranslateGrin modNm] else [])
               )
       }
%%]

%%[(8 codegen grin)
cpProcessBytecode :: HsName -> EHCompilePhase ()
cpProcessBytecode modNm 
  = do { cr <- get
       ; let (_,_,opts,_) = crBaseInfo modNm cr
       ; cpSeq [ cpMsg modNm VerboseALot "Translate ByteCode"
               , cpTranslateByteCode modNm
%%[[50
               , cpFlowHILamMp modNm
%%]]
%%[[99
               , cpCleanupFoldBytecode modNm
%%]]
               , when (ehcOptEmitBytecode opts) (cpOutputByteCodeC "c" modNm)
%%[[99
               , cpCleanupBytecode modNm
%%]]
               ]
       }

%%]


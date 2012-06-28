module EH101.EHC.CompilePhase.TopLevelPhases
( cpEhcCheckAbsenceOfMutRecModules
, cpEhcFullProgCompileAllModules
, cpEhcFullProgModuleDetermineNeedsCompile
, cpEhcModuleCompile1 )
where
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO.Unsafe
import Debug.Trace
import EH101.Base.Optimize
import EH101.Base.Target
import EH101.EHC.Common
import EH101.EHC.CompileUnit
import EH101.EHC.CompileRun
import EH101.EHC.CompilePhase.Parsers
import EH101.EHC.CompilePhase.Translations
import EH101.EHC.CompilePhase.Output
import EH101.EHC.CompilePhase.Transformations
import EH101.EHC.CompilePhase.TransformGrin
import EH101.EHC.CompilePhase.Semantics
import EH101.EHC.CompilePhase.FlowBetweenPhase
import EH101.EHC.CompilePhase.CompileC
import EH101.EHC.CompilePhase.CompileJavaScript
import EH101.EHC.CompileGroup
import EH101.EHC.CompilePhase.Module
import EH101.Module
import qualified EH101.Core as Core (cModMerge)
import EH101.Core.Utils (cModMerge2)
import qualified EH101.GrinCode as Grin (grModMerge)
import EH101.Base.PackageDatabase
import EH101.EHC.CompilePhase.Link
import EH101.Base.Pragma
import EH101.EHC.CompilePhase.Cleanup
{-# LINE 6 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
{-|
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

-}











{-# LINE 95 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
{-|
Top level entry point into compilation by the compiler driver, apart from import analysis, wich is assumed to be done.

-}
{-# LINE 99 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
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
                                        , cpCleanupGrin [mainModNm]
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
                -> case ehcOptPkg opts of
                     Just (PkgOption_Build pkg)
                       | targetAllowsOLinking (ehcOptTarget opts)
                         -> cpLinkO impModNmL pkg
                     _ -> return ()
      }
  where splitMain cr = partition (\n -> ecuHasMain $ crCU n cr)

{-# LINE 152 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
cpEhcCheckAbsenceOfMutRecModules :: EHCompilePhase ()
cpEhcCheckAbsenceOfMutRecModules
 = do { cr <- get
      ; let mutRecL = filter (\ml -> length ml > 1) $ crCompileOrder cr
      ; when (not $ null mutRecL)
             (cpSetLimitErrs 1 "compilation run" [rngLift emptyRange Err_MutRecModules mutRecL]
             )
      }

{-# LINE 175 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
cpEhcFullProgCompileAllModules :: EHCompilePhase ()
cpEhcFullProgCompileAllModules
 = do { cr <- get
      ; let modNmLL = crCompileOrder cr
            modNmL = map head modNmLL
      ; cpSeq (   []
               ++ (let modNmL2 = filter (\m -> let (ecu,_,_,_) = crBaseInfo m cr in not $ filelocIsPkg $ ecuFileLocation ecu) modNmL
                       nrMods = length modNmL2
                   in  zipWith (\m i -> cpUpdCU m (ecuStoreSeqNr (EHCCompileSeqNr i nrMods)) ) modNmL2 [1..nrMods]
                  )
               ++ [cpEhcFullProgModuleCompileN modNmL]
               ++ [cpEhcFullProgLinkAllModules modNmL]
              )
      }

{-# LINE 200 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
{-|
Post per module phases, as part of a full program compilation.
Post processing involves the following:
1. if doing full program analysis, merge the already compiled Core/Grin representations, and do the same work as for 1 module but now for the merged Core/Grin
2. compile+link everything together

-}
{-# LINE 207 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
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
           , cpCleanupGrin impModNmL -- clean up unused Grin (moved here from cpCleanupCU)
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
               ; cpCleanupCore impModNmL -- clean up Core and CoreSem (it can still be read through cr in the next statement)
               }
          where mOf m cr = panicJust "cpEhcCoreFullProgPostModulePhases.mergeIntoOneBigCore" $ ecuMbCore $ crCU m cr

{-# LINE 266 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
{-|
Per module compilation of (import) ordered sequence of module, as part of a full program compilation

-}
{-# LINE 270 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
cpEhcFullProgModuleCompileN :: [HsName] -> EHCompilePhase ()
cpEhcFullProgModuleCompileN modNmL
  = cpSeq (merge (map cpEhcFullProgModuleCompile1    modNmL)
                 (map cpEhcFullProgBetweenModuleFlow modNmL)
          )
  where merge (c1:cs1) (c2:cs2) = c1 : c2 : merge cs1 cs2
        merge []       cs       = cs
        merge cs       []       = cs

{-# LINE 281 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
{-|
Find out whether a compilation is needed, and if so, can be done.

-}
{-# LINE 285 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
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

{-# LINE 310 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
{-|
Compilation of 1 module, as part of a full program compilation

-}
{-# LINE 314 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
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

{-# LINE 330 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
{-|
Flow of info between modules, as part of a full program compilation

-}
{-# LINE 334 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
cpEhcFullProgBetweenModuleFlow :: HsName -> EHCompilePhase ()
cpEhcFullProgBetweenModuleFlow modNm
  = do { cr <- get
       ; case ecuState $ crCU modNm cr of
           ECUSHaskell HSAllSem -> return ()
           ECUSHaskell HIAllSem -> cpFlowHISem modNm
           _                    -> return ()
       ; cpCleanupFlow modNm
       }

{-# LINE 356 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
cpEhcModuleCompile1 :: Maybe HSState -> HsName -> EHCompilePhase HsName
cpEhcModuleCompile1 targHSState modNm
{-# LINE 360 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
  = do { cr <- get
       ; let (ecu,_,opts,fp) = crBaseInfo modNm cr
             defaultResult = modNm
       ; when (ehcOptVerbosity opts >= VerboseALot)
              (lift $ putStrLn ("====================== Module: " ++ show modNm ++ " ======================"))
       ; when (ehcOptVerbosity opts >= VerboseDebug)
              (lift $ putStrLn ("State: in: " ++ show (ecuState ecu) ++ ", to: " ++ show targHSState))
       ; case (ecuState ecu,targHSState) of
{-# LINE 388 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
           (ECUSHaskell st,Just HSOnlyImports)
             |    st == HSStart
               || st == LHSStart
             -> do { cpEhcHaskellModulePrepareHS1 modNm
                   ; modNm2 <- cpEhcHaskellImport stnext
                                                  (pkgExposedPackages $ ehcOptPkgDb opts)
                                                  modNm
                   ; cpEhcHaskellModulePrepareHS2 modNm2
                   ; cpMsg modNm2 VerboseNormal ("Imports of " ++ hsstateShowLit st ++ "Haskell")
                   ; when (ehcOptVerbosity opts >= VerboseDebug)
                          (do { cr <- get
                              ; let (ecu,_,opts,fp) = crBaseInfo modNm2 cr
                              ; lift $ putStrLn ("After HS import: nm=" ++ show modNm ++ ", newnm=" ++ show modNm2 ++ ", fp=" ++ show fp ++ ", imp=" ++ show (ecuImpNmS ecu))
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
                              ; lift $ putStrLn ("After HI import: nm=" ++ show modNm ++ ", fp=" ++ show fp ++ ", imp=" ++ show (ecuImpNmS ecu))
                              })
                   ; return defaultResult
                   }
           (ECUSHaskell st,Just HSOnlyImports)
             |    st == HSOnlyImports
               || st == HIOnlyImports
               || st == LHSOnlyImports
             -> return defaultResult
           (ECUSHaskell st,Just HSAllSem)
             |    st == HSOnlyImports
               || st == LHSOnlyImports
             -> do { cpMsg modNm VerboseMinimal ("Compiling " ++ hsstateShowLit st ++ "Haskell")
                   ; cpEhcHaskellModuleAfterImport (ecuIsTopMod ecu) opts st
                                                   (pkgExposedPackages $ ehcOptPkgDb opts)
                                                   modNm
                   ; cpUpdCU modNm (ecuStoreState (ECUSHaskell HSAllSem))
                   ; return defaultResult
                   }
           (ECUSHaskell st,Just HIAllSem)
             |    st == HSOnlyImports
               || st == HIOnlyImports
               || st == LHSOnlyImports
             -> do { cpMsg modNm VerboseNormal "Reading HI"
                   ; cpUpdateModOffMp [modNm]
                   ; cpUpdCU modNm (ecuStoreState (ECUSHaskell HIAllSem))
                   ; return defaultResult
                   }

{-# LINE 459 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
           (ECUSHaskell HSStart,_)
             -> do { cpMsg modNm VerboseMinimal "Compiling Haskell"
                   ; cpEhcHaskellModulePrepare modNm
                   ; cpEhcHaskellParse
                                       (ehcOptCPP opts) False
                                       (pkgExposedPackages $ ehcOptPkgDb opts)
                                       modNm
                   ; cpEhcHaskellModuleCommonPhases True True opts modNm
                   ; when (ehcOptWholeProgHPTAnalysis opts)
                          (cpEhcCoreGrinPerModuleDoneFullProgAnalysis modNm)
                   ; cpUpdCU modNm (ecuStoreState (ECUSHaskell HSAllSem))
                   ; return defaultResult
                   }
           (ECUSHaskell st,Just HMOnlyMinimal)
             |    st == HIStart || st == HSStart -- st /= HMOnlyMinimal
             -> do { let mod = emptyMod' modNm
                   ; cpUpdCU modNm (ecuStoreMod mod)
                   -- ; cpCheckMods [modNm]
				   ; cpUpdateModOffMp [modNm]
                   ; cpUpdCU modNm (ecuStoreState (ECUSHaskell HMOnlyMinimal))
                   ; return defaultResult
                   }
           (_,Just HSOnlyImports)
             -> return defaultResult
           (ECUSEh EHStart,_)
             -> do { cpMsg modNm VerboseMinimal "Compiling EH"
                   ; cpUpdOpts (\o -> o {ehcOptHsChecksInEH = True})
                   ; cpEhcEhParse modNm
                   ; cpGetDummyCheckEhMod modNm
                   ; cpEhcEhModuleCommonPhases True True True opts modNm

                   ; when (ehcOptWholeProgHPTAnalysis opts)
                          (cpEhcCoreGrinPerModuleDoneFullProgAnalysis modNm)
                   ; cpUpdCU modNm (ecuStoreState (ECUSEh EHAllSem))
                   ; return defaultResult
                   }
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
           (ECUSGrin,_)
             -> do { cpMsg modNm VerboseMinimal "Compiling Grin"
                   ; cpParseGrin modNm
                   ; cpProcessGrin modNm
                   ; cpProcessBytecode modNm
                   ; return defaultResult
                   }

           _ -> return defaultResult
       }

{-# LINE 538 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
{-|
EH common phases: analysis + core + grin

-}
{-# LINE 542 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
cpEhcEhModuleCommonPhases :: Bool -> Bool -> Bool -> EHCOpts -> HsName -> EHCompilePhase ()
cpEhcEhModuleCommonPhases isMainMod isTopMod doMkExec opts modNm
  = cpSeq ([ cpEhcEhAnalyseModuleDefs modNm
           , cpEhcCorePerModulePart1 modNm
           ]
           ++ (if ehcOptWholeProgHPTAnalysis opts
               then []
               else [cpEhcCoreGrinPerModuleDoneNoFullProgAnalysis opts isMainMod isTopMod doMkExec modNm]
              )
          )

{-# LINE 559 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
{-|
HS common phases: HS analysis + EH common

-}
{-# LINE 563 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
cpEhcHaskellModuleCommonPhases :: Bool -> Bool -> EHCOpts -> HsName -> EHCompilePhase ()
cpEhcHaskellModuleCommonPhases isTopMod doMkExec opts modNm
  = cpSeq [ cpEhcHaskellAnalyseModuleDefs modNm
          , do { cr <- get
               ; let (ecu,_,_,_) = crBaseInfo modNm cr
               ; cpEhcEhModuleCommonPhases
                   (ecuIsMainMod ecu)
                   isTopMod doMkExec opts modNm
               }
          ]

{-# LINE 580 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
{-|
Post module import common phases: Parse + Module analysis + HS common

-}
{-# LINE 584 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
cpEhcHaskellModuleAfterImport
  :: Bool -> EHCOpts -> HSState
     -> [PkgModulePartition]
     -> HsName -> EHCompilePhase ()
cpEhcHaskellModuleAfterImport
     isTopMod opts hsst
     pkgKeyDirL
     modNm
  = cpSeq [ cpEhcHaskellParse False (hsstateIsLiteral hsst)
                              pkgKeyDirL
                              modNm
          , cpEhcHaskellAnalyseModuleItf modNm
          , cpEhcHaskellModuleCommonPhases isTopMod False opts modNm
          , cpEhcHaskellModulePostlude modNm
          ]

{-# LINE 612 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
{-|
Prepare module for compilation.
This should be the first step before compilation of a module and is meant to obtain cached info from a previous compilation.

-}
{-# LINE 629 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
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

{-# LINE 657 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
cpEhcHaskellModulePostlude :: HsName -> EHCompilePhase ()
cpEhcHaskellModulePostlude modNm
  = cpSeq [ cpOutputHI "hi" modNm
          , cpCleanupCU modNm
          ]

{-# LINE 667 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
{-|
Get import information from module source text.

-}
{-# LINE 671 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
cpEhcHaskellImport
  :: HSState
     -> [PkgModulePartition]
     -> HsName -> EHCompilePhase HsName
cpEhcHaskellImport
     hsst
     pkgKeyDirL
     modNm
  = do { cr <- get
       ; let (_,opts) = crBaseInfo' cr

       -- 1st, parse
       ; cppAndParse modNm
           (ehcOptCPP opts)
       ; cpStepUID

       -- and then get pragmas and imports
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
       }
  where cppAndParse modNm
          doCPP
          = cpSeq [ when doCPP (cpPreprocessWithCPP pkgKeyDirL modNm)
                  , cpParseHsImport (hsstateIsLiteral hsst) modNm
                  ]
        foldAndImport modNm
          = do { cpFoldHsMod modNm
               ; cpGetHsImports modNm
               }

{-# LINE 728 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
{-|
Parse a Haskell module

-}
{-# LINE 732 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
cpEhcHaskellParse
  :: Bool -> Bool
     -> [PkgModulePartition]
     -> HsName -> EHCompilePhase ()
cpEhcHaskellParse
     doCPP litmode
     pkgKeyDirL
     modNm
  = cpSeq (
             (if doCPP then [cpPreprocessWithCPP pkgKeyDirL modNm] else [])
          ++ [ cpParseHs litmode modNm ]
          ++ [ cpMsg modNm VerboseALot "Parsing done"
             , cpStopAt CompilePoint_Parse
             ]
          )

{-# LINE 758 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
cpEhcEhParse :: HsName -> EHCompilePhase ()
cpEhcEhParse modNm
  = cpSeq [ cpParseEH modNm
          , cpStopAt CompilePoint_Parse
          ]

{-# LINE 766 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
{-|
Analyse a module for
  (1) module information (import, export, etc),

-}
{-# LINE 771 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
cpEhcHaskellAnalyseModuleItf :: HsName -> EHCompilePhase ()
cpEhcHaskellAnalyseModuleItf modNm
  = cpSeq [ cpStepUID, cpFoldHsMod modNm, cpGetHsMod modNm
          , cpCleanupHSMod modNm
          , cpCheckMods [modNm]
          , cpUpdateModOffMp [modNm]
          ]

{-# LINE 785 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
{-|
Analyse a module for
  (2) names + dependencies

-}
{-# LINE 790 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
cpEhcHaskellAnalyseModuleDefs :: HsName -> EHCompilePhase ()
cpEhcHaskellAnalyseModuleDefs modNm
  = cpSeq [ cpStepUID
          , cpProcessHs modNm
          , cpMsg modNm VerboseALot "Name+dependency analysis done"
          , cpStopAt CompilePoint_AnalHS
          ]

{-# LINE 800 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
{-|
Analyse a module for
  (3) types

-}
{-# LINE 805 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
cpEhcEhAnalyseModuleDefs :: HsName -> EHCompilePhase ()
cpEhcEhAnalyseModuleDefs modNm
  = cpSeq [ cpStepUID, cpProcessEH modNm
          , cpMsg modNm VerboseALot "Type analysis done"
          , cpStopAt CompilePoint_AnalEH
          ]

{-# LINE 814 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
{-|
Part 1 Core processing, on a per module basis, part1 is done always

-}
{-# LINE 818 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
cpEhcCorePerModulePart1 :: HsName -> EHCompilePhase ()
cpEhcCorePerModulePart1 modNm
  = do { cr <- get
       ; let (_,_,opts,_) = crBaseInfo modNm cr
             earlyMerge = ehcOptOptimizationScope opts >= OptimizationScope_WholeCore
       ; cpSeq
           (  [ cpStepUID ]
           ++ [ cpProcessCoreBasic modNm
              , cpMsg modNm VerboseALot "Core (basic) done"
              , when (not earlyMerge) $ cpProcessCoreRest modNm
              , cpStopAt CompilePoint_Core
              ]
           )
       }

{-# LINE 849 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
{-|
Part 2 Core processing, part2 is done either for individual modules or after full program analysis

-}
{-# LINE 853 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
cpEhcCorePerModulePart2 :: HsName -> EHCompilePhase ()
cpEhcCorePerModulePart2 modNm
  = do { cr <- get
       ; let (_,_,opts,_) = crBaseInfo modNm cr
             earlyMerge = ehcOptOptimizationScope opts >= OptimizationScope_WholeCore
       ; cpSeq [ when earlyMerge $ cpProcessCoreRest modNm
               , when (targetIsGrin (ehcOptTarget opts)) (cpProcessGrin modNm)
               ]
       }

{-# LINE 869 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
{-|
Core+grin processing, on a per module basis, may only be done when no full program analysis is done

-}
{-# LINE 873 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
cpEhcCoreGrinPerModuleDoneNoFullProgAnalysis :: EHCOpts -> Bool -> Bool -> Bool -> HsName -> EHCompilePhase ()
cpEhcCoreGrinPerModuleDoneNoFullProgAnalysis opts isMainMod isTopMod doMkExec modNm
  = cpSeq (  [ cpEhcCorePerModulePart2 modNm
             , cpMsg modNm VerboseDebug "cpFlowOptim"
             , cpFlowOptim modNm
             , cpCleanupGrin [modNm]
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

{-# LINE 898 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
{-|
Core+grin processing, on a per module basis, may only be done when full program analysis is done

-}
{-# LINE 902 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
cpEhcCoreGrinPerModuleDoneFullProgAnalysis :: HsName -> EHCompilePhase ()
cpEhcCoreGrinPerModuleDoneFullProgAnalysis modNm
  = cpSeq (  [ cpEhcCorePerModulePart2 modNm
             , cpEhcExecutablePerModule FinalCompile_Exec [] modNm
             , cpMsg modNm VerboseALot "Full Program Analysis (Core+Grin) done"
             ]
          )

{-# LINE 912 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
{-|
Make final executable code, either still partly or fully (i.e. also linking)

-}
{-# LINE 916 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
cpEhcExecutablePerModule :: FinalCompileHow -> [HsName] -> HsName -> EHCompilePhase ()
cpEhcExecutablePerModule how impModNmL modNm
  = cpSeq [ cpCompileWithGCC how impModNmL modNm
          , cpCompileJavaScript how impModNmL modNm
          ]

{-# LINE 936 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
cpProcessHs :: HsName -> EHCompilePhase ()
cpProcessHs modNm
  = cpSeq [ cpFoldHs modNm
          , cpFlowHsSem1 modNm
          , cpTranslateHs2EH modNm
          , cpCleanupHS modNm
          ]

{-# LINE 950 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
cpProcessEH :: HsName -> EHCompilePhase ()
cpProcessEH modNm
  = do { cr <- get
       ; let (_,_,opts,fp) = crBaseInfo modNm cr
       {-
             optsTr = opts { ehcOptTrace = \s x -> unsafePerformIO (do { s <- execStateT (do { cpMsg modNm VerboseALot ("EH>: " ++ s)
                                                                                             ; x `seq` cpMsg modNm VerboseALot ("EH<: " ++ s)
                                                                                             }) cr
                                                                       ; return x
                                                                       }) }
             -- optsTr = opts { ehcOptTrace = \s x -> unsafePerformIO (do { putCompileMsg VerboseALot (ehcOptVerbosity opts) ("EH: " ++ s) Nothing modNm fp ; return x }) }
             -- optsTr = opts { ehcOptTrace = \s x -> unsafePerformIO (do { putStrLn ("EH: " ++ s) ; return x }) }
             -- optsTr = opts { ehcOptTrace = trace }
       -- ; cpUpdStateInfo (\crsi -> crsi {crsiOpts = optsTr})
       ; cpUpdCU modNm (ecuStoreOpts optsTr)
       -}
       ; cpSeq [ cpFoldEH modNm
               , cpCleanupFoldEH modNm
               , cpFlowEHSem1 modNm
               , cpTranslateEH2Output modNm
               ,
                      cpTranslateEH2Core modNm
               , cpCleanupEH modNm
               ]
       }

{-# LINE 1012 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
-- unprocessed core -> folded core
cpProcessCoreBasic :: HsName -> EHCompilePhase ()
cpProcessCoreBasic modNm
  = do { cr <- get
       ; let (_,_,opts,_) = crBaseInfo modNm cr
       ; cpSeq [ cpTransformCore modNm
               , cpFlowHILamMp modNm
               -- , when (ehcOptEmitCore opts) (cpOutputCore True "" "core" modNm)
               , cpOutputCore True "" "core" modNm
               , cpProcessCoreFold modNm
               ]
        }

{-# LINE 1032 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
-- unfolded core -> folded core
-- (called on merged core, and on core directly generated from cached grin)
cpProcessCoreFold :: HsName -> EHCompilePhase ()
cpProcessCoreFold modNm
  = cpSeq [ cpFoldCore modNm
          , cpFlowCoreSem modNm
          ]

{-# LINE 1044 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
-- folded core -> grin, jazy, and the rest
cpProcessCoreRest :: HsName -> EHCompilePhase ()
cpProcessCoreRest modNm
  = do { cr <- get
       ; let (_,_,opts,_) = crBaseInfo modNm cr
       ; cpSeq (   [ cpTranslateCore2Grin modNm ]
                -- ++ (if ehcOptWholeProgHPTAnalysis opts then [ cpOutputGrin True "" modNm ] else [])
                ++ (if targetIsGrin (ehcOptTarget opts) then [ cpOutputGrin True "" modNm ] else [])
                ++ [ cpTranslateCore2JavaScript modNm ]
                ++ [ cpCleanupCore [modNm] ]
               )
       }


{-# LINE 1067 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
cpProcessGrin :: HsName -> EHCompilePhase ()
cpProcessGrin modNm
  = do { cr <- get
       ; let (_,_,opts,_) = crBaseInfo modNm cr
       ; cpSeq (   (if ehcOptDumpGrinStages opts then [cpOutputGrin False "-000-initial" modNm] else [])
                ++ [cpTransformGrin modNm]
                ++ (if ehcOptDumpGrinStages opts then [cpOutputGrin False "-099-final" modNm]  else [])
                ++ (if ehcOptEmitBytecode opts then [cpTranslateGrin2Bytecode modNm] else [])
               )
       }

{-# LINE 1083 "src/ehc/EHC/CompilePhase/TopLevelPhases.chs" #-}
cpProcessBytecode :: HsName -> EHCompilePhase ()
cpProcessBytecode modNm
  = do { cr <- get
       ; let (_,_,opts,_) = crBaseInfo modNm cr
       ; cpSeq [ cpMsg modNm VerboseALot "Translate ByteCode"
               , cpTranslateByteCode modNm
               , cpFlowHILamMp modNm
               , cpCleanupFoldBytecode modNm
               , when (ehcOptEmitBytecode opts)
                      (do { cpOutputByteCodeC "c" modNm
                          })
               , cpCleanupBytecode modNm
               ]
       }



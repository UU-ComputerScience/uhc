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
%%[8 import(qualified Data.Map as Map)
%%]

%%[8 import({%{EH}EHC.Common})
%%]
%%[8 import({%{EH}EHC.CompileUnit})
%%]
%%[8 import({%{EH}EHC.CompileRun})
%%]
%%[20 import({%{EH}EHC.CompileGroup})
%%]

%%[8 import({%{EH}EHC.CompilePhase.Parsers})
%%]
%%[8 import({%{EH}EHC.CompilePhase.Translations})
%%]
%%[8 import({%{EH}EHC.CompilePhase.Output})
%%]
%%[8 import({%{EH}EHC.CompilePhase.TransformCore},{%{EH}EHC.CompilePhase.TransformGrin})
%%]
%%[8 import({%{EH}EHC.CompilePhase.Semantics})
%%]
%%[8 import({%{EH}EHC.CompilePhase.FlowBetweenPhase})
%%]
%%[8 import({%{EH}EHC.CompilePhase.CompileC})
%%]
%%[(8 codegen grin) import({%{EH}EHC.CompilePhase.CompileLLVM})
%%]
%%[20 import({%{EH}EHC.CompilePhase.Module})
%%]
%%[99 import({%{EH}EHC.CompilePhase.Cleanup})
%%]

-- Language syntax: Core
%%[(20 codegen) import(qualified {%{EH}Core} as Core(cModMerge))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Per full program compile actions: level 6
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(20 codegen grin) haddock
Top level entry point into compilation by the compiler driver, apart from import analysis, wich is assumed to be done.
%%]

%%[20 export(cpEhcFullProgCompileAllModules)
cpEhcFullProgCompileAllModules :: EHCompilePhase ()
cpEhcFullProgCompileAllModules
 = do { cr <- get
      ; let modNmLL = crCompileOrder cr
            modNmL = map head modNmLL
            (_,opts) = crBaseInfo' cr
            Just (mm@(impModNmL,mainModNm)) = initlast modNmL
      ; cpSeq (   [cpEhcFullProgModuleCompileN modNmL]
%%[[(20 codegen grin)
               ++ (if ehcOptFullProgAnalysis opts
                   then [ cpEhcFullProgPostModulePhases opts modNmL mm
                        , cpEhcCorePerModulePart2 mainModNm
                        ]
                   else []
                  )
               ++ [cpEhcExecutablePerModule GCC_CompileExec impModNmL mainModNm]
%%]]
              )
      }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Per full program compile actions: level 5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(20 codegen grin) haddock
Post per module phases, as part of a full program compilation.
Post processing involves the following:
1. if doing full program analysis, merge the already compiled Core representations, and do the same work as for 1 module but now for the merged Core
2. compile+link everything together
%%]

%%[(20 codegen grin)
cpEhcFullProgPostModulePhases :: EHCOpts -> [HsName] -> ([HsName],HsName) -> EHCompilePhase ()
cpEhcFullProgPostModulePhases opts modNmL (impModNmL,mainModNm)
  = cpSeq [ cpSeq [cpGetPrevCore m | m <- modNmL]
          , mergeIntoOneBigCore
          , cpOutputCore "fullcore" mainModNm
          ]
  where mergeIntoOneBigCore
          = do { cr <- get
               ; cpUpdCU mainModNm (ecuStoreCore (Core.cModMerge [ panicJust "cpEhcFullProgPostModulePhases.mergeIntoOneBigCore" $ ecuMbCore $ crCU m cr
                                                                 | m <- modNmL
                                                                 ]
                                   )             )
               }
%%]

%%[20 haddock
Per module compilation of (import) ordered sequence of module, as part of a full program compilation
%%]

%%[20
cpEhcFullProgModuleCompileN :: [HsName] -> EHCompilePhase ()
cpEhcFullProgModuleCompileN modNmL
  = cpSeq (merge (map cpEhcFullProgModuleCompile1    modNmL)
                 (map cpEhcFullProgBetweenModuleFlow modNmL)
          )
  where merge (c1:cs1) (c2:cs2) = c1 : c2 : merge cs1 cs2
        merge []       cs       = cs
        merge cs       []       = cs
%%]

%%[20 haddock
Compilation of 1 module, as part of a full program compilation
%%]

%%[20
cpEhcFullProgModuleCompile1 :: HsName -> EHCompilePhase ()
cpEhcFullProgModuleCompile1 modNm
  = do { cr <- get
       ; let targ = if crModNeedsCompile modNm cr
%%[[101
                       && crModCanCompile modNm cr
%%]]
                    then HSAllSem
                    else HSAllSemHI
       ; cpSeq [cpEhcModuleCompile1 (Just targ) modNm]
       }
%%]

%%[20 haddock
Flow of info between modules, as part of a full program compilation
%%]

%%[20
cpEhcFullProgBetweenModuleFlow :: HsName -> EHCompilePhase ()
cpEhcFullProgBetweenModuleFlow modNm
  = do { cr <- get
       ; case ecuState $ crCU modNm cr of
           ECUSHaskell HSAllSem   -> return ()
           ECUSHaskell HSAllSemHI -> cpFlowHISem modNm
           _                      -> return ()
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
%%[20 -8.cpEhcModuleCompile1.sig export(cpEhcModuleCompile1)
cpEhcModuleCompile1 :: Maybe HSState -> HsName -> EHCompilePhase ()
cpEhcModuleCompile1 targHSState modNm
%%]
%%[8
  = do { cr <- get
       ; let (ecu,_,opts,fp) = crBaseInfo modNm cr
%%[[8
       ; case (ecuState ecu,undefined) of
%%][20
       ; case (ecuState ecu,targHSState) of
%%]]
%%]
%%[20
           (ECUSHaskell HSStart,Just HSOnlyImports)
             -> cpSeq [ cpMsg modNm VerboseNormal "Imports of Haskell"
                      , cpEhcHaskellModulePrepare modNm
                      , cpEhcHaskellImport HSOnlyImports modNm
                      , cpUpdCU modNm (ecuStoreState (ECUSHaskell HSOnlyImports))
                      ]
%%[[99
           (ECUSHaskell LHSStart,Just HSOnlyImports)
             -> cpSeq [ cpMsg modNm VerboseNormal "Imports of Literate Haskell"
                      , cpEhcHaskellModulePrepare modNm
                      , cpEhcHaskellImport LHSOnlyImports modNm
                      , cpUpdCU modNm (ecuStoreState (ECUSHaskell LHSOnlyImports))
                      ]
%%]]
           (ECUSHaskell HSOnlyImports,Just HSOnlyImports)
             -> return ()
%%[[99
           (ECUSHaskell LHSOnlyImports,Just HSOnlyImports)
             -> return ()
%%]]
           (ECUSHaskell HSOnlyImports,Just HSAllSem)
             -> cpSeq [ cpMsg modNm VerboseNormal "Compiling Haskell"
                      , cpEhcHaskellModuleAfterImport (ecuIsTopMod ecu) opts HSOnlyImports modNm
                      , cpUpdCU modNm (ecuStoreState (ECUSHaskell HSAllSem))
                      ]
%%[[99
           (ECUSHaskell LHSOnlyImports,Just HSAllSem)
             -> cpSeq [ cpMsg modNm VerboseNormal "Compiling Literate Haskell"
                      , cpEhcHaskellModuleAfterImport (ecuIsTopMod ecu) opts LHSOnlyImports modNm
                      , cpUpdCU modNm (ecuStoreState (ECUSHaskell HSAllSem))
                      ]
%%]]
           (ECUSHaskell st,Just HSAllSemHI)
             |    st == HSOnlyImports
%%[[99
               || st == LHSOnlyImports
%%]]
             -> do { cpMsg modNm VerboseNormal "Reading HI"
%%[[(20 codegen grin)
                   ; cpUpdateModOffMp [modNm]
%%]]
                   ; cpUpdCU modNm (ecuStoreState (ECUSHaskell HSAllSemHI))
                   }
%%]]
%%]
%%[8
           (ECUSHaskell HSStart,_)
             -> cpSeq (  [ cpMsg modNm VerboseNormal "Compiling Haskell"
                         , cpEhcHaskellModulePrepare modNm
                         , cpEhcHaskellParse True False modNm
                         , cpEhcHaskellModuleCommonPhases True True opts modNm
                         ]
                      ++ (if ehcOptFullProgAnalysis opts then [cpEhcCoreGrinPerModuleDoneFullProgAnalysis modNm] else [])
                      ++ [ cpUpdCU modNm (ecuStoreState (ECUSHaskell HSAllSem)) ]
                      )
%%[[20
           (_,Just HSOnlyImports)
             -> return ()
%%]]
           (ECUSEh EHStart,_)
             -> cpSeq (  [ cpMsg modNm VerboseNormal "Compiling EH"
                         , cpEhcEhParse modNm
%%[[20   
                         , cpGetDummyCheckEhMod modNm
%%]]   
                         , cpEhcEhModuleCommonPhases True True opts modNm
                         ]
                      ++ (if ehcOptFullProgAnalysis opts then [cpEhcCoreGrinPerModuleDoneFullProgAnalysis modNm] else [])
                      ++ [ cpUpdCU modNm (ecuStoreState (ECUSEh EHAllSem)) ]
                      )
%%[[(8 codegen grin)
           (ECUSGrin,_)
             -> cpSeq [ cpMsg modNm VerboseNormal "Compiling Grin"
                      , cpParseGrin modNm
                      , cpProcessGrin modNm
                      , cpProcessBytecode modNm 
                      ]
%%]]
           _ -> return ()
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Per module compile actions: level 3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 haddock
EH common phases: analysis + core + grin
%%]

%%[8
cpEhcEhModuleCommonPhases :: Bool -> Bool -> EHCOpts -> HsName -> EHCompilePhase ()
cpEhcEhModuleCommonPhases isTopMod doMkExec opts modNm
  = cpSeq ([ cpEhcEhAnalyseModuleDefs modNm
%%[[(8 codegen)
           , cpEhcCorePerModulePart1 modNm
%%]]
           ]
%%[[(8 codegen grin)
           ++ (if ehcOptFullProgAnalysis opts
               then []
               else [cpEhcCoreGrinPerModuleDoneNoFullProgAnalysis opts isTopMod doMkExec modNm]
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
          , cpEhcEhModuleCommonPhases isTopMod doMkExec opts modNm
          ]       
%%]

%%[20 haddock
Post module import common phases: Parse + Module analysis + HS common
%%]

%%[20
cpEhcHaskellModuleAfterImport :: Bool -> EHCOpts -> HSState -> HsName -> EHCompilePhase ()
cpEhcHaskellModuleAfterImport isTopMod opts hsst modNm
  = cpSeq [ cpEhcHaskellParse False (hsstateIsLiteral hsst) modNm
          , cpEhcHaskellAnalyseModuleItf modNm
          , cpEhcHaskellModuleCommonPhases isTopMod False opts modNm
          , cpEhcHaskellModulePostlude modNm
          ]       
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Per module compile actions: level 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 haddock
Prepare module for compilation.
This should be the first step before compilation of a module and is meant to obtain cached info from a previous compilation.
%%]

%%[8.cpEhcHaskellModulePrepare
cpEhcHaskellModulePrepare :: HsName -> EHCompilePhase ()
cpEhcHaskellModulePrepare _ = return ()
%%]

%%[20 -8.cpEhcHaskellModulePrepare
cpEhcHaskellModulePrepare :: HsName -> EHCompilePhase ()
cpEhcHaskellModulePrepare modNm
  = cpSeq [ cpGetMetaInfo modNm
          , cpGetPrevHI modNm
          , cpFoldHI modNm
          ]
%%]

%%[20
cpEhcHaskellModulePostlude :: HsName -> EHCompilePhase ()
cpEhcHaskellModulePostlude modNm
  = cpSeq [ cpOutputHI "hi" modNm
%%[[99
          , cpCleanupCU modNm
%%]]
          ]
%%]

%%[20 haddock
Get import information from module source text.
%%]

%%[20
cpEhcHaskellImport :: HSState -> HsName -> EHCompilePhase ()
cpEhcHaskellImport hsst modNm
  = cpSeq (
%%[[20
             [ cpParseHsImport modNm ]
%%][99
             [ cpPreprocessWithCPP modNm
             , cpParseHsImport (hsstateIsLiteral hsst) modNm
             ]
%%]]
          ++ [ cpStepUID
             , cpFoldHsMod modNm
             , cpGetHsImports modNm
             ]
          )
%%]

%%[20 haddock
Parse a Haskell module
%%]

%%[8
cpEhcHaskellParse :: Bool -> Bool -> HsName -> EHCompilePhase ()
cpEhcHaskellParse doCPP litmode modNm
  = cpSeq (
%%[[8
             [ cpParseHs modNm ]
%%][99
             (if doCPP then [cpPreprocessWithCPP modNm] else [])
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

%%[20 haddock
Analyse a module for
  (1) module information (import, export, etc),
%%]

%%[20
cpEhcHaskellAnalyseModuleItf :: HsName -> EHCompilePhase ()
cpEhcHaskellAnalyseModuleItf modNm
  = cpSeq [ cpStepUID, cpFoldHsMod modNm, cpGetHsMod modNm
%%[[99
          , cpCleanupHSMod modNm
%%]]
          , cpCheckMods [modNm]
%%[[(20 codegen grin)
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
  = cpSeq [ cpStepUID
          , cpProcessCoreBasic modNm
          , cpMsg modNm VerboseALot "Core (basic) done"
          , cpStopAt CompilePoint_Core
          ]
%%]

%%[(8 codegen) haddock
Part 2 Core processing, part2 is done either for individual modules or after full program analysis
%%]

%%[(8 codegen)
cpEhcCorePerModulePart2 :: HsName -> EHCompilePhase ()
cpEhcCorePerModulePart2 modNm
  = cpSeq [ cpProcessCoreRest modNm
          , cpProcessGrin modNm
          ]
%%]

%%[(8 codegen grin) haddock
Core+grin processing, on a per module basis, may only be done when no full program analysis is done
%%]

%%[(8 codegen grin)
cpEhcCoreGrinPerModuleDoneNoFullProgAnalysis :: EHCOpts -> Bool -> Bool -> HsName -> EHCompilePhase ()
cpEhcCoreGrinPerModuleDoneNoFullProgAnalysis opts isTopMod doMkExec modNm
  = cpSeq (  [ cpEhcCorePerModulePart2 modNm
%%[[20
             , cpFlowOptim modNm
%%]]
%%[[99
             , cpCleanupGrin modNm
%%]]
             , cpProcessBytecode modNm
             ]
          ++ (if not isTopMod || doMkExec
              then [cpEhcExecutablePerModule (if doMkExec then GCC_CompileExec else GCC_CompileOnly) [] modNm]
              else []
             )
          ++ [cpMsg modNm VerboseALot "Core+Grin done"]
          )
%%]

%%[(8 codegen grin) haddock
Core+grin processing, on a per module basis, may only be done when full program analysis is done
%%]

%%[(8 codegen grin)
cpEhcCoreGrinPerModuleDoneFullProgAnalysis :: HsName -> EHCompilePhase ()
cpEhcCoreGrinPerModuleDoneFullProgAnalysis modNm
  = cpSeq (  [ cpEhcCorePerModulePart2 modNm
             , cpEhcExecutablePerModule GCC_CompileExec [] modNm
             , cpMsg modNm VerboseALot "Full Program Analysis Core+Grin done"
             ]
          )
%%]

%%[(8 codegen grin) haddock
Make final executable code, either still partly or fully (i.e. also linking)
%%]

%%[(8 codegen grin)
cpEhcExecutablePerModule :: GCC_CompileHow -> [HsName] -> HsName -> EHCompilePhase ()
cpEhcExecutablePerModule how impModNmL modNm
  = cpSeq [ cpCompileWithGCC how impModNmL modNm
          , cpCompileWithLLVM modNm
          ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Per module compile actions: level 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
cpProcessHs :: HsName -> EHCompilePhase ()
cpProcessHs modNm 
  = cpSeq [ cpFoldHs modNm
%%[[20
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
  = cpSeq [ cpFoldEH modNm
%%[[99
          , cpCleanupFoldEH modNm
%%]]
          , cpFlowEHSem1 modNm
          , cpTranslateEH2Output modNm
%%[[(8 codegen)
          , cpTranslateEH2Core modNm
%%]]
%%[[99
          , cpCleanupEH modNm
%%]]
          ]
%%]

%%[(8 codegen)
cpProcessCoreBasic :: HsName -> EHCompilePhase ()
cpProcessCoreBasic modNm 
  = cpSeq [ cpTransformCore
              modNm
                (
%%[[102
                  -- [ "CS" ] ++
%%]]
                  [ "CER", "CRU", "CLU", "CILA", "CETA", "CCP", "CILA", "CETA"
                  , "CFL", "CLGA", "CCGA", "CLU", "CFL", {- "CLGA", -} "CLFG"    
%%[[9                  
                  ,  "CLDF"
%%]
%%[[8_2           
                  , "CPRNM"
%%]]
                  ]
                )
          , cpOutputCore "core" modNm
%%[[(8 java)
          , cpOutputJava "java" modNm
%%]]
          ]
%%]

%%[(8 codegen)
cpProcessCoreRest :: HsName -> EHCompilePhase ()
cpProcessCoreRest modNm 
  = cpSeq [ cpFoldCore modNm
%%[[20
          , cpFlowCoreSem modNm
%%]]
          , cpTranslateCore2Grin modNm
%%[[99
          , cpCleanupCore modNm
%%]]
          ]
          
%%]

%%[(8 codegen grin)
cpProcessGrin :: HsName -> EHCompilePhase ()
cpProcessGrin modNm 
  = do { cr <- get
       ; let (_,opts) = crBaseInfo' cr
       ; cpSeq ([ cpOutputGrin "-000-initial" modNm
                , cpTransformGrin modNm
                , cpOutputGrin "-099-final" modNm
                ]
                ++ (if ehcOptEmitBytecode opts then [cpTranslateGrin2Bytecode modNm] else [])
                ++ (if ehcOptFullProgAnalysis opts then [cpTranslateGrin modNm] else [])
               )
       }
%%]

%%[(8 codegen grin)
cpProcessBytecode :: HsName -> EHCompilePhase ()
cpProcessBytecode modNm 
  = cpSeq [ cpTranslateByteCode modNm
%%[[99
          , cpCleanupFoldBytecode modNm
%%]]
          , cpOutputByteCodeC "c" modNm
%%[[99
          , cpCleanupBytecode modNm
%%]]
          ]

%%]


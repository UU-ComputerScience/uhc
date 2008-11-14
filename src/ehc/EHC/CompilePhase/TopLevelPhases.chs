%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Top level combinations

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
%%[20 import({%{EH}EHC.CompilePhase.Module})
%%]
%%[99 import({%{EH}EHC.CompilePhase.Cleanup})
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: prepare for compilation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8.cpPrepareModuleForCompilation export(cpPrepareModuleForCompilation)
cpPrepareModuleForCompilation :: HsName -> EHCompilePhase ()
cpPrepareModuleForCompilation _ = return ()
%%]

%%[20 -8.cpPrepareModuleForCompilation export(cpPrepareModuleForCompilation)
cpPrepareModuleForCompilation :: HsName -> EHCompilePhase ()
cpPrepareModuleForCompilation modNm
  = cpSeq [cpGetMetaInfo modNm,cpGetPrevHI modNm,cpFoldHI modNm]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: top level processing combinators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(cpProcessHs)
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

%%[8 export(cpProcessEH)
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

%%[(8 codegen) export(cpProcessCoreBasic)
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

%%[(8 codegen) export(cpProcessCoreRest)
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

%%[(8 codegen grin) export(cpProcessGrin)
cpProcessGrin :: HsName -> EHCompilePhase ()
cpProcessGrin modNm 
  = cpSeq [ cpOutputGrin "-000-initial" modNm
          , cpTransformGrin modNm
          , cpOutputGrin "-099-final" modNm
          , cpTranslateGrin2Bytecode modNm
          , cpTranslateGrin modNm
          ]
%%]

%%[(8 codegen grin) export(cpProcessBytecode)
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


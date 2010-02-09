%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Cleanup between phases

%%[99 module {%{EH}EHC.CompilePhase.Cleanup}
%%]

-- general imports
%%[99 import({%{EH}EHC.Common})
%%]
%%[99 import({%{EH}EHC.CompileUnit})
%%]
%%[99 import({%{EH}EHC.CompileRun})
%%]

-- HI syntax and semantics
%%[99 import(qualified {%{EH}HI} as HI)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: cleanup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(cpCleanupHSMod,cpCleanupHS,cpCleanupFoldEH,cpCleanupEH)
cpCleanupHSMod :: HsName -> EHCompilePhase ()
cpCleanupHSMod modNm
  = cpUpdCU modNm
      (\e -> e { ecuMbHSSemMod     	  = Nothing
               }
      )

cpCleanupHS :: HsName -> EHCompilePhase ()
cpCleanupHS modNm
  = cpUpdCU modNm
      (\e -> e { ecuMbHS              = Nothing
               , ecuMbHSSem           = Nothing
               }
      )

cpCleanupFoldEH :: HsName -> EHCompilePhase ()
cpCleanupFoldEH modNm 
  = cpUpdCU modNm
      (\e -> e { ecuMbEH              = Nothing
               }
      )

cpCleanupEH :: HsName -> EHCompilePhase ()
cpCleanupEH modNm
  = cpUpdCU modNm
      (\e -> e { ecuMbEHSem           = Nothing
               }
      )
%%]

%%[(99 codegen) export(cpCleanupCore)
cpCleanupCore :: HsName -> EHCompilePhase ()
cpCleanupCore modNm
  = cpUpdCU modNm
      (\e -> e { ecuMbCore            = Nothing
               , ecuMbTyCore          = Nothing
               , ecuMbCoreSem         = Nothing
               -- , ecuMbTyCoreSem       = Nothing
               }
      )
%%]

%%[(99 codegen grin) export(cpCleanupGrin,cpCleanupFoldBytecode,cpCleanupBytecode)
cpCleanupGrin :: HsName -> EHCompilePhase ()
cpCleanupGrin modNm
  = cpUpdCU modNm
      (\e -> e { ecuMbGrin            = Nothing
               }
      )

cpCleanupFoldBytecode :: HsName -> EHCompilePhase ()
cpCleanupFoldBytecode modNm
  = cpUpdCU modNm
      (\e -> e { ecuMbBytecode          = Nothing
               }
      )

cpCleanupBytecode :: HsName -> EHCompilePhase ()
cpCleanupBytecode modNm
  = cpUpdCU modNm
      (\e -> e { ecuMbBytecodeSem       = Nothing
               }
      )
%%]

%%[99 export(cpCleanupCU,cpCleanupFlow)
cpCleanupCU :: HsName -> EHCompilePhase ()
cpCleanupCU modNm
  = do { cpUpdCU modNm
           (\e -> e { ecuHIInfo            = HI.emptyHIInfo
                    , ecuMbOptim           = Nothing
                    }
           )
%%[[(99 codegen grin)
       -- Only cleanup Grin when we don't need to merge it.
       -- TODO think about this a bit longer.
       ; cr <- get
       ; let (_,opts) = crBaseInfo' cr
       ; when (not $ ehcOptFullProgAnalysis opts) $ cpCleanupGrin modNm
%%]]
       }

cpCleanupFlow :: HsName -> EHCompilePhase ()
cpCleanupFlow modNm
  = cpUpdCU modNm
      (\e -> e { ecuMbHSSemMod        = Nothing
               , ecuMbPrevHI          = Nothing
               , ecuMbPrevHISem       = Nothing
               }
      )
%%]


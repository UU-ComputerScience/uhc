%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Cleanup between phases

%%[99 module {%{EH}EHC.CompilePhase.Cleanup}
%%]

%%[99 import({%{EH}Base.Optimize})
%%]

-- general imports
%%[99 import({%{EH}EHC.Common})
%%]
%%[99 import({%{EH}EHC.CompileUnit})
%%]
%%[99 import({%{EH}EHC.CompileRun})
%%]

%%[99 import(Control.Monad.State)
%%]

-- HI syntax and semantics
%%[9999 import(qualified {%{EH}HI} as HI)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: cleanup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(cpCleanupHSMod,cpCleanupHS,cpCleanupFoldEH,cpCleanupEH)
cpCleanupHSMod :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpCleanupHSMod modNm
  = cpUpdCU modNm
      (\e -> e { _ecuMbHSSemMod     	  = Nothing
               }
      )

cpCleanupHS :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpCleanupHS modNm
  = cpUpdCU modNm
      (\e -> e { _ecuMbHS              = Nothing
               , _ecuMbHSSem           = Nothing
               }
      )

cpCleanupFoldEH :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpCleanupFoldEH modNm 
  = cpUpdCU modNm
      (\e -> e { _ecuMbEH              = Nothing
               }
      )

cpCleanupEH :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpCleanupEH modNm
  = cpUpdCU modNm
      (\e -> e { _ecuMbEHSem           = Nothing
               }
      )
%%]

%%[(99 codegen) export(cpCleanupCore)
cpCleanupCore :: EHCCompileRunner m => [HsName] -> EHCompilePhaseT m ()
cpCleanupCore modNmL
  = cpSeq [cl m | m <- modNmL]
  where cl m = cpUpdCU m
                  (\e -> e { _ecuMbCore            = Nothing
%%[[(99 core grin)
                           , _ecuMbCoreSem         = Nothing
%%]]
%%[[(99 corein)
                           , _ecuMbCoreSemMod      = Nothing
%%]]
                           }
                  )
%%]

%%[(99 codegen cmm) export(cpCleanupCmm)
cpCleanupCmm :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpCleanupCmm modNm
  = cpUpdCU modNm
      (\e -> e { _ecuMbCmm               = Nothing
               }
      )
%%]

%%[(99 codegen grin) export(cpCleanupGrin,cpCleanupFoldBytecode,cpCleanupBytecode)
cpCleanupGrin :: EHCCompileRunner m => [HsName] -> EHCompilePhaseT m ()
cpCleanupGrin modNmL
  = cpSeq [cl m | m <- modNmL]
  where cl m = cpUpdCU m
                  (\e -> e { _ecuMbGrin            = Nothing
                           }
                  )

cpCleanupFoldBytecode :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpCleanupFoldBytecode modNm
  = cpUpdCU modNm
      (\e -> e { ecuMbBytecode          = Nothing
               }
      )

cpCleanupBytecode :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpCleanupBytecode modNm
  = cpUpdCU modNm
      (\e -> e { ecuMbBytecodeSem       = Nothing
               }
      )
%%]

%%[99 export(cpCleanupCU,cpCleanupFlow)
cpCleanupCU :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpCleanupCU modNm
  = do { cpUpdCU modNm
           (\e -> e { ecuMbOptim           = Nothing
                    -- , _ecuHIInfo            = {- HI.hiiRetainAfterCleanup -} (_ecuHIInfo e)
                    }
           )
%%[[(99 codegen grin)
       -- Only cleanup Grin when we don't need to merge it.
       -- TODO think about this a bit longer.
       ; cr <- get
       ; let (_,opts) = crBaseInfo' cr
       ; when (ehcOptOptimizationScope opts < OptimizationScope_WholeGrin) $ cpCleanupGrin [modNm]
%%]]
       }

cpCleanupFlow :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpCleanupFlow modNm
  = cpUpdCU modNm
      (\e -> e { _ecuMbHSSemMod        = Nothing
               -- , ecuMbPrevHI          = Nothing
               -- , ecuMbPrevHISem       = Nothing
               -- , _ecuMbPrevHIInfo      = Nothing
               }
      )
%%]


module EH101.EHC.CompilePhase.Cleanup
( cpCleanupHSMod, cpCleanupHS, cpCleanupFoldEH, cpCleanupEH
, cpCleanupCore
, cpCleanupGrin, cpCleanupFoldBytecode, cpCleanupBytecode
, cpCleanupCU, cpCleanupFlow )
where
import EH101.Base.Optimize
import EH101.EHC.Common
import EH101.EHC.CompileUnit
import EH101.EHC.CompileRun
import qualified EH101.HI as HI

{-# LINE 30 "src/ehc/EHC/CompilePhase/Cleanup.chs" #-}
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

{-# LINE 61 "src/ehc/EHC/CompilePhase/Cleanup.chs" #-}
cpCleanupCore :: [HsName] -> EHCompilePhase ()
cpCleanupCore modNmL
  = cpSeq [cl m | m <- modNmL]
  where cl m = cpUpdCU m
                  (\e -> e { ecuMbCore            = Nothing
                           , ecuMbCoreSem         = Nothing
                           }
                  )

{-# LINE 84 "src/ehc/EHC/CompilePhase/Cleanup.chs" #-}
cpCleanupGrin :: [HsName] -> EHCompilePhase ()
cpCleanupGrin modNmL
  = cpSeq [cl m | m <- modNmL]
  where cl m = cpUpdCU m
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

{-# LINE 108 "src/ehc/EHC/CompilePhase/Cleanup.chs" #-}
cpCleanupCU :: HsName -> EHCompilePhase ()
cpCleanupCU modNm
  = do { cpUpdCU modNm
           (\e -> e { ecuHIInfo            = {- HI.hiiRetainAfterCleanup -} (ecuHIInfo e)
                    , ecuMbOptim           = Nothing
                    }
           )
       -- Only cleanup Grin when we don't need to merge it.
       -- TODO think about this a bit longer.
       ; cr <- get
       ; let (_,opts) = crBaseInfo' cr
       ; when (ehcOptOptimizationScope opts < OptimizationScope_WholeGrin) $ cpCleanupGrin [modNm]
       }

cpCleanupFlow :: HsName -> EHCompilePhase ()
cpCleanupFlow modNm
  = cpUpdCU modNm
      (\e -> e { ecuMbHSSemMod        = Nothing
               -- , ecuMbPrevHI          = Nothing
               -- , ecuMbPrevHISem       = Nothing
               -- , ecuMbPrevHIInfo      = Nothing
               }
      )


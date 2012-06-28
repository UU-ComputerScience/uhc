module EH101.EHC.CompilePhase.Transformations
( cpTransformCore )
where
import qualified Data.Map as Map
import qualified Data.Set as Set
import EH101.EHC.Common
import EH101.EHC.CompileUnit
import EH101.EHC.CompileRun
import EH101.Core.Trf
import EH101.EHC.CompilePhase.Output(cpOutputCoreModule)
import qualified EH101.Core.ToGrin as Core2GrSem
import qualified EH101.HI as HI
import EH101.EHC.CompilePhase.Module(cpUpdHiddenExports)






{-# LINE 52 "src/ehc/EHC/CompilePhase/Transformations.chs" #-}
cpTransformCore :: HsName -> EHCompilePhase ()
cpTransformCore modNm
  = do { cr <- get
       ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
       ; cpMsg' modNm VerboseALot "Transforming Core ..." Nothing fp

         -- transform
       ; let  mbCore     = ecuMbCore ecu
              coreInh    = crsiCoreInh crsi
              trfcoreIn  = emptyTrfCore
                             { trfcoreCore          = panicJust "cpTransformCore" mbCore
                             , trfcoreUniq          = crsiNextUID crsi
                             , trfcoreExpNmOffMp    = crsiExpNmOffMp modNm crsi
                             , trfcoreInhLamMp      = Core2GrSem.lamMp_Inh_CodeAGItf $ crsiCoreInh crsi
                             }
              trfcoreOut = trfCore opts (Core2GrSem.dataGam_Inh_CodeAGItf $ crsiCoreInh crsi) modNm trfcoreIn

         -- put back result: Core
       ; cpUpdCU modNm $! ecuStoreCore (trfcoreCore trfcoreOut)

         -- dump intermediate stages, if any
       ; cpSeq [ cpOutputCoreModule False ("-" ++ show n ++ "-" ++ nm) "core" modNm c
               | (n,(nm,c)) <- zip [1..] (trfcoreCoreStages trfcoreOut)
               ]

         -- put back result: unique counter
       ; cpSetUID (trfcoreUniq trfcoreOut)

         -- put back result: call info map (lambda arity, ...)
       ; let hii   = ecuHIInfo ecu
             lamMp = HI.hiiLamMp hii
       ; cpUpdCU modNm
           ( ecuStoreHIInfo
               (hii { HI.hiiLamMp = trfcoreGathLamMp trfcoreOut `Map.union` lamMp
                    })
           )
         -- put back result: additional hidden exports, it should be in a cpFlowXX variant
       ; cpUpdHiddenExports modNm $ zip (Set.toList $ trfcoreExtraExports trfcoreOut) (repeat IdOcc_Val)
       }


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Interface/wrapper to various transformations for Core, TyCore, etc.
%%]

%%[8 module {%{EH}EHC.CompilePhase.Transformations}
%%]

-- general imports
%%[8 import(qualified Data.Map as Map, qualified Data.Set as Set)
%%]

%%[8 import({%{EH}EHC.Common})
%%]
%%[8 import({%{EH}EHC.CompileUnit})
%%]
%%[8 import({%{EH}EHC.CompileRun})
%%]
%%[99 import({%{EH}EHC.CompilePhase.Module(cpUpdHiddenExports)})
%%]

-- Core transformations
%%[(8 codegen) import({%{EH}Core.Trf})
%%]

-- TyCore transformations
%%[(8 codegen tycore) import({%{EH}TyCore.Trf})
%%]

-- Output
%%[8 import({%{EH}EHC.CompilePhase.Output(cpOutputCoreModule)})
%%]
%%[(8 tycore) import({%{EH}EHC.CompilePhase.Output(cpOutputTyCoreModule)})
%%]

-- HI syntax and semantics
%%[50 import(qualified {%{EH}HI} as HI)
%%]

-- Core semantics
%%[(8 codegen grin) import(qualified {%{EH}Core.ToGrin} as Core2GrSem)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: transformations, on core
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(cpTransformCore)
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
%%[[50
                             , trfcoreExpNmOffMp    = crsiExpNmOffMp modNm crsi
                             , trfcoreInhLamMp      = Core2GrSem.lamMp_Inh_CodeAGItf $ crsiCoreInh crsi
%%]]
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

%%[[50
         -- put back result: call info map (lambda arity, ...)
       ; let hii   = ecuHIInfo ecu
             lamMp = HI.hiiLamMp hii
       ; cpUpdCU modNm
           ( ecuStoreHIInfo
               (hii { HI.hiiLamMp = trfcoreGathLamMp trfcoreOut `Map.union` lamMp
                    })
           )
%%]]   
%%[[99
         -- put back result: additional hidden exports, it should be in a cpFlowXX variant
       ; cpUpdHiddenExports modNm $ zip (Set.toList $ trfcoreExtraExports trfcoreOut) (repeat IdOcc_Val)
%%]]
       }
%%]


%%[(8 codegen tycore) export(cpTransformTyCore)
cpTransformTyCore :: HsName -> EHCompilePhase ()
cpTransformTyCore modNm
  = do { cr <- get
       ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
       ; cpMsg' modNm VerboseALot "Transforming TyCore ..." Nothing fp
       
         -- transform
       ; let  mbTyCore    = ecuMbTyCore ecu
              trftycoreIn = emptyTrfTyCore
                              { trftycoreTyCore        = panicJust "cpTransformTyCore" mbTyCore
                              , trftycoreUniq          = crsiNextUID crsi
%%[[50
                              , trftycoreExpNmOffMp    = crsiExpNmOffMp modNm crsi
%%]]
%%[[99
                              , trftycoreInhLamMp      = Core2GrSem.lamMp_Inh_CodeAGItf $ crsiCoreInh crsi
%%]]
                              }
              trftycoreOut = trfTyCore opts modNm trftycoreIn
       
         -- put back result: TyCore
       ; cpUpdCU modNm $! ecuStoreTyCore (trftycoreTyCore trftycoreOut)

         -- dump intermediate stages, if any
       ; cpSeq [ cpOutputTyCoreModule False ("-" ++ show n ++ "-" ++ nm) "tycore" modNm c
               | (n,(nm,c)) <- zip [1..] (trftycoreTyCoreStages trftycoreOut)
               ]

         -- put back result: unique counter
       ; cpSetUID (trftycoreUniq trftycoreOut)

%%[[99
         -- put back result: call info map (lambda arity, ...)
       ; let hii   = ecuHIInfo ecu
             lamMp = HI.hiiLamMp hii
       ; cpUpdCU modNm
           ( ecuStoreHIInfo
               (hii { HI.hiiLamMp = trftycoreGathLamMp trftycoreOut `Map.union` lamMp
                    })
           )
       
         -- put back result: additional hidden exports, it should be in a cpFlowXX variant
       ; cpUpdHiddenExports modNm $ zip (Set.toList $ trftycoreExtraExports trftycoreOut) (repeat IdOcc_Val)
%%]]
       }
%%]



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

%%[8 import(Control.Monad.State)
%%]

%%[8 import({%{EH}EHC.Common})
%%]
%%[(8 codegen) import({%{EH}Base.Optimize})
%%]
%%[8 import({%{EH}EHC.CompileUnit})
%%]
%%[8 import({%{EH}EHC.CompileRun})
%%]
%%[99 import({%{EH}EHC.CompilePhase.Module(cpUpdHiddenExports)})
%%]
%%[8 import(qualified {%{EH}Config} as Cfg)
%%]

-- Transformation utils
%%[(8 codegen) import({%{EH}CodeGen.TrfUtils})
%%]

-- Core transformations
%%[(8 codegen) import({%{EH}Core.Trf})
%%]

-- TyCore transformations
%%[(8 codegen tycore) import({%{EH}TyCore.Trf})
%%]

-- JavaScript transformations
%%[(8 javascript) import({%{EH}JavaScript.Trf})
%%]

-- Cmm transformations
%%[(8 codegen cmm) import({%{EH}Cmm.Trf})
%%]

-- Output
%%[8 import({%{EH}EHC.CompilePhase.Output})
%%]

-- HI syntax and semantics
%%[50 import(qualified {%{EH}HI} as HI)
%%]

-- Core semantics
%%[(8 core) import(qualified {%{EH}Core.ToGrin} as Core2GrSem)
%%]
%%[(50 codegen corein) import(qualified {%{EH}Core.Check} as Core2ChkSem)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: transformations, on core
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(cpTransformCore)
cpTransformCore :: OptimizationScope -> HsName -> EHCompilePhase ()
cpTransformCore optimScope modNm
  = do { cr <- get
       ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
       ; cpMsg' modNm VerboseALot "Transforming Core ..." Nothing fp
       
         -- transform
       ; let  mbCore     = ecuMbCore ecu
              coreInh    = crsiCoreInh crsi
              trfcoreIn  = emptyTrfCore
                             { trfstMod             	= panicJust "cpTransformCore" mbCore
                             , trfstUniq            	= crsiNextUID crsi
                             , trfstExtra = emptyTrfCoreExtra
                                 { trfcoreECUState		= ecuState ecu
%%[[8
                                 , trfcoreIsLamLifted	= False
%%][(50 corein)
                                 , trfcoreIsLamLifted	= maybe False Core2ChkSem.isLamLifted_Syn_CodeAGItf $ ecuMbCoreSemMod ecu
%%]]
%%[[50
                                 , trfcoreExpNmOffMp    = crsiExpNmOffMpDbg "cpTransformCore" modNm crsi
								 , trfcoreInhLamMp      = Core2GrSem.lamMp_Inh_CodeAGItf $ crsiCoreInh crsi
%%]]
                                 }
                             }
              trfcoreOut = trfCore opts optimScope (Core2GrSem.dataGam_Inh_CodeAGItf $ crsiCoreInh crsi) modNm trfcoreIn
       
%%[[(50 corein)
       -- ; lift $ putStrLn $ "cpTransformCore trfcoreIsLamLifted: " ++ show (trfcoreIsLamLifted $ trfstExtra trfcoreIn)
%%]]
         -- put back result: Core
       ; cpUpdCU modNm $! ecuStoreCore (trfstMod trfcoreOut)

         -- put back result: unique counter
       ; cpSetUID (trfstUniq trfcoreOut)

%%[[50
         -- put back result: call info map (lambda arity, ...)
       ; let hii   = ecuHIInfo ecu
             lamMp = HI.hiiLamMp hii
       ; cpUpdCU modNm
           ( ecuStoreHIInfo
               (hii { HI.hiiLamMp = (trfcoreGathLamMp $ trfstExtra trfcoreOut) `Map.union` lamMp
                    })
           )
%%]]   
%%[[99
         -- put back result: additional hidden exports, it should be in a cpFlowXX variant
       ; cpUpdHiddenExports modNm $ zip (Set.toList $ trfcoreExtraExports $ trfstExtra trfcoreOut) (repeat IdOcc_Val)
%%]]

         -- dump intermediate stages, print errors, if any
       ; let (nms,mcs,errs) = unzip3 $ trfstModStages trfcoreOut
       ; cpOutputCoreModules CPOutputCoreHow_Text [{- CoreOpt_DumpAlsoNonParseable -}] (\n nm -> "-" ++ show optimScope ++ "-" ++ show n ++ "-" ++ nm) Cfg.suffixDotlessOutputTextualCore modNm [ (n,nm) | (n, Just nm) <- zip nms mcs ]
       ; cpSeq $ zipWith (\nm err -> cpSetLimitErrsWhen 5 ("Core errors: " ++ nm) err) nms errs
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


%%[(8 javascript) export(cpTransformJavaScript)
cpTransformJavaScript :: OptimizationScope -> HsName -> EHCompilePhase ()
cpTransformJavaScript optimScope modNm
  = do { cr <- get
       ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
       ; cpMsg' modNm VerboseALot "Transforming JavaScript ..." Nothing fp
       
         -- transform
       ; let  mbJavaScript     = ecuMbJavaScript ecu
              trfjsIn  = emptyTrfJavaScript
                             { trfstMod           = panicJust "cpTransformJavaScript" mbJavaScript
                             , trfstUniq          = crsiNextUID crsi
                             }
              trfjsOut = trfJavaScript opts optimScope modNm trfjsIn
       
         -- put back result: JavaScript
       ; cpUpdCU modNm $! ecuStoreJavaScript (trfstMod trfjsOut)

         -- put back result: unique counter
       ; cpSetUID (trfstUniq trfjsOut)

         -- dump intermediate stages, print errors, if any
       ; let (nms,mcs,errs) = unzip3 $ trfstModStages trfjsOut
       ; cpOutputJavaScriptModules False (\n nm -> "-" ++ show n ++ "-" ++ nm) Cfg.suffixJavaScriptLib modNm [ (n,nm) | (n, Just nm) <- zip nms mcs ]
       ; cpSeq $ zipWith (\nm err -> cpSetLimitErrsWhen 5 ("JavaScript errors: " ++ nm) err) nms errs
       }
%%]


%%[(8 codegen cmm) export(cpTransformCmm)
cpTransformCmm :: OptimizationScope -> HsName -> EHCompilePhase ()
cpTransformCmm optimScope modNm
  = do { cr <- get
       ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
       ; cpMsg' modNm VerboseALot "Transforming Cmm ..." Nothing fp
       
         -- transform
       ; let  mbCmm     = ecuMbCmm ecu
              trfcmmIn  = emptyTrfCmm
                             { trfstMod           = panicJust "cpTransformCmm" mbCmm
                             , trfstUniq          = crsiNextUID crsi
                             }
              trfcmmOut = trfCmm opts optimScope modNm trfcmmIn
       
         -- put back result: Cmm
       ; cpUpdCU modNm $! ecuStoreCmm (trfstMod trfcmmOut)

         -- put back result: unique counter
       ; cpSetUID (trfstUniq trfcmmOut)

         -- dump intermediate stages, print errors, if any
       ; let (nms,mcs,errs) = unzip3 $ trfstModStages trfcmmOut
       ; cpOutputCmmModules False (\n nm -> "-" ++ show n ++ "-" ++ nm) Cfg.suffixCmmLib modNm [ (n,nm) | (n, Just nm) <- zip nms mcs ]
       ; cpSeq $ zipWith (\nm err -> cpSetLimitErrsWhen 5 ("Cmm errors: " ++ nm) err) nms errs
       }
%%]


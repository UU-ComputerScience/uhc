%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Interface/wrapper to various transformations for Core, Grin, etc.
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

-- Output
%%[8 import({%{EH}EHC.CompilePhase.Output(cpOutputCoreModule)})
%%]

-- HI syntax and semantics
%%[20 import(qualified {%{EH}HI} as HI)
%%]

-- Core semantics
%%[(99 codegen grin) import( {%{EH}Core.ToGrin(Inh_CodeAGItf(..))})
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
%%[[20
                             , trfcoreExpNmOffMp    = crsiExpNmOffMp modNm crsi
%%]]
%%[[99
                             , trfcoreInhLamMp      = lamMp_Inh_CodeAGItf $ crsiCoreInh crsi
%%]]
                             }
              trfcoreOut = trfCore opts modNm trfcoreIn
       
         -- put back result: Core
       ; cpUpdCU modNm $! ecuStoreCore (trfcoreCore trfcoreOut)

         -- dump intermediate stages, if any
       ; cpSeq [ cpOutputCoreModule False ("-" ++ show n ++ "-" ++ nm) "core" modNm c
               | (n,(nm,c)) <- zip [1..] (trfcoreCoreStages trfcoreOut)
               ]

         -- put back result: unique counter
       ; cpSetUID (trfcoreUniq trfcoreOut)

%%[[99
         -- put back result: call info map (lambda arity, ...)
       ; let hii   = ecuHIInfo ecu
             lamMp = HI.hiiLamMp hii
       ; cpUpdCU modNm
           ( ecuStoreHIInfo
               (hii { HI.hiiLamMp = trfcoreGathLamMp trfcoreOut `Map.union` lamMp
                    })
           )
       
         -- put back result: additional hidden exports, it should be in a cpFlowXX variant
       ; cpUpdHiddenExports modNm (Set.toList $ trfcoreExtraExports trfcoreOut)
%%]]
       }
%%]




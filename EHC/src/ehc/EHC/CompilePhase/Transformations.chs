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
%%[99 import({%{EH}Module(modMpAddHiddenExps)},{%{EH}EHC.CompilePhase.Module(cpUpdateModOffMp)})
%%]

-- Output
%%[8 import({%{EH}EHC.CompilePhase.Output(cpOutputCoreModule)})
%%]

-- Core semantics
%%[(99 codegen grin) import( {%{EH}Core.ToGrin(Inh_CodeAGItf(..))})
%%]
-- Core transformations
%%[(8 codegen) import({%{EH}Core.Trf})
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
                             , trfcoreInhCLamCallMp = clamCallMp_Inh_CodeAGItf $ crsiCoreInh crsi
%%]]
                             }
              trfcoreOut = trfCore opts modNm trfcoreIn
       
         -- put back result: Core
       ; cpUpdCU modNm $! ecuStoreCore (trfcoreCore trfcoreOut)

         -- dump intermediate stages, if any
       ; cpSeq [ cpOutputCoreModule ("-" ++ show n ++ "-" ++ nm) "core" modNm c
               | (n,(nm,c)) <- zip [1..] (trfcoreCoreStages trfcoreOut)
               ]

         -- put back result: unique counter
       ; cpSetUID (trfcoreUniq trfcoreOut)

%%[[99
         -- put back result: call info map (lambda arity, ...)
       ; cpUpdSI (\crsi -> let inh = crsiCoreInh crsi
                               m   = trfcoreGathCLamCallMp trfcoreOut `Map.union` clamCallMp_Inh_CodeAGItf inh
                           in  crsi {crsiCoreInh = inh {clamCallMp_Inh_CodeAGItf = m}}
                 )

         -- put back result: additional hidden exports
       ; when (not $ Set.null $ trfcoreExtraExports trfcoreOut)
              (do { cpUpdSI (\crsi -> crsi { crsiModMp = modMpAddHiddenExps modNm (Set.toList $ trfcoreExtraExports trfcoreOut) $ crsiModMp crsi
                                           })
                  ; cpUpdateModOffMp [modNm]
                  })
%%]]
       }
%%]




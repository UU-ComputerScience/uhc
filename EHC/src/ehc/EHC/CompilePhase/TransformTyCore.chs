%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

TyCore transformations

%%[8 module {%{EH}EHC.CompilePhase.TransformTyCore}
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

-- Output
%%[8 import({%{EH}EHC.CompilePhase.Output(cpOutputCore)})
%%]

-- Core transformations
%%[(8 codegen) import({%{EH}TyCore.Trf.FlipStrictness})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: transformations, on core
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen)
cpTyCore1Trf :: HsName -> String -> EHCompilePhase ()
cpTyCore1Trf modNm trfNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbCore = ecuMbTyCore ecu
                 tyCore = panicJust "cpCore1Trf" mbCore
                 u1     = uidChild $ crsiHereUID $ crsi
                 core2  = ( case trfNm of
%%[[(8 tauphi)
                              "FLSN"    -> cmodTrfFlipStrictness
%%]]
                              _         -> id
                          ) tyCore
         ;  cpMsg' modNm VerboseALot "Transforming" (lookup trfNm cmdLineTrfs) fp
         ;  cpUpdCU modNm $! ecuStoreTyCore $! core2
         }
%%]

%%[(8 codegen) export(cpTransformTyCore)
cpTransformTyCore :: HsName -> [String] -> EHCompilePhase ()
cpTransformTyCore modNm trfNmL
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 tr     = ehcOptTrf opts
                 ps     = dmp 0 "fromeh"
                          : (concat
                            $ intersperse [cpStepUID]
                            $ zipWith trf [1..]
                            $ filter (maybe True id . trfOptOverrides tr)
                            $ trfNmL
                            )
                 trf n t= [cpTyCore1Trf modNm t,dmp n t]
                 dmp n t= when (ehcOptDumpCoreStages opts) (cpOutputCore ("tycore-" ++ show n ++ "-" ++ t) modNm)
         ;  cpSeq ps
         }
%%]


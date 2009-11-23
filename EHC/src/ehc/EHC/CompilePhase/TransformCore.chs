%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Core transformations

%%[8 module {%{EH}EHC.CompilePhase.TransformCore}
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
%%[(8 codegen) import({%{EH}Core.Trf.RenUniq}, {%{EH}Core.Trf.ANormal}, {%{EH}Core.Trf.InlineLetAlias}, {%{EH}Core.Trf.LetUnrec})
%%]
%%[(8 codegen) import({%{EH}Core.Trf.LamGlobalAsArg}, {%{EH}Core.Trf.CAFGlobalAsArg}, {%{EH}Core.Trf.FloatToGlobal}, {%{EH}Core.Trf.ConstProp})
%%]
%%[(8 codegen) import({%{EH}Core.Trf.EtaRed}, {%{EH}Core.Trf.ElimTrivApp}, {%{EH}Core.Trf.FindNullaries})
%%]
%%[(8 codegen) import({%{EH}Core.Trf.AnnBasedSimplify})
%%]
%%[(9 codegen) import({%{EH}Core.Trf.LiftDictFields})
%%]
%%[(8_2 codegen) import({%{EH}Core.Trf.PrettyVarNames})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: transformations, on core
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen)
cpCore1Trf :: HsName -> String -> EHCompilePhase ()
cpCore1Trf modNm trfNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbCore = ecuMbCore ecu
                 core   = panicJust "cpCore1Trf" mbCore
                 u1     = uidChild $ crsiHereUID $ crsi
                 core2  = ( case trfNm of
                              "CER"     -> cmodTrfEtaRed
                              "CETA"    -> cmodTrfElimTrivApp opts
                              "CCP"     -> cmodTrfConstProp opts
                              "CRU"     -> cmodTrfRenUniq
                              "CLU"     -> cmodTrfLetUnrec
                              "CILA"    -> cmodTrfInlineLetAlias
%%[[20
                                             (Map.keysSet $ crsiExpNmOffMp modNm crsi)
%%]]
                              "CFL"     -> cmodTrfANormal modNm u1
                              "CLGA"    -> cmodTrfLamGlobalAsArg
                              "CCGA"    -> cmodTrfCAFGlobalAsArg
                              "CLFG"    -> cmodTrfFloatToGlobal
                              "CFN"     -> if   ehcOptFullProgAnalysis opts
                                           then cmodTrfFindNullaries
                                           else id
                              "CTBS"    -> cmodTrfAnnBasedSimplify opts
%%[[9
                              "CLDF"    -> if   ehcOptFullProgAnalysis opts
                                           then cmodTrfLiftDictFields
                                           else id
%%]]
%%[[8_2
                              "CPRNM"   -> cmodTrfPrettyNames
%%]]
%%[[102
                              "CS"      -> cmodTrfStrip
%%]]
                              -- "CLL"     -> cmodTrfLamLift
                              _         -> id
                          ) core
         ;  cpMsg' modNm VerboseALot "Transforming" (lookup trfNm cmdLineTrfs) fp
%%[[102
%%]]
         ;  cpUpdCU modNm $! ecuStoreCore $! core2
         }
%%]
         ;  cpMsg  modNm VerboseDebug ("Core sz1: " ++ showSizeCore core)
         ;  cpMsg  modNm VerboseDebug ("Core sz2: " ++ showSizeCore core2)

%%[(8 codegen) export(cpTransformCore)
cpTransformCore :: HsName -> [String] -> EHCompilePhase ()
cpTransformCore modNm trfNmL
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
                 trf n t= [cpCore1Trf modNm t,dmp n t]
                 dmp n t= when (ehcOptDumpCoreStages opts) (cpOutputCore ("core-" ++ show n ++ "-" ++ t) modNm)
         ;  cpSeq ps
         }
%%]




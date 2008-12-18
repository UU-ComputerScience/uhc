%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Translation to another AST

%%[8 module {%{EH}EHC.CompilePhase.Translations}
%%]

-- general imports
%%[8 import(qualified Data.Map as Map, qualified EH.Util.FastSeq as Seq)
%%]

%%[8 import({%{EH}EHC.Common})
%%]
%%[8 import({%{EH}EHC.CompileUnit})
%%]
%%[8 import({%{EH}EHC.CompileRun})
%%]

-- EH semantics
%%[8 import(qualified {%{EH}EH.MainAG} as EHSem)
%%]
-- HS semantics
%%[8 import(qualified {%{EH}HS.MainAG} as HSSem)
%%]
-- Core semantics
%%[(8 codegen grin) import(qualified {%{EH}Core.ToGrin} as Core2GrSem)
%%]
-- Grin semantics
%%[(8 codegen grin) import({%{EH}GrinCode.ToGrinByteCode}(grinMod2ByteCodeMod))
%%]
-- Bytecode semantics
%%[(8 codegen grin) import({%{EH}GrinByteCode.ToC}(gbmod2C))
%%]
-- Alternative backends
%%[(8 codegen grin) import(qualified {%{EH}EHC.GrinCompilerDriver} as GRINC)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: translations to another AST
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(cpTranslateHs2EH)
cpTranslateHs2EH :: HsName -> EHCompilePhase ()
cpTranslateHs2EH modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbHsSem= ecuMbHSSem ecu
                 hsSem  = panicJust "cpTranslateHs2EH" mbHsSem
                 eh     = HSSem.eh_Syn_AGItf hsSem
                 errs   = Seq.toList $ HSSem.errSq_Syn_AGItf hsSem
         ;  when (isJust mbHsSem)
                 (do { cpUpdCU modNm (ecuStoreEH eh)
                     ; cpSetLimitErrsWhen 5 "Dependency/name analysis" errs
                     ; when (ehcOptEmitHS opts)
                            (lift $ putPPFile (fpathToStr (fpathSetSuff "hs2" fp)) (HSSem.pp_Syn_AGItf hsSem) 1000)
                     ; when (ehcOptShowHS opts)
                            (lift $ putWidthPPLn 120 (HSSem.pp_Syn_AGItf hsSem))
                     })
         }
%%]

%%[8 export(cpTranslateEH2Output)
cpTranslateEH2Output :: HsName -> EHCompilePhase ()
cpTranslateEH2Output modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbEHSem= ecuMbEHSem ecu
                 ehSem  = panicJust "cpTranslateEH2Output" mbEHSem
%%[[(8 hmtyinfer)
                 about  = "EH analyses: Type checking"
%%][8
                 about  = "EH analyses"
%%]]
%%[[8
                 errs   = Seq.toList $ EHSem.allErrSq_Syn_AGItf ehSem
%%][102
                 -- core   = Core.CModule_Mod modNm (Core.CExpr_Int 1) []
                 errs   = []
%%]]
         ;  when (isJust mbEHSem)
                 (do { cpSetLimitErrsWhen 5 about errs
%%[[8
                     ; when (ehcOptEmitEH opts)
                            (lift $ putPPFile (fpathToStr (fpathSetSuff "eh2" fp)) (EHSem.pp_Syn_AGItf ehSem) 1000)
                     ; when (ehcOptShowEH opts)
                            (lift $ putWidthPPLn 120 (EHSem.pp_Syn_AGItf ehSem))
%%][102
%%]]
%%[[8
                     ; when (ehcOptShowAst opts)
                            (lift $ putPPLn (EHSem.ppAST_Syn_AGItf ehSem))
%%][99
                     ; when (ecuIsTopMod ecu && ehcOptShowAst opts)
                            (lift $ putPPLn (EHSem.ppAST_Syn_AGItf ehSem))
%%][100
%%]]
%%[[(99 hmtyinfer)
                     ; when (ecuIsTopMod ecu && ehcOptEmitDerivTree opts /= DerivTreeWay_None)
                            (lift $ putPPFile (fpathToStr (fpathSetSuff "lhs" fp)) (EHSem.dt_Syn_AGItf ehSem) 1000)
%%][100
%%]]
                     }
                 )
         }
%%]

%%[(8 codegen) export(cpTranslateEH2Core)
cpTranslateEH2Core :: HsName -> EHCompilePhase ()
cpTranslateEH2Core modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbEHSem= ecuMbEHSem ecu
                 ehSem  = panicJust "cpTranslateEH2Core" mbEHSem
                 core   = EHSem.cmodule_Syn_AGItf ehSem
%%[[8
%%][102
                 -- core   = Core.CModule_Mod modNm (Core.CExpr_Int 1) []
%%]]
         ;  when (isJust mbEHSem)
                 (do { cpUpdCU modNm (ecuStoreCore core)
                     }
                 )
         }
%%]

%%[(8 codegen grin) export(cpTranslateCore2Grin)
cpTranslateCore2Grin :: HsName -> EHCompilePhase ()
cpTranslateCore2Grin modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbCoreSem = ecuMbCoreSem ecu
                 coreSem   = panicJust "cpTranslateCore2Grin" mbCoreSem
                 grin      = Core2GrSem.grMod_Syn_CodeAGItf coreSem
         ;  when (isJust mbCoreSem && (ehcOptEmitGrin opts || ehcOptEmitBytecode opts || ehcOptFullProgAnalysis opts))
                 (cpUpdCU modNm $! ecuStoreGrin $! grin)
         }
%%]

%%[(8 codegen grin) export(cpTranslateGrin2Bytecode)
cpTranslateGrin2Bytecode :: HsName -> EHCompilePhase ()
cpTranslateGrin2Bytecode modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 modNmLL= crCompileOrder cr
                 mbGrin = ecuMbGrin ecu
                 grin   = panicJust "cpTranslateGrin2Bytecode" mbGrin
%%[[20
                 expNmOffMp
                        = crsiExpNmOffMp modNm crsi
                 optim  = crsiOptim crsi
%%]]
                 (bc,errs)
                        = grinMod2ByteCodeMod opts
%%[[20
                            (if ecuIsTopMod ecu then [ m | (m,_) <- sortOn snd $ Map.toList $ Map.map fst $ crsiModOffMp crsi ] else [])
                            (crsiModOffMp crsi)
                            expNmOffMp
%%]]
                            $ grin
%%[[20
         -- ;  lift $ putStrLn (show (crsiModOffMp crsi))
%%]]

         ;  when (isJust mbGrin)
                 (cpUpdCU modNm $! ecuStoreBytecode bc)
         ;  when (ehcOptErrAboutBytecode opts)
                 (cpSetLimitErrsWhen 5 "Grin to ByteCode" errs)
         }
%%]

%%[(8 codegen grin) export(cpTranslateGrin)
cpTranslateGrin :: HsName -> EHCompilePhase ()
cpTranslateGrin modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbGrin = ecuMbGrin ecu
                 grin   = panicJust "cpTranslateGrin" mbGrin
         ;  when (isJust mbGrin)
                 (lift $ GRINC.doCompileGrin (Right (fp,grin)) opts)
         }
%%]

%%[(8 codegen grin) export(cpTranslateByteCode)
cpTranslateByteCode :: HsName -> EHCompilePhase ()
cpTranslateByteCode modNm
  =  do  {  cr <- get
         ;  let  (ecu,_,opts,_) = crBaseInfo modNm cr
                 mbBytecode = ecuMbBytecode ecu
%%[[8
                 grinbcPP = gbmod2C opts $ panicJust "cpTranslateByteCode" mbBytecode
%%][20
                 grinbcPP = vlist ([ppMod] ++ (if ecuIsTopMod ecu then [ppMain] else []))
                          where (ppMod,ppMain)
                                  = gbmod2C opts $ panicJust "cpTranslateByteCode" mbBytecode
%%]]
         ;  when (ehcOptEmitBytecode opts && isJust mbBytecode)
                 (do { cpUpdCU modNm $! ecuStoreBytecodeSem grinbcPP
                     })
         }
%%]





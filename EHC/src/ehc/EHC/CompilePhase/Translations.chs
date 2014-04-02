%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Translation to another AST

%%[8 module {%{EH}EHC.CompilePhase.Translations}
%%]

-- general imports
%%[8 import(qualified Data.Map as Map, qualified Data.Set as Set, qualified UHC.Util.FastSeq as Seq)
%%]
%%[8 import(Control.Monad.State)
%%]

%%[(50 codegen) import({%{EH}Base.Optimize})
%%]

%%[8 import({%{EH}EHC.Common})
%%]
%%[8 import({%{EH}EHC.CompileUnit})
%%]
%%[8 import({%{EH}EHC.CompileRun})
%%]
%%[(8 codegen) import({%{EH}Base.Target})
%%]
%%[(50 codegen) hs import({%{EH}CodeGen.ValAccess} as VA, {%{EH}CodeGen.RefGenerator})
%%]
%%[(8 codegen cmm) hs import({%{EH}CodeGen.Const} as Const (emptyConstSt))
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

-- TyCore semantics
%%[(8 codegen tycore grin) import(qualified {%{EH}TyCore.ToCore} as TyCore2Core)
%%]
%%[(8 codegen tycore grin) import(qualified {%{EH}TyCore.PrettyAST} as TyCoreSem)
%%]

-- Grin semantics
%%[(8 codegen grin) import({%{EH}GrinCode.ToGrinByteCode}(grinMod2ByteCodeMod))
%%]
%%[(8 codegen grin cmm) import({%{EH}GrinCode.ToCmm}(grinMod2CmmMod))
%%]

-- Cmm semantics
%%[(8 codegen cmm) import(qualified {%{EH}Cmm} as Cmm)
%%]
%%[(8 codegen cmm javascript) import({%{EH}Cmm.ToJavaScript})
%%]

-- Bytecode semantics
%%[(8 codegen grin) import({%{EH}GrinByteCode.ToC}(gbmod2C))
%%]

-- Jazy/JVM/JavaScript semantics
%%[(8 codegen jazy) import({%{EH}Core.ToJazy})
%%]
%%[(8 codegen javascript) import({%{EH}Core.ToJavaScript})
%%]
%%[(8 codegen java) import({%{EH}Base.Bits},{%{EH}JVMClass.ToBinary})
%%]

-- Alternative backends
%%[(8 codegen grin wholeprogAnal) import(qualified {%{EH}EHC.GrinCompilerDriver} as GRINC)
%%]

-- HI AST
%%[(50 codegen grin) import(qualified {%{EH}HI} as HI)
%%]
-- LamInfo
%%[(50 codegen grin) import({%{EH}LamInfo})
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
                            (lift $ putPPFPath (mkOutputFPath opts modNm fp "hs2") (HSSem.pp_Syn_AGItf hsSem) 1000)
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
                            (lift $ putPPFPath (mkOutputFPath opts modNm fp "eh2") (EHSem.pp_Syn_AGItf ehSem) 1000)
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
%%[[(99 hmtyinfer tyderivtree)
                     ; when (ecuIsTopMod ecu && ehcOptEmitDerivTree opts /= DerivTreeWay_None)
                            (lift $ putPPFPath (mkOutputFPath opts modNm fp "lhs") (EHSem.dt_Syn_AGItf ehSem) 1000)
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
                 core   = EHSem.cmodule_Syn_AGItf  ehSem
         ;  when (isJust mbEHSem)
                 (cpUpdCU modNm ( ecuStoreCore core
                                ))
         }
%%]

%%[(8 codegen tycore) export(cpTranslateEH2TyCore)
cpTranslateEH2TyCore :: HsName -> EHCompilePhase ()
cpTranslateEH2TyCore modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbEHSem= ecuMbEHSem ecu
                 ehSem  = panicJust "cpTranslateEH2Core" mbEHSem
                 tycore = EHSem.tcmodule_Syn_AGItf ehSem
         ;  when (isJust mbEHSem)
                 (do { cpUpdCU modNm (ecuStoreTyCore tycore)
                     ; when (ehcOptShowTyCore opts)
                            (lift $ putPPLn (TyCoreSem.ppAST opts tycore))
                     })
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
         ;  when (isJust mbCoreSem && ehcOptIsViaGrin opts)
                 (cpUpdCU modNm $! ecuStoreGrin $! grin)
         }
%%]

%%[(8 codegen tycore grin) export(cpTranslateTyCore2Core)
cpTranslateTyCore2Core :: HsName -> EHCompilePhase ()
cpTranslateTyCore2Core modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbTyCore  = ecuMbTyCore ecu
                 tycore    = panicJust "cpTranslateTyCore2Core" mbTyCore
                 core      = TyCore2Core.tycore2core opts tycore
         ;  when (isJust mbTyCore)
                 (cpUpdCU modNm $! ecuStoreCore $! core)
         }
%%]

%%[(8 codegen jazy) export(cpTranslateCore2Jazy)
cpTranslateCore2Jazy :: HsName -> EHCompilePhase ()
cpTranslateCore2Jazy modNm
  = do { cr <- get
       ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
              mbCore    = ecuMbCore ecu
       ; when (isJust mbCore && targetIsJVM (ehcOptTarget opts))
              (cpUpdCU modNm $ ecuStoreJVMClassL $ cmod2JazyJVMModule opts $ fromJust mbCore)
       }
%%]

%%[(8 codegen javascript) export(cpTranslateCore2JavaScript)
-- | Translate Core to JavaScript
cpTranslateCore2JavaScript :: HsName -> EHCompilePhase ()
cpTranslateCore2JavaScript modNm
  = do { cr <- get
       ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
              mbCore    = ecuMbCore ecu
              coreInh  = crsiCoreInh crsi
       ; when (isJust mbCore)
              (cpUpdCU modNm $ ecuStoreJavaScript $ cmod2JavaScriptModule opts (Core2GrSem.dataGam_Inh_CodeAGItf coreInh) $ fromJust mbCore)
       }
%%]

%%[(50 codegen grin)
-- | Compute info for Grin based codegen
cpGenGrinGenInfo
  :: HsName
  -> EHCompilePhase
       ( LamMp
       , [HsName]
       , HsName2FldMpMp
       , HsName2FldMp
       )
cpGenGrinGenInfo modNm
  = do { cr <- get
       ; let (ecu,crsi,opts,fp) = crBaseInfo modNm cr
             isWholeProg = ehcOptOptimizationScope opts >= OptimizationScope_WholeGrin
             impNmL     | isWholeProg = []
                        | otherwise   = ecuImpNmL ecu
             expNmFldMp | ecuIsMainMod ecu = Map.empty
                        | otherwise        = crsiExpNmOffMp modNm crsi
             modOffMp   | isWholeProg = Map.filterWithKey (\n _ -> n == modNm) $ crsiModOffMp crsi
                        | otherwise   = crsiModOffMp crsi
       ; return
           ( Core2GrSem.lamMp_Inh_CodeAGItf $ crsiCoreInh crsi
           , if ecuIsMainMod ecu then [ m | (m,_) <- sortOn snd $ Map.toList $ Map.map fst modOffMp ] else []
           , Map.fromList [ (n,(o,mp))
                          | (n,o) <- refGen 0 1 impNmL
                          , let (_,mp) = panicJust ("cpGenGrinGenInfo: " ++ show n) (Map.lookup n (crsiModOffMp crsi))
                          ]
           , expNmFldMp
           )
       }
%%]

%%[(8 codegen grin cmm) export(cpTranslateGrin2Cmm)
-- | Translate Grin to Cmm
cpTranslateGrin2Cmm :: HsName -> EHCompilePhase ()
cpTranslateGrin2Cmm modNm
  = do { cr <- get
       ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
              mbGrin    = ecuMbGrin ecu
              coreInh   = crsiCoreInh crsi
%%[[50
       ; (lamMp, allImpNmL, impNmFldMpMp, expNmFldMp) <- cpGenGrinGenInfo modNm
%%]]
       ; when (isJust mbGrin) $ do
           let (cmm,errs)
                 = grinMod2CmmMod
                     opts
                     (Core2GrSem.dataGam_Inh_CodeAGItf coreInh)
%%[[50
                     lamMp allImpNmL impNmFldMpMp expNmFldMp
%%]]
                   $ fromJust mbGrin
           cpUpdCU modNm $ ecuStoreCmm cmm
       }
%%]

%%[(8 codegen javascript cmm) export(cpTranslateCmm2JavaScript)
-- | Translate Cmm to JavaScript
cpTranslateCmm2JavaScript :: HsName -> EHCompilePhase ()
cpTranslateCmm2JavaScript modNm
  = do { cr <- get
       ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
              mbCmm    = ecuMbCmm ecu
              coreInh   = crsiCoreInh crsi
       ; when (isJust mbCmm) $ do
           let (jsmod,errs) = cmmMod2JavaScript opts (Core2GrSem.dataGam_Inh_CodeAGItf coreInh) $ fromJust mbCmm
           cpUpdCU modNm $ ecuStoreJavaScript jsmod
       }
%%]

%%[(8 codegen grin) export(cpTranslateGrin2Bytecode)
cpTranslateGrin2Bytecode :: HsName -> EHCompilePhase ()
cpTranslateGrin2Bytecode modNm
  =  do { cr <- get
        ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
%%[[50
        ; when (ehcOptVerbosity opts >= VerboseDebug)
               (lift $ putStrLn ("crsiModOffMp: " ++ show (crsiModOffMp crsi)))
        ; (lamMp, allImpNmL, impNmFldMpMp, expNmFldMp) <- cpGenGrinGenInfo modNm
%%]]
        ; let  mbGrin = ecuMbGrin ecu
               grin   = panicJust "cpTranslateGrin2Bytecode1" mbGrin
               (bc,errs)
                      = grinMod2ByteCodeMod
                            opts
%%[[50
                            lamMp allImpNmL impNmFldMpMp expNmFldMp
%%]]
                          $ grin
%%[[50
        ; when (ehcOptVerbosity opts >= VerboseDebug)
               (lift $ putStrLn ("expNmFldMp: " ++ show expNmFldMp))
%%]]

        ; cpMsg modNm VerboseDebug ("cpTranslateGrin2Bytecode: store bytecode")
        ; when (isJust mbGrin)
               (cpUpdCU modNm $! ecuStoreBytecode bc)
        ; cpMsg modNm VerboseDebug ("cpTranslateGrin2Bytecode: stored bytecode")
        ; when (ehcOptErrAboutBytecode opts)
               (cpSetLimitErrsWhen 5 "Grin to ByteCode" errs)
        }
%%]

%%[(8 codegen grin wholeprogAnal) export(cpTransformGrinHPTWholeProg)
-- This should be in Transformations
-- | Transform Grin using HPT and its results
cpTransformGrinHPTWholeProg :: HsName -> EHCompilePhase ()
cpTransformGrinHPTWholeProg modNm
  =  do { cr <- get
        ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
               mbGrin = ecuMbGrin ecu
               grin   = panicJust "cpTransformGrinHPTWholeProg" mbGrin
        ; when (isJust mbGrin)
               (lift $ GRINC.doCompileGrin (Right (fp,grin)) opts)
        }
%%]

%%[(8 codegen grin) export(cpTranslateByteCode)
cpTranslateByteCode :: HsName -> EHCompilePhase ()
cpTranslateByteCode modNm
  =  do { cr <- get
        ; let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
               mbBytecode = ecuMbBytecode ecu
%%[[8
               ( grinbcPP
%%[[(8 cmm)
                ,cmmMod
%%]]
                ) = gbmod2C opts $ panicJust "cpTranslateByteCode1" mbBytecode
%%[[(8 cmm)
               grinbcCmm = Cmm.Module_Mod modNm cmmMod Const.emptyConstSt
%%]]
%%][50
               coreInh  = crsiCoreInh crsi
               ( grinbcPP
%%[[(50 cmm)
                 ,grinbcCmm
%%]]
                 ,functionInfoExportMp)
                        = ( vlist ([ppMod] ++ (if ecuIsMainMod ecu then [ppMain] else []))
%%[[(50 cmm)
                          , Cmm.Module_Mod modNm (cmmMod  ++ (if ecuIsMainMod ecu then cmmMain else [])) emptyConstSt
%%]]
                          , functionInfoExportMp
                          )
                        where ( ppMod,ppMain
%%[[(50 cmm)
                               ,cmmMod,cmmMain
%%]]
                               ,functionInfoExportMp)
                                = gbmod2C opts lkup $ panicJust "cpTranslateByteCode2" mbBytecode
                                where lkup n = do { li <- Map.lookup n (Core2GrSem.lamMp_Inh_CodeAGItf coreInh)
                                                  ; ex <- laminfoGrinByteCode li
                                                  ; return ex
                                                  }
%%]]
          -- put back results: generated bytecode, new info about lambda's
        ; when (ehcOptEmitBytecode opts && isJust mbBytecode)
               (do { cpUpdCU modNm
                      ( ecuStoreBytecodeSem grinbcPP
%%[[(8 cmm)
                      . ecuStoreCmm grinbcCmm
%%]]
%%[[50
                      . ( let hii = ecuHIInfo ecu
                          in  ecuStoreHIInfo
                                (hii { HI.hiiLamMp = lamMpMergeFrom laminfoGrinByteCode (\gbi i -> i {laminfoGrinByteCode=gbi}) const emptyLamInfo' functionInfoExportMp $ HI.hiiLamMp hii
                                     })
                        )
%%]]
                      )
{-
                   ; when (ehcOptVerbosity opts >= VerboseDebug)
                          (lift $ do { putStrLn ("cpTranslateByteCode.lamMp: " ++ show (HI.hiiLamMp hii))
                                     ; putStrLn ("cpTranslateByteCode.functionInfoExportMp: " ++ show functionInfoExportMp)
                                     })
-}
                   })
        }
%%]


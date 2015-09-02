%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Translation to another AST

%%[8 module {%{EH}EHC.CompilePhase.Translations}
%%]

-- general imports
%%[8 import(qualified Data.Map as Map, qualified Data.Set as Set, qualified UHC.Util.FastSeq as Seq)
%%]
%%[8 import (UHC.Util.Lens)
%%]
%%[8 import(Control.Monad.State)
%%]

%%[(50 codegen) import({%{EH}Base.Optimize})
%%]
%%[(50 codegen) import({%{EH}EHC.CompilePhase.Module})
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

-- build call
%%[8 import({%{EH}EHC.BuildFunction.Run})
%%]

-- EH semantics
%%[8 import(qualified {%{EH}EH.Main} as EHSem)
%%]

-- HS semantics
%%[8 import(qualified {%{EH}HS.MainAG} as HSSem)
%%]

-- Core semantics
%%[(8 core) import(qualified {%{EH}Core.ToGrin} as Core2GrSem)
%%]
%%[(8 codegen) import({%{EH}Core.Trf.ElimNonCodegenConstructs})
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
%%[(8 codegen java) import({%{EH}CodeGen.Bits},{%{EH}JVMClass.ToBinary})
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
-- ModuleImportExportImpl
%%[(50 codegen) import({%{EH}CodeGen.ModuleImportExportImpl})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: translations to another AST
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(cpTranslateHs2EH)
cpTranslateHs2EH :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpTranslateHs2EH modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbHsSem= _ecuMbHSSem ecu
                 hsSem  = panicJust "cpTranslateHs2EH" mbHsSem
                 eh     = HSSem.eh_Syn_AGItf hsSem
                 errs   = Seq.toList $ HSSem.errSq_Syn_AGItf hsSem
         ;  when (isJust mbHsSem)
                 (do { cpUpdCU modNm (ecuStoreEH eh)
                     ; cpSetLimitErrsWhen 5 "Dependency/name analysis" errs
                     ; when (ehcOptEmitHS opts)
                            (liftIO $ putPPFPath (mkOutputFPath opts modNm fp "hs2") (HSSem.pp_Syn_AGItf hsSem) 1000)
                     ; when (ehcOptShowHS opts)
                            (liftIO $ putWidthPPLn 120 (HSSem.pp_Syn_AGItf hsSem))
                     })
         }
%%]

%%[8888 export(cpTranslateEH2Output)
cpTranslateEH2Output :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpTranslateEH2Output modNm
  =  do  {  cr <- get
%%[[50
         ;  isTopMod <- bcall $ IsTopMod $ mkPrevFileSearchKeyWithName modNm
%%]]
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbEHSem= _ecuMbEHSem ecu
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
                            (liftIO $ putPPFPath (mkOutputFPath opts modNm fp "eh2") (EHSem.pp_Syn_AGItf ehSem) 1000)
                     ; when (ehcOptShowEH opts)
                            (liftIO $ putWidthPPLn 120 (EHSem.pp_Syn_AGItf ehSem))
%%][102
%%]]
%%[[8
                     ; when (ehcOptShowAst opts)
                            (liftIO $ putPPLn (EHSem.ppAST_Syn_AGItf ehSem))
%%][99
                     ; when (isTopModu && ehcOptShowAst opts)
                            (liftIO $ putPPLn (EHSem.ppAST_Syn_AGItf ehSem))
%%][100
%%]]
%%[[(99 hmtyinfer tyderivtree)
                     ; when (isTopMod && ehcOptEmitDerivTree opts /= DerivTreeWay_None)
                            (liftIO $ putPPFPath (mkOutputFPath opts modNm fp "lhs") (EHSem.dt_Syn_AGItf ehSem) 1000)
%%][100
%%]]
                     }
                 )
         }
%%]

%%[(8 codegen) export(cpTranslateEH2Core)
cpTranslateEH2Core :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpTranslateEH2Core modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbEHSem= _ecuMbEHSem ecu
                 ehSem  = panicJust "cpTranslateEH2Core" mbEHSem
                 core   = cmodTrfElimNonCodegenConstructs opts $ EHSem.cmodule_Syn_AGItf  ehSem
         ;  when (isJust mbEHSem)
                 (cpUpdCU modNm ( ecuStoreCore core
                                ))
         }
%%]

%%[(8 codegen tycore) export(cpTranslateEH2TyCore)
cpTranslateEH2TyCore :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpTranslateEH2TyCore modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbEHSem= _ecuMbEHSem ecu
                 ehSem  = panicJust "cpTranslateEH2Core" mbEHSem
                 tycore = EHSem.tcmodule_Syn_AGItf ehSem
         ;  when (isJust mbEHSem)
                 (do { cpUpdCU modNm (ecuStoreTyCore tycore)
                     ; when (ehcOptShowTyCore opts)
                            (liftIO $ putPPLn (TyCoreSem.ppAST opts tycore))
                     })
         }
%%]

%%[(8 codegen grin) export(cpTranslateCore2Grin)
cpTranslateCore2Grin :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpTranslateCore2Grin modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbCoreSem = _ecuMbCoreSem ecu
                 coreSem   = panicJust "cpTranslateCore2Grin" mbCoreSem
                 grin      = Core2GrSem.grMod_Syn_CodeAGItf coreSem
         ;  when (isJust mbCoreSem && ehcOptIsViaGrin opts)
                 (cpUpdCU modNm $! ecuStoreGrin $! grin)
         }
%%]

%%[(8 codegen tycore grin) export(cpTranslateTyCore2Core)
cpTranslateTyCore2Core :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
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
cpTranslateCore2Jazy :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpTranslateCore2Jazy modNm
  = do { cr <- get
       ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
              mbCore    = _ecuMbCore ecu
       ; when (isJust mbCore && targetIsJVM (ehcOptTarget opts))
              (cpUpdCU modNm $ ecuStoreJVMClassL $ cmod2JazyJVMModule opts $ fromJust mbCore)
       }
%%]

%%[(8 codegen javascript) export(cpTranslateCore2JavaScript)
-- | Translate Core to JavaScript
cpTranslateCore2JavaScript :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpTranslateCore2JavaScript modNm
  = do { cr <- get
       ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
              mbCore    = _ecuMbCore ecu
              coreInh  = crsi ^. crsiCoreInh
       ; when (isJust mbCore)
              (cpUpdCU modNm $ ecuStoreJavaScript $ cmod2JavaScriptModule opts (Core2GrSem.dataGam_Inh_CodeAGItf coreInh) $ fromJust mbCore)
       }
%%]

%%[(50 codegen grin)
-- | Compute info for Grin based codegen.
-- TBD: can become obsolete
cpGenGrinGenInfo
  :: EHCCompileRunner m => 
     HsName
  -> EHCompilePhaseT m
       ( LamMp
       , [HsName]
       , HsName2FldMpMp
       , HsName2FldMp
       )
cpGenGrinGenInfo modNm
  = do let modSearchKey = mkPrevFileSearchKeyWithName modNm
       opts <- bcall $ EHCOptsOf modSearchKey
       -- mieimpl <- cpGenModuleImportExportImpl modNm
       mieimpl <- bcall $ ImportExportImpl modSearchKey (ehcOptOptimizationScope opts)
       return (mieimplLamMp mieimpl, mieimplUsedModNmL mieimpl, mieimplHsName2FldMpMp mieimpl, mieimplHsName2FldMp mieimpl)
%%]

%%[(8 codegen grin cmm) export(cpTranslateGrin2Cmm)
-- | Translate Grin to Cmm
cpTranslateGrin2Cmm :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpTranslateGrin2Cmm modNm
  = do { cr <- get
       ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
              mbGrin    = _ecuMbGrin ecu
              coreInh   = crsi ^. crsiCoreInh
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
cpTranslateCmm2JavaScript :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpTranslateCmm2JavaScript modNm
  = do { cr <- get
       ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
              mbCmm    = _ecuMbCmm ecu
              coreInh   = crsi ^. crsiCoreInh
       ; when (isJust mbCmm) $ do
           let (jsmod,errs) =
                 cmmMod2JavaScript opts
%%[[50
                   (ecuImportUsedModules ecu)
%%]]
                   (Core2GrSem.dataGam_Inh_CodeAGItf coreInh)
                   (fromJust mbCmm)
           cpUpdCU modNm $ ecuStoreJavaScript jsmod
       }
%%]

%%[(8 codegen grin) export(cpTranslateGrin2Bytecode)
cpTranslateGrin2Bytecode :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpTranslateGrin2Bytecode modNm
  =  do { cr <- get
        ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
%%[[50
        ; when (ehcOptVerbosity opts >= VerboseDebug)
               (liftIO $ putStrLn ("crsiModOffMp: " ++ show (crsiModOffMp crsi)))
        ; (lamMp, allImpNmL, impNmFldMpMp, expNmFldMp) <- cpGenGrinGenInfo modNm
%%]]
        ; let  mbGrin = _ecuMbGrin ecu
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
               (liftIO $ putStrLn ("expNmFldMp: " ++ show expNmFldMp))
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
cpTransformGrinHPTWholeProg :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpTransformGrinHPTWholeProg modNm
  =  do { cr <- get
        ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
               mbGrin = _ecuMbGrin ecu
               grin   = panicJust "cpTransformGrinHPTWholeProg" mbGrin
        ; when (isJust mbGrin)
               (liftIO $ GRINC.doCompileGrin (Right (fp,grin)) opts)
        }
%%]

%%[(8 codegen grin) export(cpTranslateByteCode)
cpTranslateByteCode :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpTranslateByteCode modNm
  =  do { cr <- get
        ; let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
               mbBytecode = ecuMbBytecode ecu
%%[[8
               ( grinbcPP
%%[[(8 cmm cmmbc)
                ,cmmMod
%%]]
                ) = gbmod2C opts $ panicJust "cpTranslateByteCode1" mbBytecode
%%[[(8 cmm cmmbc)
               grinbcCmm = Cmm.Module_Mod modNm cmmMod Nothing Const.emptyConstSt
%%]]
%%][50
               coreInh  = crsi ^. crsiCoreInh
               ( grinbcPP
%%[[(50 cmm cmmbc)
                 ,grinbcCmm
%%]]
                 ,functionInfoExportMp)
                        = ( vlist ([ppMod] ++ (if ecuIsMainMod ecu then [ppMain] else []))
%%[[(50 cmm cmmbc)
                          , Cmm.Module_Mod modNm (cmmMod  ++ (if ecuIsMainMod ecu then cmmMain else [])) Nothing emptyConstSt
%%]]
                          , functionInfoExportMp
                          )
                        where ( ppMod,ppMain
%%[[(50 cmm cmmbc)
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
%%[[(8 cmm cmmbc)
                      . ecuStoreCmm grinbcCmm
%%]]
%%[[50
                      . ( let hii = ecu ^. ecuHIInfo
                          in  ecuStoreHIInfo
                                (hii { HI.hiiLamMp = lamMpMergeFrom laminfoGrinByteCode (\gbi i -> i {laminfoGrinByteCode=gbi}) const emptyLamInfo' functionInfoExportMp $ HI.hiiLamMp hii
                                     })
                        )
%%]]
                      )
{-
                   ; when (ehcOptVerbosity opts >= VerboseDebug)
                          (liftIO $ do { putStrLn ("cpTranslateByteCode.lamMp: " ++ show (HI.hiiLamMp hii))
                                       ; putStrLn ("cpTranslateByteCode.functionInfoExportMp: " ++ show functionInfoExportMp)
                                       })
-}
                   })
        }
%%]


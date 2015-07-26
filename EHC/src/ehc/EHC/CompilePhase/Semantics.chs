%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Folding over AST to compute semantics

%%[8 module {%{EH}EHC.CompilePhase.Semantics}
%%]

-- general imports
%%[8 import(UHC.Util.Lens)
%%]
%%[8 import(Control.Monad.State)
%%]
%%[8 import(qualified Data.Map as Map)
%%]
%%[50 import(qualified Data.Set as Set)
%%]

%%[8 import({%{EH}EHC.Common})
%%]
%%[8 import({%{EH}EHC.CompileUnit})
%%]
%%[8 import({%{EH}EHC.CompileRun})
%%]
%%[(50 codegen) import({%{EH}EHC.CompilePhase.Common})
%%]

-- build call
%%[8 import({%{EH}EHC.BuildFunction.Run})
%%]

-- EH semantics
%%[8 import(qualified {%{EH}EH.MainAG} as EHSem)
%%]
-- HS semantics
%%[8 import(qualified {%{EH}HS.MainAG} as HSSem)
%%]
-- Core syntax and semantics
%%[(8 core) import(qualified {%{EH}Core} as Core, qualified {%{EH}Core.ToGrin} as Core2GrSem)
%%]
%%[(8 core corerun) import(qualified {%{EH}Core.ToCoreRun} as Core2CoreRunSem)
%%]
%%[(50 codegen corein) import(qualified {%{EH}Core.Check} as Core2ChkSem)
%%]
-- CoreRun syntax and semantics
%%[(8 corerun) import(qualified {%{EH}CoreRun} as CoreRun)
%%]
%%[(50 codegen corerunin) import(qualified {%{EH}CoreRun.Check} as CoreRun2ChkSem)
%%]
-- TyCore syntax and semantics
%%[(8 codegen tycore) import(qualified {%{EH}TyCore} as C)
%%]

-- HI syntax and semantics
%%[50 import(qualified {%{EH}HI} as HI)
%%]

-- Module
%%[50 import(qualified UHC.Util.Rel as Rel)
%%]
%%[50 import({%{EH}Module.ImportExport})
%%]
%%[50 import(qualified {%{EH}HS.ModImpExp} as HSSemMod)
%%]

-- for debug
%%[50 hs import({%{EH}Base.Debug},UHC.Util.Pretty)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: computing semantics
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(cpFoldCore2Grin)
cpFoldCore2Grin :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpFoldCore2Grin modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 mbCore   = _ecuMbCore ecu
                 core     = panicJust "cpFoldCore2Grin" mbCore
                 coreInh  = crsiCoreInh crsi
                 coreSem  = Core2GrSem.wrap_CodeAGItf
                              (Core2GrSem.sem_CodeAGItf (Core.CodeAGItf_AGItf core))
                              (coreInh { Core2GrSem.gUniq_Inh_CodeAGItf                         = crsi ^. crsiHereUID
                                       , Core2GrSem.opts_Inh_CodeAGItf                          = opts
%%[[50
                                       , Core2GrSem.importUsedModules_Inh_CodeAGItf             = ecuImportUsedModules ecu
%%]]
                                       })
         ;  when (isJust mbCore)
                 (cpUpdCU modNm ( ecuStoreCoreSem coreSem
                                ))
         }
%%]

%%[(8 core corerun) export(cpFoldCore2CoreRun)
cpFoldCore2CoreRun :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpFoldCore2CoreRun modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) 		= crBaseInfo modNm cr
                 mbCore   				= _ecuMbCore ecu
%%[[8
                 hasMain  				= True
%%][50
                 hasMain  				= ecuHasMain ecu
%%]]
                 core     				= panicJust "cpFoldCore2CoreRun" mbCore
                 core2RunInh			= crsiCore2RunInh crsi
                 (corerun,nm2ref,sem)	= Core2CoreRunSem.cmod2CoreRun' opts hasMain 0 core2RunInh core
                 core2RunInh'			= nm2ref `CoreRun.nm2refUnion` core2RunInh
         ;  when (isJust mbCore) $ do
                 -- between module flow part
                 cpUpdSI (\crsi -> crsi {crsiCore2RunInh = core2RunInh'})
                 -- per module part
                 cpUpdCU modNm ( ecuStoreCoreRun corerun
                               . ecuStoreCore2CoreRunSem sem
                               )
         }
%%]

%%[(50 codegen corerunin) export(cpFoldCoreRunMod)
cpFoldCoreRunMod :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpFoldCoreRunMod modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 mbCoreRun= _ecuMbCoreRun ecu
                 core     = panicJust "cpFoldCoreRunMod" mbCoreRun
                 inh      = CoreRun2ChkSem.Inh_AGItf
                                { CoreRun2ChkSem.opts_Inh_AGItf = opts
                                , CoreRun2ChkSem.moduleNm_Inh_AGItf = modNm
                                -- , CoreRun2ChkSem.dataGam_Inh_AGItf = EHSem.dataGam_Inh_AGItf $ _crsiEHInh crsi
                                }
                 crrSem   = CoreRun2ChkSem.crmodCheck' inh core
                 hasMain  = CoreRun2ChkSem.hasMain_Syn_AGItf crrSem
                 mod      = CoreRun2ChkSem.mod_Syn_AGItf crrSem
         -- ;  liftIO $ putStrLn $ "cpFoldCoreRunMod " ++ show hasMain
         ;  when (isJust mbCoreRun)
                 (cpUpdCU modNm ( ecuStoreCoreRunSemMod crrSem
                                . ecuSetHasMain hasMain
                                . ecuStoreMod mod
                                ))
         }
%%]

%%[(50 codegen corein) export(cpFoldCoreMod)
cpFoldCoreMod :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpFoldCoreMod modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 mbCore   = _ecuMbCore ecu
                 core     = panicJust "cpFoldCoreMod" mbCore
                 inh      = Core2ChkSem.Inh_CodeAGItf
                                { Core2ChkSem.opts_Inh_CodeAGItf = opts
                                , Core2ChkSem.moduleNm_Inh_CodeAGItf = modNm
                                , Core2ChkSem.dataGam_Inh_CodeAGItf = EHSem.dataGam_Inh_AGItf $ crsi ^. crsiEHInh
                                }
                 coreSem  = Core2ChkSem.cmodCheck' inh core
                 hasMain  = Core2ChkSem.hasMain_Syn_CodeAGItf coreSem
                 mod      = Core2ChkSem.mod_Syn_CodeAGItf coreSem
         -- ;  liftIO $ putStrLn $ "cpFoldCoreMod " ++ show hasMain
         ;  when (isJust mbCore)
                 (cpUpdCU modNm ( ecuStoreCoreSemMod coreSem
                                . ecuSetHasMain hasMain
                                . ecuStoreMod mod
                                ))
         }
%%]

%%[8 export(cpFoldEH)
cpFoldEH :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpFoldEH modNm
  =  do  {  cr <- get
%%[[(50 codegen)
         ;  mieimpl <- cpGenModuleImportExportImpl modNm
%%]]
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 mbEH   = _ecuMbEH ecu
                 ehSem  = EHSem.wrap_AGItf (EHSem.sem_AGItf $ panicJust "cpFoldEH" mbEH)
                                           ((crsi ^. crsiEHInh)
                                                  { EHSem.moduleNm_Inh_AGItf         		= ecuModNm ecu
                                                  , EHSem.gUniq_Inh_AGItf            		= crsi ^. crsiHereUID
                                                  , EHSem.opts_Inh_AGItf             		= opts
%%[[(50 codegen)
                                                  , EHSem.importUsedModules_Inh_AGItf		= ecuImportUsedModules ecu
                                                  , EHSem.moduleImportExportImpl_Inh_AGItf	= mieimpl
%%]]
%%[[50
                                                  , EHSem.isMainMod_Inh_AGItf        		= ecuIsMainMod ecu
%%]]
                                                  })
         ;  when (isJust mbEH)
                 (cpUpdCU modNm $! ecuStoreEHSem $! ehSem)
         }
%%]

%%[8 export(cpFoldHs)
cpFoldHs :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpFoldHs modNm
  =  do  {  cr <- get
%%[[50
         ;  isTopMod <- bcall $ IsTopMod modNm
%%]]
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 mbHS   = _ecuMbHS ecu
                 inh    = crsi ^. crsiHSInh
                 hsSem  = HSSem.wrap_AGItf (HSSem.sem_AGItf $ panicJust "cpFoldHs" mbHS)
                                           (inh { HSSem.opts_Inh_AGItf             = opts
                                                , HSSem.gUniq_Inh_AGItf            = crsi ^. crsiHereUID
%%[[50
                                                , HSSem.moduleNm_Inh_AGItf         = modNm
                                                , HSSem.isTopMod_Inh_AGItf         = isTopMod -- ecuIsTopMod ecu
                                                , HSSem.modInScope_Inh_AGItf       = inscps
                                                , HSSem.modEntToOrig_Inh_AGItf     = exps
                                                , HSSem.topInstanceNmL_Inh_AGItf   = modInstNmL (ecuMod ecu)
%%]]
                                                })
%%[[50
                        where mmi    = panicJust "cpFoldHs.crsiModMp" $ Map.lookup modNm $ crsiModMp crsi
                              inscps = Rel.toDomMap --- $ (\v -> tr "XX mmiInscps mmi" (pp v ) v)
                                                    $ mmiInscps 
                                                    --- $ (\v -> tr "XX mmi" (pp v ) v)
                                                    $ mmi
                              exps   = Rel.toRngMap $ Rel.restrictRng (\o -> let mq = hsnQualifier (ioccNm o) in isJust mq && fromJust mq /= modNm)
                                                    $ Rel.mapRng mentIdOcc $ mmiExps mmi
%%]]
%%[[50
                 hasMain= HSSem.mainValExists_Syn_AGItf hsSem
%%]]
         ;  when (isJust mbHS)
                 (do { cpUpdCU modNm ( ecuStoreHSSem hsSem
%%[[50
                                     . ecuStoreHIDeclImpS ( -- (\v -> tr "YY" (pp $ Set.toList v) v) $
                                                           ecuHSDeclImpNmS ecu)
                                     -- . ecuSetHasMain hasMain
%%]]
                                     )
%%[[50
                     ; when (ehcOptVerbosity opts >= VerboseDebug)
                            (liftIO $ putStrLn (show modNm ++ " hasMain=" ++ show hasMain))
                     -- ; when hasMain (crSetAndCheckMain modNm)
%%]]
                     })
         }
%%]

%%[50 export(cpFoldHsMod)
cpFoldHsMod :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpFoldHsMod modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 mbHS       = _ecuMbHS ecu
                 inh        = crsiHSModInh crsi
                 hsSemMod   = HSSemMod.wrap_AGItf (HSSemMod.sem_AGItf $ panicJust "cpFoldHsMod" mbHS)
                                                  (inh { HSSemMod.gUniq_Inh_AGItf        = crsi ^. crsiHereUID
                                                       , HSSemMod.moduleNm_Inh_AGItf     = modNm
                                                       })
                 hasMain= HSSemMod.mainValExists_Syn_AGItf hsSemMod
%%[[99
                 pragmas = HSSemMod.fileHeaderPragmas_Syn_AGItf hsSemMod
                 (ecuOpts,modifiedOpts)
                         = ehcOptUpdateWithPragmas pragmas opts
%%]]
         ;  when (isJust mbHS)
                 (cpUpdCU modNm ( ecuStoreHSSemMod hsSemMod
                                . ecuSetHasMain hasMain
%%[[99
                                . ecuStorePragmas pragmas
                                . (if modifiedOpts then ecuStoreOpts ecuOpts else id)
%%]]
                 )              )
         }
%%]

%%[50 export(cpFoldHIInfo)
cpFoldHIInfo :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpFoldHIInfo modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 mbHIInfo   = _ecuMbPrevHIInfo ecu
                 hiInfo     = panicJust "cpFoldHIInfo" mbHIInfo
                 hasMain    = HI.hiiHasMain hiInfo
         ;  when (isJust mbHIInfo && HI.hiiValidity hiInfo == HI.HIValidity_Ok)
                 (do { let mm     = crsiModMp crsi
                           mmi    = Map.findWithDefault emptyModMpInfo modNm mm
                           mmi'   = mkModMpInfo modNm
                                                (mmiInscps mmi)
                                                ( -- (\v -> tr "cpFoldHIInfo.hiiExps" (pp v) v) $
                                                 HI.hiiExps hiInfo)
                                                (HI.hiiHiddenExps hiInfo)
                     -- ; when hasMain (crSetAndCheckMain modNm)
                     ; cpUpdSI (\crsi -> crsi {crsiModMp = Map.insert modNm mmi' mm})
                     ; cpUpdCU modNm ( ecuStorePrevHIInfo hiInfo
                                     . ecuStoreHIDeclImpS (HI.hiiHIDeclImpModS hiInfo)
                                     . ecuStoreHIUsedImpS (HI.hiiHIUsedImpModS hiInfo)
                                     . ecuSetHasMain hasMain
                                     )
                     ; when (ehcOptVerbosity opts >= VerboseDebug)
                            (liftIO $ putStrLn
                               (show modNm
                                ++ ": hi imps, decl=" ++ show (HI.hiiHIDeclImpModS hiInfo)
                                ++ ", used=" ++ show (HI.hiiHIUsedImpModS hiInfo)
                            )  )
                     })
         }
%%]





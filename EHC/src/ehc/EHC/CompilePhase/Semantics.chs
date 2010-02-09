%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Folding over AST to compute semantics

%%[8 module {%{EH}EHC.CompilePhase.Semantics}
%%]

-- general imports
%%[8 import(qualified Data.Map as Map)
%%]
%%[20 import(qualified Data.Set as Set)
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
-- Core syntax and semantics
%%[(8 codegen grin) import(qualified {%{EH}Core} as Core, qualified {%{EH}Core.ToGrin} as Core2GrSem)
%%]
-- TyCore syntax and semantics
%%[(8 codegen) import(qualified {%{EH}TyCore} as C)
%%]

-- HI syntax and semantics
%%[20 import(qualified {%{EH}HI.MainAG} as HISem,qualified {%{EH}HI} as HI)
%%]

-- Module
%%[20 import(qualified EH.Util.Rel as Rel)
%%]
%%[20 import({%{EH}Module})
%%]
%%[20 import(qualified {%{EH}HS.ModImpExp} as HSSemMod)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: computing semantics
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(cpFoldCore)
cpFoldCore :: HsName -> EHCompilePhase ()
cpFoldCore modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 mbCore   = ecuMbCore ecu
                 core     = panicJust "cpFoldCore" mbCore
                 coreInh  = crsiCoreInh crsi
                 coreSem  = Core2GrSem.wrap_CodeAGItf
                              (Core2GrSem.sem_CodeAGItf (Core.CodeAGItf_AGItf core))
                              (coreInh { Core2GrSem.gUniq_Inh_CodeAGItf            = crsiHereUID crsi
                                       , Core2GrSem.opts_Inh_CodeAGItf             = crsiOpts crsi
                                       })
         ;  when (isJust mbCore)
                 (cpUpdCU modNm ( ecuStoreCoreSem coreSem
                                ))
         }
%%]

%%[8 export(cpFoldEH)
cpFoldEH :: HsName -> EHCompilePhase ()
cpFoldEH modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 mbEH   = ecuMbEH ecu
                 ehSem  = EHSem.wrap_AGItf (EHSem.sem_AGItf $ panicJust "cpFoldEH" mbEH)
                                           ((crsiEHInh crsi)
                                                  { EHSem.moduleNm_Inh_AGItf         = ecuModNm ecu
                                                  , EHSem.gUniq_Inh_AGItf            = crsiHereUID crsi
                                                  , EHSem.opts_Inh_AGItf             = opts
%%[[20
                                                  , EHSem.isMainMod_Inh_AGItf        = ecuIsMainMod ecu
%%]]
                                                  })
         ;  when (isJust mbEH)
                 (cpUpdCU modNm $! ecuStoreEHSem $! ehSem)
         }
%%]

%%[8 export(cpFoldHs)
cpFoldHs :: HsName -> EHCompilePhase ()
cpFoldHs modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 mbHS   = ecuMbHS ecu
                 inh    = crsiHSInh crsi
                 hsSem  = HSSem.wrap_AGItf (HSSem.sem_AGItf $ panicJust "cpFoldHs" mbHS)
                                           (inh { HSSem.opts_Inh_AGItf             = crsiOpts crsi
                                                , HSSem.gUniq_Inh_AGItf            = crsiHereUID crsi
%%[[20
                                                , HSSem.moduleNm_Inh_AGItf         = modNm
                                                , HSSem.isTopMod_Inh_AGItf         = ecuIsTopMod ecu
                                                , HSSem.modInScope_Inh_AGItf       = inscps
                                                , HSSem.modEntToOrig_Inh_AGItf     = exps
                                                , HSSem.topInstanceNmL_Inh_AGItf   = modInstNmL (ecuMod ecu)
%%]]
                                                })
%%[[20
                        where mmi    = panicJust "cpFoldHs.crsiModMp" $ Map.lookup modNm $ crsiModMp crsi
                              inscps = Rel.toDomMap $ mmiInscps $ mmi
                              exps   = Rel.toRngMap $ Rel.restrictRng (\o -> let mq = hsnQualifier (ioccNm o) in isJust mq && fromJust mq /= modNm)
                                                    $ Rel.mapRng mentIdOcc $ mmiExps $ mmi
%%]]
%%[[20
                 hasMain= HSSem.mainValExists_Syn_AGItf hsSem
%%]]
         ;  when (isJust mbHS)
                 (do { cpUpdCU modNm ( ecuStoreHSSem hsSem
%%[[20
                                     . ecuStoreHIDeclImpL (ecuHSDeclImpNmL ecu)
                                     -- . ecuSetHasMain hasMain
%%]]
                                     )
%%[[20
                     ; when (ehcOptVerbosity opts >= VerboseDebug)
                            (lift $ putStrLn (show modNm ++ " hasMain=" ++ show hasMain))
                     ; when hasMain
                            (do { cr <- get
                                ; let (crsi,opts) = crBaseInfo' cr
                                      mkerr lim ns = cpSetLimitErrs 1 "compilation run" [rngLift emptyRange Err_MayOnlyHaveNrMain lim ns modNm]
                                ; case crsiMbMainNm crsi of
                                    Just n                   -> mkerr 1 [n]
                                    _ | ehcOptDoLinking opts -> cpUpdSI (\crsi -> crsi {crsiMbMainNm = Just modNm})
                                      | otherwise            -> mkerr 0 []
                                })
%%]]
                     })
         }
%%]

%%[20 export(cpFoldHsMod)
cpFoldHsMod :: HsName -> EHCompilePhase ()
cpFoldHsMod modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,_,_) = crBaseInfo modNm cr
                 mbHS       = ecuMbHS ecu
                 inh        = crsiHSModInh crsi
                 hsSemMod   = HSSemMod.wrap_AGItf (HSSemMod.sem_AGItf $ panicJust "cpFoldHsMod" mbHS)
                                                  (inh { HSSemMod.gUniq_Inh_AGItf        = crsiHereUID crsi
                                                       , HSSemMod.moduleNm_Inh_AGItf     = modNm
                                                       })
                 hasMain= HSSemMod.mainValExists_Syn_AGItf hsSemMod
         ;  when (isJust mbHS)
                 (cpUpdCU modNm ( ecuStoreHSSemMod hsSemMod
                                . ecuSetHasMain hasMain
                 )              )
         }
%%]

%%[20 export(cpFoldHI)
cpFoldHI :: HsName -> EHCompilePhase ()
cpFoldHI modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 mbHI   = ecuMbPrevHI ecu
                 inh    = crsiHIInh crsi
                 hiSem  = HISem.wrap_AGItf (HISem.sem_AGItf $ panicJust "cpFoldHI" mbHI)
                                           (inh { HISem.opts_Inh_AGItf             = crsiOpts crsi
                                                })
                 hiSettings = maybe HI.emptyHiSettings id $ HISem.settings_Syn_AGItf hiSem
                 hasMain    = HI.hisettingsHasMain hiSettings
         ;  when (isJust mbHI && HISem.isValidVersion_Syn_AGItf hiSem)
                 (do { let mm     = crsiModMp crsi
                           mmi    = Map.findWithDefault emptyModMpInfo modNm mm
                           mmi'   = mkModMpInfo modNm (mmiInscps mmi) (HISem.exportRel_Syn_AGItf hiSem) (HISem.exportHideRel_Syn_AGItf hiSem)
                     ; cpUpdSI (\crsi -> crsi {crsiModMp = Map.insert modNm mmi' mm})
                     ; cpUpdCU modNm ( ecuStorePrevHISem hiSem
                                     . ecuStoreHIDeclImpL (HISem.asDeclImpModL_Syn_AGItf hiSem)
                                     . ecuStoreHIUsedImpL (HISem.asUsedImpModL_Syn_AGItf hiSem)
                                     . ecuSetHasMain hasMain
                                     )
                     ; when (ehcOptVerbosity opts >= VerboseDebug)
                            (lift $ putStrLn (show modNm ++ ": hi imps, decl=" ++ show (HISem.asDeclImpModL_Syn_AGItf hiSem) ++ ", used=" ++ show (HISem.asUsedImpModL_Syn_AGItf hiSem)))
                     })
         }
%%]





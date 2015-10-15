%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module analysis

%%[50 module {%{EH}EHC.CompilePhase.Module}
%%]

-- general imports
%%[50 import(qualified Data.Map as Map, qualified Data.Set as Set)
%%]
%%[50 import(qualified UHC.Util.Rel as Rel)
%%]
%%[50 import(UHC.Util.Time, UHC.Util.FPath)
%%]
%%[50 import(System.Directory)
%%]
%%[50 import(Control.Monad.State)
%%]

%%[50 import({%{EH}Base.Optimize})
%%]

%%[50 import({%{EH}EHC.Common})
%%]
%%[50 import({%{EH}EHC.CompileUnit})
%%]
%%[50 import({%{EH}EHC.CompileRun.Base})
%%]

%%[50 import({%{EH}Module.ImportExport})
%%]
%%[50 import(qualified {%{EH}Config} as Cfg)
%%]
%%[50 import(qualified {%{EH}HS.ModImpExp} as HSSemMod)
%%]

%%[(50 codegen corein) import(qualified {%{EH}Core.Check} as Core2ChkSem)
%%]
%%[(50 codegen corerunin) import(qualified {%{EH}CoreRun.Check} as CoreRun2ChkSem)
%%]

%%[(50 codegen) hs import({%{EH}CodeGen.RefGenerator})
%%]

%%[5050 import({%{EH}Base.Debug})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module analysis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(cpCheckModsModWith)
-- | Check module dependencies for given 'Mod'
cpCheckModsModWith :: EHCCompileRunner m => (HsName -> ModMpInfo) -> [Mod] -> EHCompilePhaseT m ()
cpCheckModsModWith dfltMod modL@(Mod {modName = modNm} : _)
  = do { cr <- get
       ; cpMsg modNm VerboseDebug $ "cpCheckModsModWith modL: " ++ show modL
       ; let crsi   = _crStateInfo cr
             (mm,e) = modMpCombine' dfltMod modL (crsiModMp crsi)
       ; cpUpdSI (\crsi -> crsi {crsiModMp = mm})
%%[[5050
       ; when (ehcOptVerbosity (crsi ^. crsiOpts) >= VerboseDebug)
              (do { cpMsg modNm VerboseDebug "cpCheckModsModWith"
                  ; liftIO $ putWidthPPLn 120 (pp modNm >-< pp modL >-< ppModMp mm)
                  })
%%][10110
%%]]
       ; cpSetLimitErrsWhen 5 "Module analysis" e
       }
%%]

%%[50 export(cpCheckModsWithOrWithoutBuiltin)
cpCheckModsWithOrWithoutBuiltin :: EHCCompileRunner m => Bool -> [HsName] -> EHCompilePhaseT m ()
cpCheckModsWithOrWithoutBuiltin bltin modNmL@(modNm:_)
  = do { cr <- get
       ; cpMsg modNm VerboseDebug $ "cpCheckModsWithOrWithoutBuiltin modNmL: " ++ show modNmL
       ; let modL   = [ addBuiltin $ ecuMod $ crCU n cr | n <- modNmL ]
       ; cpCheckModsModWith (\n -> panic $ "cpCheckModsWithOrWithoutBuiltin: " ++ show n) modL
       }
  where addBuiltin | bltin     = \m -> m { modImpL = modImpBuiltin : modImpL m }
                   | otherwise = id
%%]

%%[50 export(cpCheckModsWithBuiltin)
cpCheckModsWithBuiltin :: EHCCompileRunner m => [HsName] -> EHCompilePhaseT m ()
cpCheckModsWithBuiltin = cpCheckModsWithOrWithoutBuiltin True
{-# INLINE cpCheckModsWithBuiltin #-}
%%]

%%[50 export(cpCheckModsWithoutBuiltin)
cpCheckModsWithoutBuiltin :: EHCCompileRunner m => [HsName] -> EHCompilePhaseT m ()
cpCheckModsWithoutBuiltin = cpCheckModsWithOrWithoutBuiltin False
{-# INLINE cpCheckModsWithoutBuiltin #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Get info for module analysis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(5050 corerunin) export(cpGetCoreRunModnameAndImports)
cpGetCoreRunModnameAndImports :: EHCCompileRunner m => HsName -> EHCompilePhaseT m HsName
cpGetCoreRunModnameAndImports modNm
  =  do  {  cr <- get
         ;  let  (ecu,_,opts,_) = crBaseInfo modNm cr
                 mbCrSemMod = _ecuMbCoreRunSemMod ecu
                 crSemMod   = panicJust "cpGetCoreRunModnameAndImports" mbCrSemMod
                 modNm'     = CoreRun2ChkSem.realModuleNm_Syn_AGItf crSemMod
         ;  cpMsg modNm VerboseDebug $ "cpGetCoreRunModnameAndImports: " ++ show modNm ++ " -> " ++ show modNm'
         ;  case mbCrSemMod of
              Just _ -> cpUpdCUWithKey modNm $ \_ ecu ->
                          ( modNm'
                          , ecuStoreSrcDeclImpS (Set.fromList $ CoreRun2ChkSem.impModNmL_Syn_AGItf crSemMod )
                            $ cuUpdKey modNm' ecu
                          )
              _      -> return modNm
         }
%%]

%%[(50 corein) export(cpGetCoreModnameAndImports)
cpGetCoreModnameAndImports :: EHCCompileRunner m => HsName -> EHCompilePhaseT m HsName
cpGetCoreModnameAndImports modNm
  =  do  {  cr <- get
         ;  let  (ecu,_,opts,_) = crBaseInfo modNm cr
                 mbCrSemMod = _ecuMbCoreSemMod ecu
                 crSemMod   = panicJust "cpGetCoreModnameAndImports" mbCrSemMod
                 modNm'     = Core2ChkSem.realModuleNm_Syn_CodeAGItf crSemMod
         ;  cpMsg modNm VerboseDebug $ "cpGetCoreModnameAndImports: " ++ show modNm ++ " -> " ++ show modNm'
         ;  case mbCrSemMod of
              Just _ -> cpUpdCUWithKey modNm $ \_ ecu ->
                          ( modNm'
                          , ecuStoreSrcDeclImpS (Set.fromList $ Core2ChkSem.impModNmL_Syn_CodeAGItf crSemMod )
                            $ cuUpdKey modNm' ecu
                          )
              _      -> return modNm
         }
%%]

%%[50 export(cpGetHsModnameAndImports)
cpGetHsModnameAndImports :: EHCCompileRunner m => HsName -> EHCompilePhaseT m HsName
cpGetHsModnameAndImports modNm
  =  do  {  cr <- get
         ;  let  (ecu,_,opts,_) = crBaseInfo modNm cr
                 mbHsSemMod = _ecuMbHSSemMod ecu
                 hsSemMod   = panicJust "cpGetHsModnameAndImports" mbHsSemMod
                 modNm'     = HSSemMod.realModuleNm_Syn_AGItf hsSemMod
                 upd        = ecuStoreSrcDeclImpS ( -- (\v -> tr "XX" (pp $ Set.toList v) v) $ 
                                                  HSSemMod.modImpNmS_Syn_AGItf hsSemMod)
         ;  case mbHsSemMod of
              Just _ | _ecuIsTopMod ecu -> cpUpdCUWithKey modNm (\_ ecu -> (modNm', upd $ cuUpdKey modNm' ecu))
                     | otherwise        -> do { cpUpdCU modNm upd ; return modNm }
              _      -> return modNm
         }
%%]

%%[50 export(cpGetHsMod)
cpGetHsMod :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpGetHsMod modNm
  =  do  {  cr <- get
         ;  let  (ecu,_,opts,_) = crBaseInfo modNm cr
                 mbHsSemMod = _ecuMbHSSemMod ecu
                 hsSemMod   = panicJust "cpGetHsMod" mbHsSemMod
                 mod        = HSSemMod.mod_Syn_AGItf hsSemMod
         ;  when (ehcOptVerbosity opts >= VerboseDebug)
                 (do { cpMsg modNm VerboseDebug "cpGetHsMod"
                     ; liftIO $ putWidthPPLn 120 (pp mod)
                     })
         ;  when (isJust mbHsSemMod)
                 (cpUpdCU modNm (ecuStoreMod mod))
         }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Create dummy module info for .eh's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(cpGetDummyCheckSrcMod)
cpGetDummyCheckSrcMod :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpGetDummyCheckSrcMod modNm
  = do { cr <- get
       ; let crsi   = _crStateInfo cr
             mm     = crsiModMp crsi
             mod    = Mod modNm Nothing Nothing [] Rel.empty Rel.empty []
       ; cpUpdCU modNm (ecuStoreMod mod)
       ; cpUpdSI (\crsi -> crsi {crsiModMp = Map.insert modNm emptyModMpInfo mm})
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Update module offset info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 codegen) export(cpUpdateModOffMp)
cpUpdateModOffMp :: EHCCompileRunner m => [HsName] -> EHCompilePhaseT m ()
cpUpdateModOffMp modNmL@(modNm:_)
  = do { cr <- get
       ; cpMsg modNm VerboseDebug "cpUpdateModOffMp"
       ; let crsi   = _crStateInfo cr
             offMp  = crsiModOffMp crsi
             (offMp',_)
                    = foldr add (offMp, Map.size offMp) modNmL
                    where add modNm (offMp, offset)
                            = case Map.lookup modNm offMp of
                                Just (o,_) -> (Map.insert modNm (o, new) offMp, offset )
                                _          -> (Map.insert modNm (o, new) offMp, offset')
                                           where (o, offset') = refGen1 offset 1 modNm
                            where new = crsiExpNmOffMp modNm crsi
       ; cpUpdSI (\crsi -> crsi {crsiModOffMp = offMp'})
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Generate list of all imported modules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 codegen) export(cpGenImportNameInfo)
-- | Compute imported module names
cpGenImportNameInfo :: EHCCompileRunner m => HsName -> EHCompilePhaseT m [HsName]
cpGenImportNameInfo modNm
  = do { cr <- get
       ; let (ecu,crsi,opts,fp) = crBaseInfo modNm cr
             isWholeProg = ehcOptOptimizationScope opts > OptimizationScope_PerModule
             impNmL     | isWholeProg = []
                        | otherwise   = ecuImpNmL ecu
       ; return impNmL
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Update new hidden exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(92 codegen) export(cpUpdHiddenExports)
cpUpdHiddenExports :: EHCCompileRunner m => HsName -> [(HsName,IdOccKind)] -> EHCompilePhaseT m ()
cpUpdHiddenExports modNm exps
  = when (not $ null exps)
         (do { cpUpdSI (\crsi -> crsi { crsiModMp = modMpAddHiddenExps modNm exps $ crsiModMp crsi
                                      })
             ; cpUpdateModOffMp [modNm]
             })

%%]


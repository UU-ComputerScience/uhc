%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Translation to another AST

%%[(50 codegen) module {%{EH}EHC.CompilePhase.Common}
%%]

-- general imports
%%[(50 codegen) import(qualified Data.Map as Map, qualified Data.Set as Set)
%%]
%%[(50 codegen) import(UHC.Util.Lens, qualified UHC.Util.FastSeq as Seq)
%%]
%%[(50 codegen) import(Control.Monad.State)
%%]
%%[(50 codegen) import({%{EH}EHC.Common})
%%]
%%[(50 codegen) import({%{EH}Base.Optimize})
%%]
%%[(50 codegen) import({%{EH}CodeGen.RefGenerator})
%%]

-- Driver
%%[(50 codegen) import({%{EH}EHC.CompileRun})
%%]
%%[(50 codegen) import({%{EH}EHC.CompilePhase.Module})
%%]
%%[(50 codegen) import({%{EH}EHC.CompileUnit})
%%]

-- Core semantics
%%[(50 codegen) import(qualified {%{EH}Core.ToGrin} as Core2GrSem)
%%]

-- ModuleImportExportImpl
%%[(50 codegen) import({%{EH}CodeGen.ModuleImportExportImpl})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Shared/common utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 core) export(cpGenModuleImportExportImpl)
-- | Compute impl info for module codegen
cpGenModuleImportExportImpl :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ModuleImportExportImpl
cpGenModuleImportExportImpl modNm
  = do { cr <- get
       ; cpMsg modNm VerboseDebug "cpGenModuleImportExportImpl"
       ; impNmL <- cpGenImportNameInfo modNm
       ; let (ecu,crsi,opts,fp) = crBaseInfo modNm cr
             isWholeProg = ehcOptOptimizationScope opts > OptimizationScope_PerModule
             expNmFldMp | ecuIsMainMod ecu = Map.empty
                        | otherwise        = crsiExpNmOffMp modNm crsi
             modOffMp   | isWholeProg = Map.filterWithKey (\n _ -> n == modNm) $ crsiModOffMp crsi
                        | otherwise   = crsiModOffMp crsi
       -- ; liftIO $ putStrLn $ "cpGenModuleImportExportImpl " ++ show impNmL
       -- ; liftIO $ putStrLn $ "cpGenModuleImportExportImpl ecuHSDeclImpNmS " ++ show (ecuHSDeclImpNmS ecu)
       -- ; liftIO $ putStrLn $ "cpGenModuleImportExportImpl ecuHIDeclImpNmS " ++ show (ecuHIDeclImpNmS ecu)
       -- ; liftIO $ putStrLn $ "cpGenModuleImportExportImpl ecuHIUsedImpNmS " ++ show (ecuHIUsedImpNmS ecu)
       -- ; liftIO $ putStrLn $ "cpGenModuleImportExportImpl modOffMp " ++ show modOffMp
       -- ecuHSDeclImpNmS ecu, ecuHIDeclImpNmS ecu, ecuHIUsedImpNmS ecu
       ; return $ emptyModuleImportExportImpl
           { mieimplUsedModNmL 		= if ecuIsMainMod ecu then [ m | (m,_) <- sortOnLazy snd $ Map.toList $ Map.map fst modOffMp ] else []
           , mieimplHsName2FldMpMp 	= Map.fromList
               [ (n,(o,mp))
               | (n,o) <- refGen 0 1 impNmL
               , let (_,mp) = panicJust ("cpGenModuleImportExportImpl: " ++ show n) (Map.lookup n (crsiModOffMp crsi))
               ]
           , mieimplHsName2FldMp 	= expNmFldMp
%%[[(50 grin)
           , mieimplLamMp 			= crsi ^. crsiCEnv ^. cenvLamMp -- Core2GrSem.lamMp_Inh_CodeAGItf $ _crsiCoreInh crsi
%%]]
           }
       }
%%]


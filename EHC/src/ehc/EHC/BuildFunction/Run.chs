%%[0 hs
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE RecursiveDo #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Build function running
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Running of BuildFunction
%%]

%%[8 module {%{EH}EHC.BuildFunction.Run}
%%]

-- build function
%%[8 import ({%{EH}EHC.BuildFunction}) export(module {%{EH}EHC.BuildFunction})
%%]

-- compiler driver
%%[8 import ({%{EH}EHC.Common}, {%{EH}EHC.CompileRun.Base}, {%{EH}EHC.CompileUnit}, {%{EH}EHC.FileSuffMp})
%%]
%%[8888 import ({%{EH}EHC.Main.Compile})
%%]

-- low level (compilation) passes
%%[8 import ({%{EH}EHC.CompilePhase.CompileC})
%%]

-- package
%%[99 import ({%{EH}Base.PackageDatabase})
%%]

-- parsing, scanning
%%[8 import ({%{EH}Base.ParseUtils}, UHC.Util.ScanUtils as ScanUtils)
%%]

-- (pretty)printing
%%[8888 import (UHC.Util.Pretty)
%%]

-- source handling
%%[8 import ({%{EH}EHC.ASTHandler}, {%{EH}EHC.ASTHandler.Instances})
%%]

-- general imports
%%[8 import (qualified {%{EH}Config} as Cfg)
%%]
%%[8 import({%{EH}Opts.CommandLine})
%%]
%%[8 import({%{EH}Base.Optimize})
%%]
%%[8 import({%{EH}CodeGen.RefGenerator})
%%]
%%[8 import (UHC.Util.Lens)
%%]
%%[8888 import (qualified UHC.Util.RelMap as Rel)
%%]
%%[8 import (Data.Typeable)
%%]
%%[8 import (Data.Maybe, qualified Data.Map as Map, qualified Data.Set as Set, qualified UHC.Util.FastSeq as Seq)
%%]
%%[8 import (Control.Applicative, Control.Monad.State, Control.Monad.Error)
%%]

%%[50 import(qualified UHC.Util.Rel as Rel)
%%]
-- timestamps
%%[50 import(UHC.Util.Time, System.Directory)
%%]

-- HS Module
%%[50 import({%{EH}Module.ImportExport}, qualified {%{EH}HS.ModImpExp} as HSSemMod)
%%]
%%[50 import({%{EH}CodeGen.ModuleImportExportImpl})
%%]
%%[(92 codegen) import({%{EH}EHC.CompilePhase.Module(cpUpdHiddenExports)})
%%]

-- EH semantics
%%[8 import(qualified {%{EH}EH} as EH)
%%]
%%[8 import(qualified {%{EH}EH.Main} as EHSem)
%%]

-- CHR solver
%%[(50 hmtyinfer) import({%{EH}CHR.Solve}(chrStoreUnion))
%%]

-- HS semantics
%%[8 import(qualified {%{EH}HS} as HS)
%%]
%%[8 import(qualified {%{EH}HS.MainAG} as HSSem)
%%]

-- Core syntax and semantics
-- TBD: this depends on grin gen, but should also be available for Core, so in a CoreXXXSem
%%[(8 core) import(qualified {%{EH}Core} as Core, qualified {%{EH}Core.ToGrin} as Core2GrSem)
%%]
%%[(8 core corerun) import(qualified {%{EH}Core.ToCoreRun} as Core2CoreRunSem)
%%]
%%[(50 codegen corein) import(qualified {%{EH}Core.Check} as Core2ChkSem)
%%]
%%[(8 codegen) import({%{EH}Core.Trf.ElimNonCodegenConstructs})
%%]

-- CoreRun syntax and semantics
%%[(8 corerun) import(qualified {%{EH}CoreRun} as CoreRun)
%%]
%%[(50 codegen corerunin) import(qualified {%{EH}CoreRun.Check} as CoreRun2ChkSem)
%%]

-- Pragma
%%[99 import({%{EH}Base.Pragma})
%%]

-- HI syntax and semantics
%%[50 import(qualified {%{EH}HI} as HI)
%%]

-- Misc info: LamInfo/LamMp
%%[(8 codegen) hs import({%{EH}LamInfo})
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Build function calling/running
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(bcall)
-- | Execute a build function, possibly caching/memoizing a result
bcall :: forall res m . (Typeable res, EHCCompileRunner m) => BFun' res -> EHCompilePhaseT m res
bcall bfun = do
    -- query cache
    bcache      <- getl $ st ^* bstateCache
    mbCachedRes <- lkup bfun bcache
    
    case mbCachedRes of
      Just res -> do
        -- debug/trace
        cpTr TraceOn_BuildFun ["cache " ++ show bfun]
        -- immediate result
        return res

      _ -> do
        -- debug/trace
        getl cstk >>= \stk -> cpTr TraceOn_BuildFun $ [">>>>> " ++ show bfun] ++ map show stk
        -- prepare
        start
        
        -- actual execution
        res <- case bfun of
          CRSI -> brefto bfun BRef_CRSI

%%[[50
{-
          CRSIWithCompileOrder modNmFrom compileOrder forAstTypes -> do
               case compileOrder of
                   _ | length mutRecL > 0 -> cpSetLimitErrs 1 "compilation run" [rngLift emptyRange Err_MutRecModules mutRecL]
                     | otherwise          -> forM_ compileOrder $ \[modNm] -> flow1 modNm
                     where mutRecL = filter ((> 1) . length) compileOrder
                           
                           -- Flow module info into global state
                           flow1 modNm = do
                               -- From: cpFlowHsSem1
                               updHS <- guardMaybeM (allowFlow ASTType_HS) (bGetHsSemMb modNm) (return id) $ \hsSem -> do
                                 return $ \crsi ->
                                   let opts   = crsi ^. crsiOpts
                                       ehInh  = crsi ^. crsiEHInh
                                       hsInh  = crsi ^. crsiHSInh
                                       ig     = HSSem.gathIdGam_Syn_AGItf hsSem
                                       fg     = HSSem.gathFixityGam_Syn_AGItf hsSem
                                       ehInh' = ehInh
                                         { EHSem.idQualGam_Inh_AGItf  = idGam2QualGam ig `gamUnion` EHSem.idQualGam_Inh_AGItf ehInh
                                         }
%%[[50
                                       mk = idQualGamReplacement (EHSem.idQualGam_Inh_AGItf ehInh')
%%][99
                                       mk = if ehcOptUseAssumePrelude opts
                                            then \_ n -> n
                                            else \k n -> idQualGamReplacement (EHSem.idQualGam_Inh_AGItf ehInh') k (hsnQualified n)
%%]]
                                   in  crsi
                                         { -- From: cpFlowHsSem1
                                           _crsiHSInh = hsInh
                                              { HSSem.idGam_Inh_AGItf      = ig `gamUnion` HSSem.idGam_Inh_AGItf     hsInh
                                              , HSSem.fixityGam_Inh_AGItf  = fg `gamUnion` HSSem.fixityGam_Inh_AGItf hsInh
                                              }
                                         , _crsiEHInh = ehInh'
                                         , _crsiOpts = opts
                                              { ehcOptBuiltinNames = mkEHBuiltinNames mk
                                              }
                                         }
                               -- From: cpFlowEHSem1
                               updEH <- guardMaybeM (allowFlow ASTType_EH) (bcall $ FoldEHMb $ mkPrevFileSearchKeyWithName modNm) (return id) $ \ehSem -> do
                                 return $ \crsi ->
                                   let opts   = crsi ^. crsiOpts
                                       ehInh  = crsi ^. crsiEHInh
%%[[(8 codegen)
                                       coreInh= crsi ^. crsiCoreInh
%%]]
%%[[50
                                       mmi      = panicJust "cpFlowEHSem1.crsiModMp" $ Map.lookup modNm $ crsiModMp crsi
                                       mentrelFilterMp
%%[[(8 codegen hmtyinfer)
                                                = mentrelFilterMpUnions [ EHSem.gathMentrelFilterMp_Syn_AGItf ehSem, mentrelToFilterMp' False [modNm] (mmiExps mmi) ]
%%][8
                                                = mentrelToFilterMp' False [modNm] (mmiExps mmi)
%%]]
                                       usedImpS = mentrelFilterMpModuleNames mentrelFilterMp
                                       ehInh' = ehInh
%%[[(50 hmtyinfer)
                                         { EHSem.dataGam_Inh_AGItf    = EHSem.gathDataGam_Syn_AGItf    ehSem `gamUnion`  EHSem.dataGam_Inh_AGItf    ehInh
                                         , EHSem.valGam_Inh_AGItf     = EHSem.gathValGam_Syn_AGItf     ehSem `gamUnion`  EHSem.valGam_Inh_AGItf     ehInh
                                         , EHSem.tyGam_Inh_AGItf      = EHSem.gathTyGam_Syn_AGItf      ehSem `gamUnion`  EHSem.tyGam_Inh_AGItf      ehInh
                                         , EHSem.tyKiGam_Inh_AGItf    = EHSem.gathTyKiGam_Syn_AGItf    ehSem `gamUnion`  EHSem.tyKiGam_Inh_AGItf    ehInh
                                         , EHSem.polGam_Inh_AGItf     = EHSem.gathPolGam_Syn_AGItf     ehSem `gamUnion`  EHSem.polGam_Inh_AGItf     ehInh
                                         , EHSem.kiGam_Inh_AGItf      = EHSem.gathKiGam_Syn_AGItf      ehSem `gamUnion`  EHSem.kiGam_Inh_AGItf      ehInh
                                         , EHSem.clGam_Inh_AGItf      = EHSem.gathClGam_Syn_AGItf      ehSem `gamUnion`  EHSem.clGam_Inh_AGItf      ehInh
                                         , EHSem.clDfGam_Inh_AGItf    = EHSem.gathClDfGam_Syn_AGItf    ehSem `gamUnion`  EHSem.clDfGam_Inh_AGItf    ehInh
                                         , EHSem.chrStore_Inh_AGItf   = EHSem.gathChrStore_Syn_AGItf   ehSem `chrStoreUnion` EHSem.chrStore_Inh_AGItf   ehInh
                                         }
%%]]
%%[[(8 codegen)
                                       coreInh' = coreInh
%%[[8
                                         { Core2GrSem.dataGam_Inh_CodeAGItf = EHSem.gathDataGam_Syn_AGItf ehSem
                                         , Core2GrSem.lamMp_Inh_CodeAGItf   = EHSem.gathLamMp_Syn_AGItf   ehSem
%%][50
                                         { Core2GrSem.dataGam_Inh_CodeAGItf = EHSem.dataGam_Inh_AGItf     ehInh'
                                         , Core2GrSem.lamMp_Inh_CodeAGItf   = lm `lamMpUnionBindAspMp` Core2GrSem.lamMp_Inh_CodeAGItf coreInh      -- assumption: no duplicates, otherwise merging as done later has to be done
%%]]
                                         }
%%]]
%%[[(50 codegen)
                                       lm       = EHSem.gathLamMp_Syn_AGItf      ehSem
%%]]
%%]]
                                   in  crsi -- From: cpFlowEHSem1
%%[[(8 codegen)
                                         { _crsiCoreInh = coreInh' }
%%][(50 codegen)
                                         { _crsiCoreInh = coreInh', _crsiEHInh = ehInh' }
%%][50
                                         { _crsiEHInh = ehInh' }
%%]]
                               updCore <- guardMaybeM (allowFlow ASTType_Core) (bcall $ FoldCore2GrinMb $ mkPrevFileSearchKeyWithName modNm) (return id) $ \core2GrinSem -> do
                                 return $ \crsi ->
                                   let 
                                   in  crsi
            
                               cpUpdSI $ updCore . updEH . updHS
                       
                             where allowFlow = bAllowFlow modNm forAstTypes ASTSemFlowStage_BetweenModule
                 
               brefto bfun BRef_CRSI
-}

          CRSIWithCompileOrderP modNmFrom compileOrder astpipe -> do
               -- let forAstTypes = 
               case compileOrder of
                   _ | length mutRecL > 0 -> cpSetLimitErrs 1 "compilation run" [rngLift emptyRange Err_MutRecModules mutRecL]
                     | otherwise          -> forM_ compileOrder $ \[modNm] -> flow1 modNm
                     where mutRecL = filter ((> 1) . length) compileOrder
                           
                           -- Flow module info into global state
                           flow1 modNm = do
                               -- From: cpFlowHsSem1
                               updHS <- maybe2M
                                   -- (allowFlow $ \p -> astpType p == ASTType_HS)
                                   (allowFlow $ astpMbFromHSToEH True)
                                   (bGetHsSemPMb modNm) (return id) $ \(hsSem) -> do
                                 return $ \crsi ->
                                   let hsInh  = crsi ^. crsiHSInh
                                       ig     = HSSem.gathIdGam_Syn_AGItf hsSem
                                       fg     = HSSem.gathFixityGam_Syn_AGItf hsSem
                                   in  crsi
                                         { -- From: cpFlowHsSem1
                                           _crsiHSInh = hsInh
                                              { HSSem.idGam_Inh_AGItf      = ig `gamUnion` HSSem.idGam_Inh_AGItf     hsInh
                                              , HSSem.fixityGam_Inh_AGItf  = fg `gamUnion` HSSem.fixityGam_Inh_AGItf hsInh
                                              }
                                         }

                               -- From: cpFlowEHSem1
                               updEH <- maybe2M
                                   -- (allowFlow $ \p -> astpType p == ASTType_EH)
                                   (allowFlow $ astpMbFromEH True)
                                   (\p -> bcall $ FoldEHPMb (mkPrevFileSearchKeyWithName modNm) p) (return id) $ \ehSem -> do
                                 return $ \crsi ->
                                   let opts   = crsi ^. crsiOpts
                                       ehInh  = crsi ^. crsiEHInh
                                       ehInh' = ehInh
%%[[(50 hmtyinfer)
                                         { EHSem.dataGam_Inh_AGItf    = EHSem.gathDataGam_Syn_AGItf    ehSem `gamUnion`  EHSem.dataGam_Inh_AGItf    ehInh
                                         , EHSem.valGam_Inh_AGItf     = EHSem.gathValGam_Syn_AGItf     ehSem `gamUnion`  EHSem.valGam_Inh_AGItf     ehInh
                                         , EHSem.tyGam_Inh_AGItf      = EHSem.gathTyGam_Syn_AGItf      ehSem `gamUnion`  EHSem.tyGam_Inh_AGItf      ehInh
                                         , EHSem.tyKiGam_Inh_AGItf    = EHSem.gathTyKiGam_Syn_AGItf    ehSem `gamUnion`  EHSem.tyKiGam_Inh_AGItf    ehInh
                                         , EHSem.polGam_Inh_AGItf     = EHSem.gathPolGam_Syn_AGItf     ehSem `gamUnion`  EHSem.polGam_Inh_AGItf     ehInh
                                         , EHSem.kiGam_Inh_AGItf      = EHSem.gathKiGam_Syn_AGItf      ehSem `gamUnion`  EHSem.kiGam_Inh_AGItf      ehInh
                                         , EHSem.clGam_Inh_AGItf      = EHSem.gathClGam_Syn_AGItf      ehSem `gamUnion`  EHSem.clGam_Inh_AGItf      ehInh
                                         , EHSem.clDfGam_Inh_AGItf    = EHSem.gathClDfGam_Syn_AGItf    ehSem `gamUnion`  EHSem.clDfGam_Inh_AGItf    ehInh
                                         , EHSem.chrStore_Inh_AGItf   = EHSem.gathChrStore_Syn_AGItf   ehSem `chrStoreUnion` EHSem.chrStore_Inh_AGItf   ehInh
                                         }
%%]]
                                   in  crsi -- From: cpFlowEHSem1
                                         { _crsiEHInh = ehInh' }

%%[[(50 core corein)
                               -- From: cpFlowCoreModSem
                               updCoreSrc <- maybe2M
                                   (allowFlow $ astpMbFromCoreSrc True)
                                   (\p -> bcall $ FoldCoreModPMb (mkPrevFileSearchKeyWithName modNm) p) (return id) $ \(coreChkSem, _, _, _, _, _) -> do
                                 return $ crsiCoreInh ^$= \coreInh ->
                                   coreInh { Core2GrSem.dataGam_Inh_CodeAGItf = Core2GrSem.dataGam_Inh_CodeAGItf coreInh `gamUnion` Core2ChkSem.gathDataGam_Syn_CodeAGItf coreChkSem }
%%][50
                               let updCoreSrc = id
%%]]

%%[[(50 corerun corerunin)
                               -- From: cpFlowCoreModSem
                               updCoreRunSrc <- maybe2M
                                   (allowFlow $ astpMbFromCoreRunSrc True)
                                   (\p -> bcall $ FoldCoreRunModPMb (mkPrevFileSearchKeyWithName modNm) p) (return id) $ \(corerunChkSem, _, _, _, _, _) -> do
                                 return $ (crsiCoreRunState ^* crcrsiNm2RefMp) ^$= \coreRunInh ->
                                   CoreRun2ChkSem.nm2ref_Syn_AGItf corerunChkSem `CoreRun.nm2refUnion` coreRunInh
%%][50
                               let updCoreRunSrc = id
%%]]

%%[[(50 core)
                               -- From: cpFlowCoreSemAfterFold
                               updCoreGrin <- maybe2M
                                   (allowFlow $ astpMbFromCoreToGrin True)
                                   (\p -> bcall $ FoldCore2GrinPMb (mkPrevFileSearchKeyWithName modNm) p) (return id) $ \core2GrinSem -> do
                                 return $ crsiCoreInh ^$= \coreInh ->
                                   -- assumption: old info can safely be overridden, otherwise merge should be done here
                                   coreInh { Core2GrSem.lamMp_Inh_CodeAGItf = Core2GrSem.gathLamMp_Syn_CodeAGItf core2GrinSem `lamMpUnionBindAspMp` Core2GrSem.lamMp_Inh_CodeAGItf coreInh }
%%][50
                               let updCoreGrin = id
%%]]
            
%%[[(50 core corerun)
                               -- From: cpFoldCore2CoreRun
                               updCoreCoreRun <- maybe2M
                                   (allowFlow $ astpMbFromCoreToCoreRun True)
                                   (\p -> bcall $ FoldCore2CoreRunPMb (mkPrevFileSearchKeyWithName modNm) p) (return id) $ \core2corerunSem -> do
                                 return $ (crsiCoreRunState ^* crcrsiNm2RefMp) ^$= \core2RunInh -> 
                                   Core2CoreRunSem.nm2refGath_Syn_CodeAGItf core2corerunSem `CoreRun.nm2refUnion` core2RunInh
%%][50
                               let updCoreCoreRun = id
%%]]
            
                               cpUpdSI $ updCoreRunSrc . updCoreCoreRun . updCoreSrc . updCoreGrin . updEH . updHS
                       
                             where allowFlow = bAllowFlowP modNm astpipe ASTSemFlowStage_BetweenModule
                 
               brefto bfun BRef_CRSI

{-
          CRSIWithImps ((modNm,_),mbPrev) imps forAstTypes -> do
               impsmp <- bcall $ ImportsRecursiveWithImps mbPrev imps
               let compileOrder = scc [ (n, Set.toList i) | (n,i) <- Map.toList impsmp ]
               cpTr TraceOn_BuildSccImports $ [show modNm ++ " " ++ show imps] ++ [show compileOrder]
               bcall $ CRSIWithCompileOrder modNm compileOrder forAstTypes
-}

          CRSIWithImpsP ((modNm,_),mbPrev) imps astpipe -> do
               impsmp <- bcall $ ImportsRecursiveWithImps mbPrev imps
               let compileOrder = scc [ (n, Set.toList i) | (n,i) <- Map.toList impsmp ]
               cpTr TraceOn_BuildSccImports $ [show modNm ++ " " ++ show imps] ++ [show compileOrder]
               bcall $ CRSIWithCompileOrderP modNm compileOrder astpipe
%%]]

          CRSIOfName modSearchKey forAstType -> do
               bcall $ CRSIOfNameP modSearchKey $ mkASTPipeForASTTypes [minBound .. forAstType]

          CRSIOfNameP modSearchKey@((modNmAsked,_),_) astpipe -> do
               let forAstType  = astpType astpipe
                   forAstTypes = astpTypes astpipe
%%[[8
               let modNm = modNmAsked
%%][50
               (modNm, imps) <- bcall $ ImportsOfName modSearchKey
               let modSearchKey' = updPrevFileSearchKeyWithName modNm modSearchKey
%%]]
               let allowFlow = bAllowFlowP modNm astpipe ASTSemFlowStage_PerModule
%%[[50
               ecu <- bcall $ EcuOfPrevNameAndPath modSearchKey'
               cpTrPP TraceOn_BuildPipe $ [pp "CRSIOfNameP ASTPipe", pp astpipe]
               
               -- first recursively take care of all imports
               bcall $ CRSIWithImpsP (mkPrevFileSearchKeyWithNameMbPrev modNm (_ecuMbPrevSearchInfo ecu)) imps astpipe
%%]]
%%[[50
               -- flow HS semantics forward into global state for current module, for a next stage in the pipeline
               -- From: cpFlowHsSem1
               maybe2M -- (allowFlow $ \p -> astpType p == ASTType_HS)
                       (allowFlow $ astpMbFromHSToEH True)
                       (bGetHsSemPMb modNm) (return ()) $ \hsSem -> do
                   cpUpdSI $ \crsi -> 
                       let ehInh  = crsi ^. crsiEHInh
                           ig     = HSSem.gathIdGam_Syn_AGItf hsSem
                           ehInh' = ehInh
                             { EHSem.idQualGam_Inh_AGItf  = idGam2QualGam ig `gamUnion` EHSem.idQualGam_Inh_AGItf ehInh
                             }
                       in  crsi
                             { _crsiEHInh = ehInh'
                             }
               -- flow HS semantics forward into global state for current module, for a next stage in the pipeline
               -- From: cpFlowHsSem1
               maybe2M -- (allowFlow $ \p -> astpType p == ASTType_HS)
                       (allowFlow $ astpMbFromHS True)
                       (bGetHsSemPMb modNm) (return ()) $ \hsSem -> do
                   cpUpdSI $ \crsi -> 
                       let ehInh  = crsi ^. crsiEHInh
                           opts   = crsi ^. crsiOpts
%%[[50
                           mk = idQualGamReplacement (EHSem.idQualGam_Inh_AGItf ehInh)
%%][99
                           mk = if ehcOptUseAssumePrelude opts
                                then \_ n -> n
                                else \k n -> idQualGamReplacement (EHSem.idQualGam_Inh_AGItf ehInh) k (hsnQualified n)
%%]]
                       in  crsi
                             { _crsiOpts = opts
                                  { ehcOptBuiltinNames = mkEHBuiltinNames mk
                                  }
                             }

               -- flow EH semantics forward into global state for current module, for a next stage in the pipeline
               -- From: cpFlowEHSem1
               maybe2M -- (allowFlow $ \p -> astpType p == ASTType_EH)
                       (allowFlow $ astpMbFromEH True)
                       (\p -> bcall $ FoldEHPMb (mkPrevFileSearchKeyWithName modNm) p) (return ()) $ \ehSem -> do
                   crsi <- bcall CRSI
                   let mmi      = panicJust "cpFlowEHSem1.crsiModMp" $ Map.lookup modNm $ crsiModMp crsi
                       mentrelFilterMp
%%[[(50 codegen hmtyinfer)
                                = mentrelFilterMpUnions [ EHSem.gathMentrelFilterMp_Syn_AGItf ehSem, mentrelToFilterMp' False [modNm] (mmiExps mmi) ]
%%][50
                                = mentrelToFilterMp' False [modNm] (mmiExps mmi)
%%]]
                       usedImpS = mentrelFilterMpModuleNames mentrelFilterMp
%%[[50
                   bUpdECU modNm ( ecuStoreHIUsedImpS usedImpS
%%[[99
                                 . ecuStoreUsedNames mentrelFilterMp
%%]]
                                 )                   
%%]]
%%[[(92 codegen)
                   -- put back additional hidden exports
                   cpUpdHiddenExports modNm $ Seq.toList $ EHSem.gathHiddenExports_Syn_AGItf ehSem
%%]]
%%]]

%%[[(8 core)
               -- flow EH semantics specific for Core forward into global state for current module, for a next stage in the pipeline
               -- From: cpFlowEHSem1
               maybe2M -- (allowFlow $ \p -> astpType p == ASTType_EH)
                       (allowFlow $ astpMbFromEHToCore True)
                       (\p -> bcall $ FoldEHPMb (mkPrevFileSearchKeyWithName modNm) p) (return ()) $ \ehSem -> do
                   cpUpdSI $ \crsi -> 
                       let ehInh  = crsi ^. crsiEHInh
                           coreInh= crsi ^. crsiCoreInh
                           coreInh' = coreInh
%%[[8
                             { Core2GrSem.dataGam_Inh_CodeAGItf = EHSem.gathDataGam_Syn_AGItf ehSem
                             , Core2GrSem.lamMp_Inh_CodeAGItf   = EHSem.gathLamMp_Syn_AGItf   ehSem
%%][50
                             { Core2GrSem.dataGam_Inh_CodeAGItf = EHSem.dataGam_Inh_AGItf     ehInh
                             , Core2GrSem.lamMp_Inh_CodeAGItf   = lm `lamMpUnionBindAspMp` Core2GrSem.lamMp_Inh_CodeAGItf coreInh      -- assumption: no duplicates, otherwise merging as done later has to be done
%%]]
                             }
%%[[50
                           lm       = EHSem.gathLamMp_Syn_AGItf      ehSem
%%]]
                       in  crsi -- From: cpFlowEHSem1
                             { _crsiCoreInh = coreInh' }
%%]]

               brefto bfun BRef_CRSI

%%[[99
          ExposedPackages -> brefto bfun BRef_ExposedPackages
%%]]

          EHCOptsOf modNm -> brefto bfun $ BRef_EHCOpts modNm

%%[[50
          ImportsOfName modSearchKey -> do
               ecu <- bcall $ EcuOfPrevNameAndPath modSearchKey
               (nm, imps, _) <- bcall $ ModnameAndImports modSearchKey (_ecuASTType ecu)
               breturn (nm, imps)

          ImportsRecursiveWithImps mbPrev imps -> do
               -- ecu     <- bcall $ EcuOfPrevNameAndPath modSearchKey
               recimps <- fmap Map.unions $ forM (Set.toList imps) $ \imp -> do
                 (nm', imps', recimps') <- bcall $ ImportsRecursiveOfName $ mkPrevFileSearchKeyWithNameMbPrev imp mbPrev
                 return $ Map.insert nm' imps' recimps'
               breturn (recimps)

          ImportsRecursiveOfName modSearchKey -> do
               ecu        <- bcall $ EcuOfPrevNameAndPath modSearchKey
               (nm, imps) <- bcall $ ImportsOfName modSearchKey
               recimps    <- bcall $ ImportsRecursiveWithImps (_ecuMbPrevSearchInfo ecu) imps
               breturn (nm, imps, recimps)
%%]]

          ActualModNm modNm -> (fmap ecuModNm $ bcall $ EcuOfName modNm) >>= breturn

          EcuOf modNm -> brefto bfun $ BRef_ECU modNm

          -- does not typecheck...
          -- EcuMbOf modNm -> brefto' bfun $ BRef_ECU modNm

          EcuOfName modNm -> do
               -- For interaction with old build system, in case of already existing ecu and no dependency on file, take that one
               maybeM' (fmap fst $ bderef'' $ BRef_ECU modNm) (\_ -> bcall $ EcuOf modNm) $
                 bcall $ EcuOfNameAndPath (modNm, ASTFileNameOverride_AsIs)
{-
               (mbEcu,_) <- bderef'' $ BRef_ECU modNm
               case mbEcu of
                 Just ecu -> bcall $ EcuOf modNm
                 _        -> bcall $ EcuOfNameAndPath (modNm, ASTFileNameOverride_AsIs)
-}
           
          EcuOfNameAndPath mf -> do
               bcall $ EcuOfPrevNameAndPath (mf,Nothing)
           
          EcuOfPrevNameAndPath modSearchKey@((modNm,overrFp),mbPrev) -> do
               opts <- bcall $ EHCOptsOf modNm
               let -- mbFp            = astFileNameOverrideToMaybe overrFp
                   -- isTopModule     = isJust mbFp
                   (mbFp,isTopModule) = case overrFp of
                     ASTFileNameOverride_AsIs          -> (Nothing, False)
                     ASTFileNameOverride_FPath      fp -> (Just fp, False)
                     ASTFileNameOverride_FPathAsTop fp -> (Just fp, True )
                   searchPath      = ehcOptImportFileLocPath opts
%%[[8
                   adaptFileSuffMp = id
%%][50
                   adaptFileSuffMp = if isTopModule then (fileSuffMpHsNoSuff ++) else id
%%]]
               fileSuffMpHs <- fmap (map tup123to12 . adaptFileSuffMp) $ getl $ crStateInfo ^* crsiFileSuffMp
%%[[8
               fpFound  <- cpFindFileForFPath fileSuffMpHs searchPath (Just modNm) mbFp
%%][50
               fpsFound <- cpFindFilesForFPath False fileSuffMpHs searchPath (Just modNm) mbFp
%%][99
               let searchPath' = prevSearchInfoAdaptedSearchPath mbPrev searchPath
               cpTr TraceOn_BuildSearchPaths ["FPathSearchForFile: " ++ show modNm, "sp1=" ++ show searchPath, "sp2=" ++ show searchPath', "prev=" ++ show mbPrev]
               fpsFound <- cpFindFilesForFPathInLocations (fileLocSearch opts) tup123to1 False fileSuffMpHs searchPath' (Just modNm) mbFp
%%]]
%%[[99
               cpTr TraceOn_BuildFPaths $ ["EcuOfPrevNameAndPath: " ++ show modSearchKey, "on searchpath: " ++ show searchPath', "suffices: " ++ show fileSuffMpHs] ++ [ "found: " ++ show (fpathToStr fp) | fp <- fpsFound ]
%%]]
%%[[50
               when isTopModule
                    (bUpdECU modNm (ecuSetIsTopMod True))
%%]]
               bmemo $ BRef_ECU modNm
               fmap (panicJust "EcuOfPrevNameAndPath") $ bLookupECU modNm
           
          FPathSearchForFile suff fn -> do
               let fp    = mkTopLevelFPath suff fn
                   modNm = mkHNm $ fpathBase fp
               cpTr TraceOn_BuildFPaths ["FPathSearchForFile: " ++ show modNm ++ ", " ++ fpathToStr fp]
               breturn (modNm, fp)
      
          FPathForAST modSearchKey@((modNmAsked,overr),_) asttype skey tkey -> do
               res@(fp, suffoverr, ecu) <- case overr of
                 ASTFileNameOverride_FPath fp -> (bcall $ EcuOf modNmAsked) >>= \ecu -> return (fp, ASTFileSuffOverride_AsIs, ecu)
                 _                            -> (bcall $ EcuOfPrevNameAndPath modSearchKey) >>= \ecu -> return (ecuSrcFilePath ecu, ASTFileSuffOverride_Suff skey, ecu)
               breturn res
      
          ASTFromFile modSearchKey@((modNm,_),_) chkTimeStamp asttype skey tkey -> do
               maybeM (bASTFromFileMb modSearchKey chkTimeStamp asttype skey tkey) (undefFor modNm) $ \(res,_) -> breturn res

          ASTRefFromFileMb modSearchKey chkTimeStamp asttype skey tkey -> do
               eithAST <- bcall $ ASTRefFromFileEither modSearchKey False chkTimeStamp asttype skey tkey
               case eithAST of
                 Left  _   -> return Nothing
                 Right res -> breturn $ Just res

          ASTRefFromFileEither modSearchKey@((modNmAsked,overr),_) yieldErr (AlwaysEq chkTimeStamp) asttype skey@(astfcont,_) tkey -> do
               (fp, suffoverr, ecu) <- bcall $ FPathForAST modSearchKey asttype skey tkey
                 
               let modNm = ecuModNm ecu
                   -- ref   =  -- :: BRef res -- ast
               
               case BRef_ASTFile modSearchKey asttype skey tkey of
                 -- use case instead of let to be able to also access the AST type via tyvar 'ast' (@20150803 ghc 7.8.3: let does not allow scoped tyvars to be introduced in this way)
                 (ref :: BRef ast) -> do
%%[[8
                   let mbtm = Just undefined
%%][50
                   mbtm <- bcall $ ModfTimeOfFile modSearchKey asttype skey tkey
%%]]
                   opts <- bcall $ EHCOptsOf modNm
                   let (mbhdlr :: Maybe (ASTHandler' ast)) = asthandlerLookup asttype
                       mkfp hdlr = asthdlrMkInputFPath hdlr opts ecu suffoverr modNm fp

                   (mbRes :: Maybe ast, mbset) <- bderef'' ref
               
                   case (mbRes, mbset, mbhdlr) of
                     (Just ast, _, _) -> ret ref ast
                 
                     (_, Just set, Just (astHdlr :: ASTHandler' ast)) | chkTimeStamp == ASTFileTimeHandleHow_Ignore || isJust mbtm {- | isJust mbi && isJust mbl -} -> case astfcont of
%%[[50
                          ASTFileContent_Binary -> do
                            cpMsg' modNm VerboseALot "Decoding" Nothing fpC
                            cpTr TraceOn_BuildFPaths ["ASTFromFile ASTFileContent_Binary: " ++ show modNm ++ ", fp=" ++ fpathToStr fp ++ " -> fpC=" ++ fpathToStr fpC]
                            mbx@(~(Just x)) <- liftIO $ _asthdlrGetSerializeFileIO astHdlr opts fpC
                            if isJust mbx
                              then do
                                cpTr TraceOn_BuildResult ["ASTFromFile Deserialize ok: " ++ show modNm]
                                let errs = _asthdlrPostInputCheck astHdlr opts ecu modNm fpC x
                                if null errs
                                  then do
                                    cpTr TraceOn_BuildRef ["ASTFromFile ASTFileContent_Binary: " ++ show modNm ++ ", ref=" ++ show ref]
                                    setret ref x
                                  else do
                                    cpTr TraceOn_BuildResult ["ASTFromFile ASTFileContent_Binary postcheck errors: " ++ show modNm]
                                    err'' ("Decode AST check " ++ _asthdlrName astHdlr) errs
                              else err "decoder"
%%]]
                          fc | fc `elem` [ASTFileContent_Text, ASTFileContent_LitText] -> do
                            let --
%%[[8
                                popts       = defaultEHParseOpts
%%][50
                                popts       = _astsuffinfoUpdParseOpts info defaultEHParseOpts
%%]]
                                sopts       = _asthdlrParseScanOpts astHdlr opts popts
                                description = "Parse (" ++ (if ScanUtils.scoLitmode sopts then "Literate " else "") ++ _asthdlrName astHdlr ++ " syntax) of module `" ++ show modNm ++ "`"
                                seterrs es  = err'' description es
                            cpTr TraceOn_BuildFPaths ["ASTFromFile " ++ show fc ++ ": " ++ show modNm ++ ", fp=" ++ fpathToStr fp ++ " -> fpC=" ++ fpathToStr fpC ++ "(fp == fpC: " ++ show (fp == fpC) ++ ") ehpoptsOkToStopAtErr=" ++ show (ehpoptsOkToStopAtErr popts) ]
                            case _asthdlrParser astHdlr opts popts of
                              Just (ASTParser p) -> do
                                (ast,errs) <- parseWithFPath sopts popts p fpC
                                if null errs || ehpoptsOkToStopAtErr popts
                                  then setret ref ast
                                  else seterrs errs
                              _ -> do
                                seterrs [strMsg $ "No parser for " ++ _asthdlrName astHdlr]

                          fc -> err $ "ast content handler " ++ show fc

                       where mbi@(~(Just info)) = astsuffixLookup skey $ _asthdlrSuffixRel astHdlr
                             mbl@(~(Just lens)) = Map.lookup tkey $ _astsuffinfoASTLensMp info
                             fpC                = mkfp astHdlr
                             err                = err' fpC (_asthdlrName astHdlr)
                             setret ref ast     = set ast >> ret ref ast

                     _ | isNothing mbhdlr               -> err1 "ast handler"
                       | isNothing mbset                -> err2 mbhdlr "ast setter"
                       | chkTimeStamp == ASTFileTimeHandleHow_AbsenceIsError && isNothing mbtm
                                                        -> err2 mbhdlr "file time info (probably non existent)"
                       | otherwise                      -> dflt'
                       where err1               = err' fp (show asttype)
                             err2 (Just h)      = err' (mkfp h) (_asthdlrName h)

            where -- dflt' :: EHCompilePhaseT m res
                  -- dflt' = undefFor modNmAsked
                  dflt' = return $ Left ("",[])
                  err' fp k m = err'' ("Decode " ++ k ++ " for file " ++ fpathToStr fp) [strMsg $ "No " ++ m ++ " for " ++ k ++ " (" ++ show skey ++ "/" ++ show tkey ++ ")"]
                  err'' | yieldErr  = \desc es -> return $ Left (desc,es)
                        | otherwise = \desc es -> cpSetLimitErrsWhen 5 desc es >> dflt'
                  ret ref ast = let r = Right ref in bmemo' r >> return r

          AST modSearchKey@((modNm,_),_) asttypeAsked -> do
               maybeM (bcall $ ASTMb modSearchKey asttypeAsked) (undefFor modNm) $ \(res,_) -> breturn res

          ASTP modSearchKey@((modNm,_),_) astpipe -> do
               maybeM (bcall $ ASTPMb modSearchKey astpipe) (undefFor modNm) $ \(ASTResult {_astresAST=res}) -> breturn res

          ASTMb modSearchKey@((modNmAsked,_),_) asttypeAsked -> do
               -- the source file available to produce the asked AST, not necessarily used
               ecu <- bcall $ EcuOfPrevNameAndPath modSearchKey

%%[[8
               let modNm = modNmAsked
                   modSearchKey' = modSearchKey
%%][50
               (modNm, _, _) <- bcall $ ModnameAndImports modSearchKey (_ecuASTType ecu)
               let modSearchKey' = updPrevFileSearchKeyWithName modNm modSearchKey
%%]]
               -- (mbRes, mbset) <- bderef'' refAsked

               let -- set and return ast
                   retast :: Typeable xxx => xxx -> EHCompilePhaseT m res
                   retast ast = case cast ast of
                     Just ast -> do
                        (_, mbset) <- bderef'' ref
                        case mbset of
                          Just set -> set ast >> breturn (Just (ast, ref))
                          _        -> breturn Nothing
                       where ref = BRef_AST modSearchKey' asttypeAsked
                     _ -> breturn Nothing

               opts <- bcall $ EHCOptsOf modNm
               
               case asttypeAsked of
%%[[50
                 ASTType_HI -> do
                   let hii0 = HI.emptyHIInfo
                   hii1 <- maybeM (bGetHsSemMb modNm) (return hii0) $ \hsSem -> return $ hii0
                         {
                           -- cpFlowHsSem1
                           HI.hiiFixityGam            = HSSem.gathFixityGam_Syn_AGItf hsSem
                         , HI.hiiHIDeclImpModS        = ecuHIDeclImpNmS ecu                     -- TBD: sort out, should not be from hi
                         }
                   hii2 <- maybeM (bcall $ FoldEHMb (mkPrevFileSearchKeyWithName modNm)) (return hii1) $ \ehSem -> return $ hii1
%%[[(50 hmtyinfer)
                         {
                           -- cpFlowEHSem1
                           HI.hiiDataGam       = EHSem.gathDataGam_Syn_AGItf    ehSem
                         , HI.hiiValGam        = EHSem.gathValGam_Syn_AGItf     ehSem
                         , HI.hiiTyGam         = EHSem.gathTyGam_Syn_AGItf      ehSem
                         , HI.hiiTyKiGam       = EHSem.gathTyKiGam_Syn_AGItf    ehSem
                         , HI.hiiPolGam        = EHSem.gathPolGam_Syn_AGItf     ehSem
                         , HI.hiiClGam         = EHSem.gathClGam_Syn_AGItf      ehSem
                         , HI.hiiClDfGam       = EHSem.gathClDfGam_Syn_AGItf    ehSem
                         , HI.hiiCHRStore      = EHSem.gathChrStore_Syn_AGItf   ehSem
                         -- , HI.hiiLamMp         = lm
%%[[(50 codegen hmtyinfer)
                         , HI.hiiMbOrphan      = EHSem.mbOrphan_Syn_AGItf ehSem
%%]]
                         -- , HI.hiiHIUsedImpModS = usedImpS        -- TBD: sort out
                         }
%%]]
                   retast hii2
%%]]
                 _ ->
                   if asttypeAsked == _ecuASTType ecu
                     then do
                       -- if the available and asked for AST are of the same type, get the file contents
                       bASTFromFileMb modSearchKey' (AlwaysEq ASTFileTimeHandleHow_AbsenceIsError) asttypeAsked (_ecuASTFileContent ecu, _ecuASTFileUse ecu) ASTFileTiming_Current
                     else
                       -- case analysis on what is asked for and what is given
                       case asttypeAsked of
                         ASTType_EH -> 
                           case _ecuASTType ecu of
                             ASTType_HS ->
                               -- From: cpTranslateHs2EH
                               maybeM' (bGetHsSemMb modNm)
                                       (retast . HSSem.eh_Syn_AGItf) dflt
                             _ -> dflt

%%[[(8 core)
                         ASTType_Core -> 
                           -- From: cpTranslateEH2Core
                           maybeM' (bcall $ FoldEHMb (mkPrevFileSearchKeyWithName modNm))
                                   (retast . cmodTrfElimNonCodegenConstructs opts . EHSem.cmodule_Syn_AGItf) $
                           -- try out other ways of extracting until we fail...
                           dflt
%%]]

                         _ -> dflt

             where       
               dflt = return Nothing

          ASTPMb modSearchKey@((modNmAsked,_),_) astpipe -> do
               let asttypeAsked = astpType astpipe
               -- the source file available to produce the asked AST, not necessarily used
               ecu <- bcall $ EcuOfPrevNameAndPath modSearchKey

%%[[8
               let modNm = modNmAsked
                   modSearchKey' = modSearchKey
%%][50
               (modNm, _, _) <- bcall $ ModnameAndImports modSearchKey (_ecuASTType ecu)
               let modSearchKey' = updPrevFileSearchKeyWithName modNm modSearchKey
%%]]

               let -- set and return ast
                   retast :: Typeable xxx => ASTPipe -> xxx -> EHCompilePhaseT m res
                   retast astpipe ast = case cast ast of
                     Just ast -> do
                        (_, mbset) <- bderef'' ref
                        case mbset of
                          Just set -> set ast >> mkASTResult ast ref astpipe >>= (breturn . Just)
                          _        -> breturn Nothing
                       where ref = BRef_AST modSearchKey' asttypeAsked
                     _ -> breturn Nothing

               opts <- bcall $ EHCOptsOf modNm
               
               res <- case astpipe of
%%[[50
                 ASTPipe_Derived ASTType_HI _ -> do
                     let hii0 = HI.emptyHIInfo
                     hii1 <- maybeM (bGetHsSemMb modNm) (return hii0) $ \hsSem -> return $ hii0
                           {
                           -- cpFlowHsSem1
                           HI.hiiFixityGam            = HSSem.gathFixityGam_Syn_AGItf hsSem
                           , HI.hiiHIDeclImpModS        = ecuHIDeclImpNmS ecu                       -- TBD: sort out, should not be from hi
                           }
                     hii2 <- maybeM (bcall $ FoldEHMb (mkPrevFileSearchKeyWithName modNm)) (return hii1) $ \ehSem -> return $ hii1
%%[[(50 hmtyinfer)  
                           {
                           -- cpFlowEHSem1
                             HI.hiiDataGam       = EHSem.gathDataGam_Syn_AGItf    ehSem
                           , HI.hiiValGam        = EHSem.gathValGam_Syn_AGItf     ehSem
                           , HI.hiiTyGam         = EHSem.gathTyGam_Syn_AGItf      ehSem
                           , HI.hiiTyKiGam       = EHSem.gathTyKiGam_Syn_AGItf    ehSem
                           , HI.hiiPolGam        = EHSem.gathPolGam_Syn_AGItf     ehSem
                           , HI.hiiClGam         = EHSem.gathClGam_Syn_AGItf      ehSem
                           , HI.hiiClDfGam       = EHSem.gathClDfGam_Syn_AGItf    ehSem
                           , HI.hiiCHRStore      = EHSem.gathChrStore_Syn_AGItf   ehSem
                           -- , HI.hiiLamMp         = lm
%%[[(50 codegen hmtyinfer)
                           , HI.hiiMbOrphan      = EHSem.mbOrphan_Syn_AGItf ehSem
%%]]  
                           -- , HI.hiiHIUsedImpModS = usedImpS      -- TBD: sort out
                           }
%%]]
%%[[(50 core grin)
                     hii3 <- maybeM (bcall $ FoldCore2GrinMb (mkPrevFileSearchKeyWithName modNm)) (return hii2) $ \coreSem -> return $ hii2
                           { -- cpFlowCoreSemAfterFold
                             HI.hiiLamMp         = Core2GrSem.gathLamMp_Syn_CodeAGItf coreSem
                           }
%%][50  
                     let hii3 = hii2
%%]]  
                     retast astpipe hii2
%%]]
                 ASTPipe_Src asttypeAsked
                   | asttypeAsked == _ecuASTType ecu -> do
                     bASTFromFileEither modSearchKey' False (AlwaysEq ASTFileTimeHandleHow_AbsenceIsError) asttypeAsked (_ecuASTFileContent ecu, _ecuASTFileUse ecu) ASTFileTiming_Current >>=
                       -- 20150818 TBD: factor below out
                       either (\_ -> return Nothing)
                              (\(a,r,t) -> fmap Just $ mkASTResult' a r astpipe (Just t))

%%[[50
                 ASTPipe_Cached asttypeAsked -> do
                     bASTFromFileEither modSearchKey' True (AlwaysEq ASTFileTimeHandleHow_AbsenceIgnore) asttypeAsked (ASTFileContent_Binary, ASTFileUse_Cache) ASTFileTiming_Prev >>=
                       -- 20150818 TBD: factor below out
                       either (\_ -> return Nothing)
                              (\(a,r,t) -> fmap Just $ mkASTResult' a r astpipe (Just t))

                 ASTPipe_Cache asttypeAsked astpipe' -> do
                     -- Cache/write AST
                     maybeM (bcall $ ASTPMb modSearchKey' astpipe') dflt $ \res -> do
                       -- TBD: write/output side effect
                       breturn $ Just $ astresPipe ^= astpipe' $ res -- (ast, ref, astpipe')

                 ASTPipe_FirstNewestAvailable asttypeAsked astpipe1 astpipe2 -> do
                     tmChoose dflt (tmOf modSearchKey' astpipe1) (tmOf modSearchKey' astpipe2)
                       (\_ -> bcall $ ASTPMb modSearchKey' astpipe1)
                       (\_ -> bcall $ ASTPMb modSearchKey' astpipe2)
                   where
                     -- source: time of src itself + imports
                     tmOf modSearchKey p@(ASTPipe_Src {astpType=t}) = do
                         mbTm <- bcall $ ModfTimeOfFile modSearchKey t (_ecuASTFileContent ecu, _ecuASTFileUse ecu) ASTFileTiming_Current
                         case mbTm of
                           Just tm -> do
                             (_, imps, _) <- bcall $ ModnameAndImports modSearchKey t
                             imptms <- forM (Set.toList imps) $ \n -> tmOf (mkPrevFileSearchKeyWithName n) p >>= \mbt -> return (n, fmap snd mbt)
                             if all (isJust . snd) imptms
                               then return $ Just (Map.fromList [(n,t) | (n, Just t) <- imptms], tm)
                               else dflt
                           _ -> dflt
                       where dflt = return Nothing

                     -- cached: just time of cached file itself, if file is valid
                     tmOf modSearchKey (ASTPipe_Cached {astpType=t}) = ifM' (bcall $ ASTFileIsValid modSearchKey t (ASTFileContent_Binary, ASTFileUse_Cache) ASTFileTiming_Prev) (return Nothing) $ do
                         tm <- bcall $ ModfTimeOfFile modSearchKey t (ASTFileContent_Binary, ASTFileUse_Cache) ASTFileTiming_Prev
                         return $ fmap ((,) Map.empty) tm

                     -- cache: time recursively computed, if file can be cached
                     tmOf modSearchKey (ASTPipe_Cache {astpPipe=p}) = ifM' (bcall $ DirOfModIsWriteable modNm) (return Nothing) $ tmOf modSearchKey p

                     -- derived: time recursively computed
                     tmOf modSearchKey (ASTPipe_Derived {astpPipe=p}) = tmOf modSearchKey p

                     -- transformation (also derived): time recursively computed
                     tmOf modSearchKey (ASTPipe_Trf {astpPipe=p}) = tmOf modSearchKey p

                     -- whole: time recursively computed, merged into 1 time for all
                     tmOf modSearchKey (ASTPipe_Whole {astpPipe=p}) = fmap (fmap (\(imps,tm) -> (Map.empty, maximum $ tm : Map.elems imps))) $ tmOf modSearchKey p

                     --
                     tmOf modSearchKey (ASTPipe_FirstNewestAvailable {astpPipe1=p1, astpPipe2=p2}) = tmChoose dflt (tmOf modSearchKey p1) (tmOf modSearchKey p2) (return . Just) (return . Just)

                     -- compound: maximum of all
                     tmOf modSearchKey (ASTPipe_Compound {astpPipes=ps}) = do 
                         tms <- mapM (tmOf modSearchKey) ps
                         return $ if null tms && not (all isJust tms)
                           then Nothing
                           else let (tsimps, ts) = unzip $ map fromJust tms
                                in  Just (Map.unions tsimps, maximum ts)

                     -- others
                     tmOf _ _ = return Nothing

                     tmChoose adflt tm1 tm2 a1 a2 = tm1 >>= \mbtm1 -> tm2 >>= \mbtm2 -> case (mbtm1, mbtm2) of
                       (Just t1			, Nothing			) -> a1 t1
                       (Nothing			, Just t2			) -> a2 t2
                       (Just t1@(_,tm1)	, Just t2@(ti2,tm2)	) -> if tm1 `diffClockTimes` mxtm2 > noTimeDiff then a1 t1 else a2 t2
                         where mxtm2 = maximum $ tm2 : Map.elems ti2
                       _                 		 			  -> adflt
%%]]

                 -- ASTPipe_Derived ASTType_EH _ -> do
                 p | isJust $ astpMbFromHSToEH True p -> do
                     -- From: cpTranslateHs2EH
                     maybeM (bGetHsSemPMb modNm astpipe) dflt
                            (retast astpipe . HSSem.eh_Syn_AGItf)

%%[[(8 core)
                 -- ASTPipe_Derived ASTType_Core _ -> do
                 p | isJust $ astpMbFromEHToCore True p -> do
                     -- From: cpTranslateEH2Core
                     maybeM (bcall $ FoldEHPMb (mkPrevFileSearchKeyWithName modNm) astpipe) dflt
                            (retast astpipe . cmodTrfElimNonCodegenConstructs opts . EHSem.cmodule_Syn_AGItf)
%%]]
%%[[(8 core grin)
                 -- ASTPipe_Derived ASTType_Grin _ -> do
                 p | isJust $ astpMbFromCoreToGrin True p -> do
                     -- From: cpTranslateCore2Grin
                     maybeM (bcall $ FoldCore2GrinPMb (mkPrevFileSearchKeyWithName modNm) astpipe) dflt
                            (retast astpipe . Core2GrSem.grMod_Syn_CodeAGItf)
%%]]
%%[[(8 core corerun)
                 -- ASTPipe_Derived ASTType_CoreRun _ -> do
                 p | isJust $ astpMbFromCoreToCoreRun True p -> do
                     -- From: cpFoldCore2CoreRun
                     maybeM (bcall $ FoldCore2CoreRunPMb (mkPrevFileSearchKeyWithName modNm) astpipe) dflt
                            (retast astpipe . Core2CoreRunSem.crm_Syn_CodeAGItf)
%%]]

                 _ -> dflt

               cpTr TraceOn_BuildResult ["ASTPMb ok?=" ++ show (isJust res)]
               return res

             where       
               dflt = return Nothing

%%[[50
          ModfTimeOfFile modSearchKey@((modNm,_),_) asttype skey tkey -> do
               (fp, suffoverr, ecu) <- bcall $ FPathForAST modSearchKey asttype skey tkey
               opts <- bcall $ EHCOptsOf modNm
               case (asthandlerLookup' asttype $ \hdlr -> do
                         suffinfo <- astsuffixLookup skey $ _asthdlrSuffixRel hdlr
                         let mblens = Map.lookup tkey $ _astsuffinfoModfTimeMp suffinfo
                         return (mblens, asthdlrMkInputFPath hdlr opts ecu suffoverr modNm fp)
                    ) of
                         Just (mblens, fp) -> do
                           r <- tm opts ecu (maybe (const id) (\lens -> (lens ^=) . Just) mblens) fp
                           cpTr TraceOn_BuildTimes ["ModfTimeOfFile " ++ show modSearchKey, "asttype: " ++ show asttype ++ ", skey: " ++ show skey ++ ", tkey: " ++ show tkey, "fp: " ++ fpathToStr fp, "mb time: " ++ show r]
                           return r
                         _                 ->
                           return Nothing
            where
              tm opts ecu store fp = do
                  let n = fpathToStr fp
                  nExists <- liftIO $ doesFileExist n
                  when (ehcOptVerbosity opts >= VerboseDebug) $ liftIO $ putStrLn ("meta info of: " ++ show (ecuModNm ecu) ++ ", file: " ++ n ++ ", exists: " ++ show nExists)
                  if nExists 
                    then do
                      t <- liftIO $ fpathGetModificationTime fp
                      when (ehcOptVerbosity opts >= VerboseDebug) $ liftIO $ putStrLn ("time stamp of: " ++ show (ecuModNm ecu) ++ ", time: " ++ show t)
                      bUpdECU modNm $ store t
                      breturn $ Just t
                    else
                      return Nothing

          ASTFileIsValid modSearchKey asttype skey tkey -> do
               mbValid <- asthandlerLookupM' asttype $ \hdlr -> do
                   eith <- bASTFromFileEither modSearchKey False (AlwaysEq ASTFileTimeHandleHow_AbsenceIgnore) asttype skey tkey
                   case eith of
                     Left _ -> return Nothing
                     Right (ast, _, _) -> return $ Just $ _asthdlrASTIsValid hdlr ast
               maybe (return False) breturn mbValid

          DirOfModIsWriteable modNm -> do
               ecu <- bcall $ EcuOfName modNm
               let fp = ecuSrcFilePath ecu
               pm <- liftIO $ getPermissions (maybe "." id $ fpathMbDir fp)
               let res = writable pm
               -- liftIO $ putStrLn (fpathToStr fp ++ " writ " ++ show res)
               bUpdECU modNm $ ecuDirIsWritable ^= res
               breturn res

          CanCompile modNm -> do
               ecu  <- bcall $ EcuOfName modNm
               isWr <- bcall $ DirOfModIsWriteable modNm
               mbTm <- bcall $ ModfTimeOfFile (mkPrevFileSearchKeyWithName modNm) (ecu ^. ecuASTType) (ecu ^. ecuASTFileContent, ecu ^. ecuASTFileUse) ASTFileTiming_Current
               breturn $ isJust mbTm && isWr
                     
          NeedsCompile modNm -> do
               -- From: crModNeedsCompile
               opts <- bcall $ EHCOptsOf modNm
               ecu  <- bcall $ EcuOfName modNm
               return $
                 ecuIsMainMod ecu -- ecuIsTopMod ecu
                 || not (  ehcOptCheckRecompile opts
                        -- 20150817 TBD
                        -- && ecuCanUseHIInsteadOfHS ecu
                        -- && null newer
                        )

          ASTFileIsNewerThan (modSearchKey1, asttype1, skey1, tkey1) (modSearchKey2, asttype2, skey2, tkey2) -> do
               mbTm1 <- bcall $ ModfTimeOfFile modSearchKey1 asttype1 skey1 tkey1
               mbTm2 <- bcall $ ModfTimeOfFile modSearchKey2 asttype2 skey2 tkey2
               case (mbTm1, mbTm2) of
                 (Just t1, Just t2) -> breturn $ Just $ t1 `diffClockTimes` t2 > noTimeDiff
                 _                  -> return Nothing

{-
crModNeedsCompile :: HsName -> EHCompileRun -> Bool
crModNeedsCompile modNm cr
  = ecuIsMainMod ecu -- ecuIsTopMod ecu
    || not (  ehcOptCheckRecompile opts
           && ecuCanUseHIInsteadOfHS ecu
           && null newer
           )
  where ecu = crCU modNm cr
        (newer,_) = crPartitionNewerOlderImports modNm cr
        opts = _crStateInfo cr ^. crsiOpts

crPartitionNewerOlderImports :: HsName -> EHCompileRun -> ([EHCompileUnit],[EHCompileUnit])
crPartitionNewerOlderImports modNm cr
  = partition isNewer $ map (flip crCU cr) $ ecuImpNmL ecu
  where ecu = crCU modNm cr
        t   = panicJust "crPartitionNewerOlderImports1" $ _ecuMbHIInfoTime ecu
        isNewer ecu'
            | isJust mbt = t' `diffClockTimes` t > noTimeDiff
            | otherwise  = False
            where t' = panicJust "crPartitionNewerOlderImports2" $ _ecuMbHIInfoTime ecu'
                  mbt = _ecuMbHIInfoTime ecu'

-- | Can HI be used instead of HS?
--   This is purely based on HI being of the right version and HS not newer.
--   The need for recompilation considers dependencies on imports as well.
ecuCanUseHIInsteadOfHS :: EHCompileUnit -> Bool
ecuCanUseHIInsteadOfHS ecu
  = ecuIsValidHIInfo ecu && not (ecuIsHSNewerThanHI ecu)

ecuIsValidHIInfo :: EHCompileUnit -> Bool
ecuIsValidHIInfo ecu
  = case _ecuMbPrevHIInfo ecu of
      Just i -> HI.hiiValidity i == HI.HIValidity_Ok
      _      -> False

-- | Is HS newer?
--   If no HS exists False is returned.
ecuIsHSNewerThanHI :: EHCompileUnit -> Bool
ecuIsHSNewerThanHI ecu
  = case (_ecuMbSrcTime ecu,_ecuMbHIInfoTime ecu) of
      (Just ths,Just thi) -> ths `diffClockTimes` thi > noTimeDiff 
      (Nothing ,Just thi) -> False
      _                   -> True
-}

          IsTopMod modNm -> do
               ecu  <- bcall $ EcuOfName modNm
               breturn $ ecuIsTopMod ecu
                     
%%]]

%%[[50
          FoldHsMod modSearchKey@((modNm,overr),mbPrev) mbPkgKeyDirLForCPP@(~(Just pkgKeyDirL)) -> do
%%[[99
               let doCPP = isJust mbPkgKeyDirLForCPP
%%]]
               overr' <-
%%[[99
                 if doCPP
                   then fmap ASTFileNameOverride_FPath $ bcall $ FPathPreprocessedWithCPP pkgKeyDirL modSearchKey
                   else
%%]]
                        return ASTFileNameOverride_AsIs
               let modSearchKey' = ((modNm,overr'),mbPrev)
               ecu  <- bcall $ EcuOfPrevNameAndPath modSearchKey'
               let modNm'         = ecuModNm ecu
                   modSearchKey'' = ((modNm',overr'),mbPrev)
               hs   <- bcall $ ASTFromFile modSearchKey'' (AlwaysEq ASTFileTimeHandleHow_AbsenceIsError) ASTType_HS (_ecuASTFileContent ecu, ASTFileUse_SrcImport) ASTFileTiming_Current
               crsi <- bcall $ CRSI
               opts <- bcall $ EHCOptsOf modNm'
               let  inh        = crsiHSModInh crsi
                    hsSemMod   = HSSemMod.wrap_AGItf (HSSemMod.sem_AGItf hs)
                                                     (inh { HSSemMod.gUniq_Inh_AGItf        = crsi ^. crsiHereUID
                                                          , HSSemMod.moduleNm_Inh_AGItf     = modNm'
                                                          })
                    hasMain= HSSemMod.mainValExists_Syn_AGItf hsSemMod
%%[[99
                    pragmas = HSSemMod.fileHeaderPragmas_Syn_AGItf hsSemMod
                    (ecuOpts,modifiedOpts)
                            = ehcOptUpdateWithPragmas pragmas opts
%%]]
               bUpdECU modNm' ( ecuStoreHSSemMod hsSemMod
                              . ecuSetHasMain hasMain
%%[[99
                              . ecuStorePragmas pragmas
                              . (if modifiedOpts then ecuStoreOpts ecuOpts else id)
%%]]
                              )
               breturn
                 ( hsSemMod
                 , hasMain
%%[[99
                 , pragmas
                 , if modifiedOpts then Just ecuOpts else Nothing
%%]]
                 )

          ModnameAndImports modSearchKey@((modNm,_),mbPrev) asttype -> do
               -- ecu <- bcall $ EcuOfPrevNameAndPath modSearchKey
               case asttype of
                 ASTType_HS -> bcall $ HsModnameAndImports modSearchKey
                 ASTType_EH -> return (modNm, Set.empty, mbPrev)
                 ASTType_HI -> do
                   (_, impNmS, _, _) <- bcall $ FoldHIInfo modSearchKey
                   breturn (modNm, impNmS, mbPrev)
%%[[(50 corein)
                 ASTType_Core -> do
                   (_, modNm', impNmS, _, _, newPrev) <- bcall $ FoldCoreMod modSearchKey
                   breturn (modNm', impNmS, newPrev)
%%]]
%%[[(50 corerunin)
                 ASTType_CoreRun -> do
                   (_, modNm', impNmS, _, _, newPrev) <- bcall $ FoldCoreRunMod modSearchKey
                   breturn (modNm', impNmS, newPrev)
%%]]
                 _ -> do 
                   cpSetLimitErrsWhen 1 "Imports" [rngLift emptyRange Err_Str $ "Cannot extract module name and imports from " ++ show modNm ++ " (" ++ show asttype ++ ")" ]
                   -- should not arrive at usage of this result
                   breturn $ panic $ "BuildFunction.Run.bcall ModnameAndImports: " ++ show modNm

          HsModnameAndImports modSearchKey@((modNm,_),_) -> do
%%[[50
               let mbPkgKeyDirL = Nothing
%%][99
               opts <- bcall $ EHCOptsOf modNm
               pkgKeyDirL <- bcall ExposedPackages
               let doCPP                = ehcOptCPP opts
                   mkMbPkgKeyDirL doCPP = if doCPP then Just pkgKeyDirL else Nothing
                   mbPkgKeyDirL         = mkMbPkgKeyDirL doCPP
%%]]
               cpStepUID
               
               resFold1@( _, _
%%[[99
                 , pragmas, mbOpts
%%]]
                 ) <- bcall $ FoldHsMod modSearchKey mbPkgKeyDirL

%%[[50
               let resFold2@( hsSemMod, _ ) = resFold1
%%][99
               let opts2 = maybe opts id mbOpts
               resFold2@( hsSemMod, _, _, _ ) <-
                 if (  not (ehcOptCPP opts2)                -- reinvoke if CPP has not been invoked before
                    || ehcOptCmdLineOptsDoneViaPragma opts2 -- or options have been set via pragma
                    )
                    -- check whether the pragma has a cmdline option like effect
                    && (not $ null $ filter pragmaInvolvesCmdLine $ Set.toList pragmas)
                  then do
                    let doCPP2 = doCPP || Set.member Pragma_CPP pragmas
                    bcall $ FoldHsMod modSearchKey $ mkMbPkgKeyDirL doCPP2
                  else return resFold1
%%]]
               let modNm'     = HSSemMod.realModuleNm_Syn_AGItf hsSemMod
                   impNmS     = HSSemMod.modImpNmS_Syn_AGItf hsSemMod
               (modNmNew, newPrev) <- newModNm modNm modNm' $ ecuStoreSrcDeclImpS impNmS
               return (modNmNew, impNmS, newPrev)

          FoldHIInfo modSearchKey@((modNm,_),_) -> do
               hiInfo <- bcall $ ASTFromFile modSearchKey (AlwaysEq ASTFileTimeHandleHow_AbsenceIgnore) ASTType_HI (ASTFileContent_Binary, ASTFileUse_Cache) ASTFileTiming_Prev
               opts   <- bcall $ EHCOptsOf modNm
               crsi   <- bcall $ CRSI
               let hasMain= HI.hiiHasMain hiInfo
                   mm     = crsiModMp crsi
                   mmi    = Map.findWithDefault emptyModMpInfo modNm mm
                   mmi'   = mkModMpInfo modNm
                                        (mmiInscps mmi)
                                        ( -- (\v -> tr "FoldHIInfo.hiiExps" (pp v) v) $
                                         HI.hiiExps hiInfo)
                                        (HI.hiiHiddenExps hiInfo)
               -- when hasMain (crSetAndCheckMain modNm)
               cpUpdSI (\crsi -> crsi {crsiModMp = Map.insert modNm mmi' mm})       -- this should be made explicit
               bUpdECU modNm ( ecuStoreHIDeclImpS (HI.hiiHIDeclImpModS hiInfo)
                             . ecuStoreHIUsedImpS (HI.hiiHIUsedImpModS hiInfo)
                             . ecuSetHasMain hasMain
                             )
               when (ehcOptVerbosity opts >= VerboseDebug)
                    (liftIO $ putStrLn
                       (show modNm
                        ++ ": hi imps, decl=" ++ show (HI.hiiHIDeclImpModS hiInfo)
                        ++ ", used=" ++ show (HI.hiiHIUsedImpModS hiInfo)
                    )  )
               breturn
                 ( hiInfo
                 , HI.hiiHIDeclImpModS hiInfo
                 , HI.hiiHIUsedImpModS hiInfo
                 , hasMain
                 )
               
          ImportNameInfo modSearchKey@((modNm,_),_) optimScope -> do
               ecu <- bcall $ EcuOfPrevNameAndPath modSearchKey
               let isWholeProg = optimScope > OptimizationScope_PerModule
                   impNmL     | isWholeProg = []
                              | otherwise   = ecuImpNmL ecu
               return impNmL
  
               
          ImportExportImpl modSearchKey@((modNm,_),_) optimScope -> do
               ecu    <- bcall $ EcuOfPrevNameAndPath modSearchKey
               opts   <- bcall $ EHCOptsOf modNm
               crsi   <- bcall $ CRSI
               impNmL <- bcall $ ImportNameInfo modSearchKey optimScope
               let isWholeProg = optimScope > OptimizationScope_PerModule
                   expNmFldMp | ecuIsMainMod ecu = Map.empty
                              | otherwise        = crsiExpNmOffMp modNm crsi
                   modOffMp   | isWholeProg = Map.filterWithKey (\n _ -> n == modNm) $ crsiModOffMp crsi
                              | otherwise   = crsiModOffMp crsi
               return $ emptyModuleImportExportImpl
                 { mieimplLamMp             = Core2GrSem.lamMp_Inh_CodeAGItf $ _crsiCoreInh crsi
                 , mieimplUsedModNmL        = if ecuIsMainMod ecu then [ m | (m,_) <- sortOnLazy snd $ Map.toList $ Map.map fst modOffMp ] else []
                 , mieimplHsName2FldMpMp    = Map.fromList
                     [ (n,(o,mp))
                     | (n,o) <- refGen 0 1 impNmL
                     , let (_,mp) = panicJust ("cpGenModuleImportExportImpl: " ++ show n) (Map.lookup n (crsiModOffMp crsi))
                     ]
                 , mieimplHsName2FldMp  = expNmFldMp
                 }
  
  
%%]]

          FoldHs modSearchKey@((modNm,_),_) astpipe -> do
               -- maybeM (bcall $ FoldHsPMb modSearchKey astpipe) (undefFor modNm) breturn
               maybeM (bcall $ FoldHsMb modSearchKey) (undefFor modNm) breturn
               
          FoldHsMb modSearchKey@((modNm,_),_) -> do
               -- hs       <- bcall $ ASTFromFile modSearchKey (AlwaysEq ASTFileTimeHandleHow_AbsenceIsError) ASTType_HS (_ecuASTFileContent ecu, ASTFileUse_Src) ASTFileTiming_Current
               maybeM (bcall $ ASTMb modSearchKey ASTType_HS) (return Nothing) $ \(hs, _) -> do
               ecu      <- bcall $ EcuOfPrevNameAndPath modSearchKey
               opts     <- bcall $ EHCOptsOf modNm
               crsi     <- bcall $ CRSIOfName modSearchKey ASTType_HS
%%[[50
               isTopMod <- bcall $ IsTopMod modNm
%%]]
               let  inh    = crsi ^. crsiHSInh
                    hsSem  = HSSem.wrap_AGItf (HSSem.sem_AGItf hs)
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
                           where mmi    = panicJust "FoldHs.crsiModMp" $ Map.lookup modNm $ crsiModMp crsi
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
               cpSetLimitErrsWhen 5 "Dependency/name analysis" $ Seq.toList $ HSSem.errSq_Syn_AGItf hsSem
               bUpdECU modNm ( ecuStoreHSSem hsSem
%%[[50
                             -- TBD: should move elsewhere
                             . ecuStoreHIDeclImpS ( -- (\v -> tr "YY" (pp $ Set.toList v) v) $
                                                   ecuSrcDeclImpNmS ecu)
                             -- . ecuSetHasMain hasMain
%%]]
                             )
               when (ehcOptEmitHS opts)
                    (liftIO $ putPPFPath (mkOutputFPath opts modNm (ecuFilePath ecu) "hs2") (HSSem.pp_Syn_AGItf hsSem) 1000)
               when (ehcOptShowHS opts)
                    (liftIO $ putWidthPPLn 120 (HSSem.pp_Syn_AGItf hsSem))
%%[[50
               when (ehcOptVerbosity opts >= VerboseDebug)
                    (liftIO $ putStrLn (show modNm ++ " hasMain=" ++ show hasMain))
               -- when hasMain (crSetAndCheckMain modNm)
%%]]

               breturn $ Just
                 ( hsSem
%%[[50
                 -- , ecuSrcDeclImpNmS ecu
                 , hasMain
%%]]
                 )
               
          FoldHsPMb modSearchKey@((modNm,_),_) astpipe -> do
               cpTrPP TraceOn_BuildFold $ ["FoldHsPMb" >#< modNm, pp astpipe]
               -- maybeM (bcall $ ASTPMb modSearchKey astpipe) (return Nothing) $ \(hs, _, astpipe) -> do
               maybe2M (return $ astpMbFromHSToEH False astpipe)
                       (\(p,_) -> bcall $ ASTPMb modSearchKey p) (return Nothing) $ \(ASTResult {_astresAST=hs}) -> do
               cpTr TraceOn_BuildFold $ ["FoldHsPMb ok"]
               ecu      <- bcall $ EcuOfPrevNameAndPath modSearchKey
               opts     <- bcall $ EHCOptsOf modNm
               crsi     <- bcall $ CRSIOfNameP modSearchKey astpipe
%%[[50
               isTopMod <- bcall $ IsTopMod modNm
%%]]
               let  inh    = crsi ^. crsiHSInh
                    hsSem  = HSSem.wrap_AGItf (HSSem.sem_AGItf hs)
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
                           where mmi    = panicJust "FoldHs.crsiModMp" $ Map.lookup modNm $ crsiModMp crsi
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
               cpSetLimitErrsWhen 5 "Dependency/name analysis" $ Seq.toList $ HSSem.errSq_Syn_AGItf hsSem
               bUpdECU modNm ( ecuStoreHSSem hsSem
%%[[50
                             -- TBD: should move elsewhere
                             . ecuStoreHIDeclImpS ( -- (\v -> tr "YY" (pp $ Set.toList v) v) $
                                                   ecuSrcDeclImpNmS ecu)
                             -- . ecuSetHasMain hasMain
%%]]
                             )
               when (ehcOptEmitHS opts)
                    (liftIO $ putPPFPath (mkOutputFPath opts modNm (ecuFilePath ecu) "hs2") (HSSem.pp_Syn_AGItf hsSem) 1000)
               when (ehcOptShowHS opts)
                    (liftIO $ putWidthPPLn 120 (HSSem.pp_Syn_AGItf hsSem))
%%[[50
               when (ehcOptVerbosity opts >= VerboseDebug)
                    (liftIO $ putStrLn (show modNm ++ " hasMain=" ++ show hasMain))
               -- when hasMain (crSetAndCheckMain modNm)
%%]]

               breturn $ Just
                 ( hsSem
%%[[50
                 -- , ecuSrcDeclImpNmS ecu
                 , hasMain
%%]]
                 )
               
          FoldEH modSearchKey@((modNm,_),_) -> do
               maybeM (bcall $ FoldEHMb modSearchKey) (undefFor modNm) breturn
               
          FoldEHMb modSearchKey@((modNm,_),_) -> do
               -- eh   <- bcall $ ASTFromFile modSearchKey (AlwaysEq ASTFileTimeHandleHow_AbsenceIsError) ASTType_EH (ASTFileContent_Text, ASTFileUse_Src) ASTFileTiming_Current
               maybeM (bcall $ ASTMb modSearchKey ASTType_EH) (return Nothing) $ \(eh, _) -> do
               ecu  <- bcall $ EcuOfPrevNameAndPath modSearchKey
               opts <- bcall $ EHCOptsOf modNm
               crsi <- bcall $ CRSIOfName modSearchKey ASTType_EH
%%[[(50 codegen)
               mieimpl <- bcall $ ImportExportImpl modSearchKey (ehcOptOptimizationScope opts)
%%]]
               let  mbEH   = _ecuMbEH ecu
                    ehSem  = EHSem.wrap_AGItf (EHSem.sem_AGItf eh)
                                              ((crsi ^. crsiEHInh)
                                                     { EHSem.moduleNm_Inh_AGItf                = ecuModNm ecu
                                                     , EHSem.gUniq_Inh_AGItf                   = crsi ^. crsiHereUID
                                                     , EHSem.opts_Inh_AGItf                    = opts
%%[[(50 codegen)
                                                     , EHSem.importUsedModules_Inh_AGItf       = ecuImportUsedModules ecu
                                                     , EHSem.moduleImportExportImpl_Inh_AGItf  = mieimpl
%%]]
%%[[50
                                                     , EHSem.isMainMod_Inh_AGItf               = ecuIsMainMod ecu
%%]]
                                                     })
%%[[(8 hmtyinfer)
                    about  = "EH analyses: Type checking"
%%][8
                    about  = "EH analyses"
%%]]
                    errs   = Seq.toList $ EHSem.allErrSq_Syn_AGItf ehSem
                    
               cpSetLimitErrsWhen 5 about errs
               bUpdECU modNm $! ecuStoreEHSem $! ehSem
               when (ehcOptEmitEH opts)
                    (liftIO $ putPPFPath (mkOutputFPath opts modNm (ecuFilePath ecu) "eh2") (EHSem.pp_Syn_AGItf ehSem) 1000)
               when (ehcOptShowEH opts)
                    (liftIO $ putWidthPPLn 120 (EHSem.pp_Syn_AGItf ehSem))
%%[[99
               isTopMod <- bcall $ IsTopMod modNm
%%]]
%%[[8
               when (ehcOptShowAst opts)
                    (liftIO $ putPPLn (EHSem.ppAST_Syn_AGItf ehSem))
%%][99
               when (isTopMod && ehcOptShowAst opts)
                    (liftIO $ putPPLn (EHSem.ppAST_Syn_AGItf ehSem))
%%][100
%%]]
%%[[(99 hmtyinfer tyderivtree)
               when (isTopMod && ehcOptEmitDerivTree opts /= DerivTreeWay_None)
                    (liftIO $ putPPFPath (mkOutputFPath opts modNm (ecuFilePath ecu) "lhs") (EHSem.dt_Syn_AGItf ehSem) 1000)
%%][100
%%]]
               breturn $ Just ehSem

          FoldEHPMb modSearchKey@((modNm,_),_) astpipe -> do
               -- maybeM (bcall $ ASTPMb modSearchKey astpipe) (return Nothing) $ \(eh, _, astpipe) -> do
               maybe2M (return $ astpMbFromEH False astpipe)
                       (\(p,_) -> bcall $ ASTPMb modSearchKey p) (return Nothing) $ \(ASTResult {_astresAST=eh}) -> do
               ecu  <- bcall $ EcuOfPrevNameAndPath modSearchKey
               opts <- bcall $ EHCOptsOf modNm
               crsi <- bcall $ CRSIOfNameP modSearchKey astpipe
%%[[(50 codegen)
               mieimpl <- bcall $ ImportExportImpl modSearchKey (ehcOptOptimizationScope opts)
%%]]
               let  mbEH   = _ecuMbEH ecu
                    ehSem  = EHSem.wrap_AGItf (EHSem.sem_AGItf eh)
                                              ((crsi ^. crsiEHInh)
                                                     { EHSem.moduleNm_Inh_AGItf                = ecuModNm ecu
                                                     , EHSem.gUniq_Inh_AGItf                   = crsi ^. crsiHereUID
                                                     , EHSem.opts_Inh_AGItf                    = opts
%%[[(50 codegen)
                                                     , EHSem.importUsedModules_Inh_AGItf       = ecuImportUsedModules ecu
                                                     , EHSem.moduleImportExportImpl_Inh_AGItf  = mieimpl
%%]]
%%[[50
                                                     , EHSem.isMainMod_Inh_AGItf               = ecuIsMainMod ecu
%%]]
                                                     })
%%[[(8 hmtyinfer)
                    about  = "EH analyses: Type checking"
%%][8
                    about  = "EH analyses"
%%]]
                    errs   = Seq.toList $ EHSem.allErrSq_Syn_AGItf ehSem
                    
               cpSetLimitErrsWhen 5 about errs
               bUpdECU modNm $! ecuStoreEHSem $! ehSem
               when (ehcOptEmitEH opts)
                    (liftIO $ putPPFPath (mkOutputFPath opts modNm (ecuFilePath ecu) "eh2") (EHSem.pp_Syn_AGItf ehSem) 1000)
               when (ehcOptShowEH opts)
                    (liftIO $ putWidthPPLn 120 (EHSem.pp_Syn_AGItf ehSem))
%%[[99
               isTopMod <- bcall $ IsTopMod modNm
%%]]
%%[[8
               when (ehcOptShowAst opts)
                    (liftIO $ putPPLn (EHSem.ppAST_Syn_AGItf ehSem))
%%][99
               when (isTopMod && ehcOptShowAst opts)
                    (liftIO $ putPPLn (EHSem.ppAST_Syn_AGItf ehSem))
%%][100
%%]]
%%[[(99 hmtyinfer tyderivtree)
               when (isTopMod && ehcOptEmitDerivTree opts /= DerivTreeWay_None)
                    (liftIO $ putPPFPath (mkOutputFPath opts modNm (ecuFilePath ecu) "lhs") (EHSem.dt_Syn_AGItf ehSem) 1000)
%%][100
%%]]
               breturn $ Just ehSem

%%[[99
          FPathPreprocessedWithCPP pkgKeyDirL modSearchKey@((modNm,_),_) -> do
               -- Just <$> cpPreprocessWithCPP pkgKeyDirL modNm
               -- cr <- get
               opts <- bcall $ EHCOptsOf modNm
               ecu  <- bcall $ EcuOf modNm
               let fp              = ecuSrcFilePath ecu
                   fpCPP           = fpathSetSuff {- mkOutputFPath opts modNm fp -} (maybe "" (\s -> s ++ "-") (fpathMbSuff fp) ++ "cpp") fp
                   -- fpCPP           = fpathSetBase {- mkOutputFPath opts modNm fp -} (fpathBase fp ++ "-cpp") fp
                   shellCmdCpp     = Cfg.shellCmdOverride opts Cfg.shellCmdCpp PgmExec_CPP
                   shellCmdCppOpts = execOptsPlain $ Map.findWithDefault [] shellCmdCpp $ ehcOptExecOptsMp opts
                   preCPP  = mkShellCmd' [Cmd_CPP,Cmd_CPP_Preprocessing] shellCmdCpp
                               (  Cfg.cppOpts ++ gccDefs opts ["CPP"]
                               ++ map cppOptF shellCmdCppOpts -- [ {- "traditional-cpp", -} {- "std=gnu99", -} "fno-show-column", "P" ]
%%[[(99 codegen)
                               ++ [ cppOptI d | d <- gccInclDirs opts pkgKeyDirL ]
%%]]
                               ++ ehcOptCmdLineOpts opts
                               ++ map (cppArg . fpathToStr) [ fp ] -- , fpCPP ]
                               )
               when (ehcOptVerbosity opts >= VerboseALot) $ do
                 cpMsg modNm VerboseALot "CPP"
                 -- liftIO $ putStrLn ("pkg db: " ++ show (ehcOptPkgDb opts))
                 -- liftIO $ putStrLn ("pkg srch filter: " ++ (show $ ehcOptPackageSearchFilter opts))
                 -- liftIO $ putStrLn ("exposed pkgs: " ++ show (pkgExposedPackages $ ehcOptPkgDb opts))
                 -- liftIO $ putStrLn ("pkgKeyDirL: " ++ show pkgKeyDirL)
                 liftIO $ putStrLn $ showShellCmd preCPP

               -- when (ecuCanCompile ecu)
               ifM (bcall $ CanCompile modNm) 
                 (do liftIO $ fpathEnsureExists fpCPP
                     cpSystem' (Just $ fpathToStr fpCPP) preCPP
                     cpRegisterFilesToRm [fpCPP]
                     bUpdECU modNm (ecuStoreCppFilePath fpCPP)
                     breturn fpCPP
                 )
                 (do cpSetLimitErrsWhen 1 "CPP" [rngLift emptyRange Err_CannotCreateFile (show modNm) (fpathToStr fpCPP) ]
                     -- should not arrive at usage of this result
                     breturn $ panic $ "BuildFunction.Run.bcall FPathPreprocessedWithCPP: " ++ fpathToStr fpCPP 
                 )
%%]]

%%[[(50 corein)
          FoldCoreMod modSearchKey@((modNm,_),_) -> do
               ecu  <- bcall $ EcuOfPrevNameAndPath modSearchKey
               core <- bcall $ ASTFromFile modSearchKey (AlwaysEq ASTFileTimeHandleHow_AbsenceIsError) ASTType_Core (_ecuASTFileContent ecu, ASTFileUse_Src) ASTFileTiming_Current
               opts <- bcall $ EHCOptsOf modNm
               let inh      = Core2ChkSem.Inh_CodeAGItf
                                  { Core2ChkSem.opts_Inh_CodeAGItf = opts
                                  , Core2ChkSem.moduleNm_Inh_CodeAGItf = modNm
                                  -- , Core2ChkSem.dataGam_Inh_CodeAGItf = EHSem.dataGam_Inh_AGItf $ crsi ^. crsiEHInh
                                  }
                   coreSem  = Core2ChkSem.cmodCheck' inh core
                   hasMain  = Core2ChkSem.hasMain_Syn_CodeAGItf coreSem
                   modNm'   = Core2ChkSem.realModuleNm_Syn_CodeAGItf coreSem
                   impNmS   = Set.fromList $ Core2ChkSem.impModNmL_Syn_CodeAGItf coreSem
                   mod      = Core2ChkSem.mod_Syn_CodeAGItf coreSem
               (modNmNew, newPrev) <- newModNm modNm modNm' $
                 ( ecuStoreCoreSemMod coreSem
                 . ecuSetHasMain hasMain
                 . ecuStoreMod mod
                 . ecuStoreSrcDeclImpS impNmS
                 )
               breturn
                 ( coreSem
                 , modNmNew
                 , impNmS
                 , mod
                 , hasMain
                 , newPrev
                 )
  
          FoldCoreModPMb modSearchKey@((modNm,_),_) astpipe -> do
               maybe2M (return $ astpMbFromCoreSrc False astpipe)
                       (\(p,_) -> bcall $ ASTPMb modSearchKey p) (return Nothing) $ \(ASTResult {_astresAST=core}) -> do
               -- ecu  <- bcall $ EcuOfPrevNameAndPath modSearchKey
               -- core <- bcall $ ASTFromFile modSearchKey (AlwaysEq ASTFileTimeHandleHow_AbsenceIsError) ASTType_Core (_ecuASTFileContent ecu, ASTFileUse_Src) ASTFileTiming_Current
               opts <- bcall $ EHCOptsOf modNm
               let inh      = Core2ChkSem.Inh_CodeAGItf
                                  { Core2ChkSem.opts_Inh_CodeAGItf = opts
                                  , Core2ChkSem.moduleNm_Inh_CodeAGItf = modNm
                                  -- , Core2ChkSem.dataGam_Inh_CodeAGItf = EHSem.dataGam_Inh_AGItf $ crsi ^. crsiEHInh
                                  }
                   coreSem  = Core2ChkSem.cmodCheck' inh core
                   hasMain  = Core2ChkSem.hasMain_Syn_CodeAGItf coreSem
                   modNm'   = Core2ChkSem.realModuleNm_Syn_CodeAGItf coreSem
                   impNmS   = Set.fromList $ Core2ChkSem.impModNmL_Syn_CodeAGItf coreSem
                   mod      = Core2ChkSem.mod_Syn_CodeAGItf coreSem
               (modNmNew, newPrev) <- newModNm modNm modNm' $
                 ( ecuStoreCoreSemMod coreSem
                 . ecuSetHasMain hasMain
                 . ecuStoreMod mod
                 . ecuStoreSrcDeclImpS impNmS
                 )
               breturn $ Just
                 ( coreSem
                 , modNmNew
                 , impNmS
                 , mod
                 , hasMain
                 , newPrev
                 )
  
%%]]

%%[[(8 core grin)
          FoldCore2Grin modSearchKey@((modNm,_),_) -> do
               maybeM (bcall $ FoldCore2GrinMb modSearchKey) (undefFor modNm) breturn
               
          FoldCore2GrinMb modSearchKey@((modNm,_),_) -> do
               -- core <- bcall $ ASTFromFile modSearchKey (AlwaysEq ASTFileTimeHandleHow_AbsenceIsError) ASTType_Core (_ecuASTFileContent ecu, ASTFileUse_Src) ASTFileTiming_Current
               maybeM (bcall $ ASTMb modSearchKey ASTType_Core) (return Nothing) $ \(core, _) -> do
               ecu  <- bcall $ EcuOfPrevNameAndPath modSearchKey
               opts <- bcall $ EHCOptsOf modNm
               crsi <- bcall $ CRSIOfName modSearchKey ASTType_Core
               let  coreInh  = _crsiCoreInh crsi
                    coreSem  = Core2GrSem.wrap_CodeAGItf
                                 (Core2GrSem.sem_CodeAGItf (Core.CodeAGItf_AGItf core))
                                 (coreInh { Core2GrSem.gUniq_Inh_CodeAGItf                         = crsi ^. crsiHereUID
                                          , Core2GrSem.opts_Inh_CodeAGItf                          = opts
%%[[50
                                          , Core2GrSem.importUsedModules_Inh_CodeAGItf             = ecuImportUsedModules ecu
%%]]
                                          })
               bUpdECU modNm $ ecuStoreCoreSem coreSem
               breturn $ Just coreSem

          FoldCore2GrinPMb modSearchKey@((modNm,_),_) astpipe -> do
               maybe2M (return $ astpMbFromCoreToGrin False astpipe)
                       (\(p,_) -> bcall $ ASTPMb modSearchKey p) (return Nothing) $ \(ASTResult {_astresAST=core}) -> do
               ecu  <- bcall $ EcuOfPrevNameAndPath modSearchKey
               opts <- bcall $ EHCOptsOf modNm
               crsi <- bcall $ CRSIOfNameP modSearchKey astpipe
               let  coreInh  = _crsiCoreInh crsi
                    coreSem  = Core2GrSem.wrap_CodeAGItf
                                 (Core2GrSem.sem_CodeAGItf (Core.CodeAGItf_AGItf core))
                                 (coreInh { Core2GrSem.gUniq_Inh_CodeAGItf                         = crsi ^. crsiHereUID
                                          , Core2GrSem.opts_Inh_CodeAGItf                          = opts
%%[[50
                                          , Core2GrSem.importUsedModules_Inh_CodeAGItf             = ecuImportUsedModules ecu
%%]]
                                          })
               bUpdECU modNm $ ecuStoreCoreSem coreSem
               breturn $ Just coreSem
%%]]

%%[[(8 core corerun)
{-
          FoldCore2CoreRun modSearchKey@((modNm,_),_) -> do
               maybeM (bcall $ FoldCore2CoreRunMb modSearchKey) (undefFor modNm) breturn
               
          FoldCore2CoreRunMb modSearchKey@((modNm,_),_) -> do
               -- core <- bcall $ ASTFromFile modSearchKey (AlwaysEq ASTFileTimeHandleHow_AbsenceIsError) ASTType_Core (_ecuASTFileContent ecu, ASTFileUse_Src) ASTFileTiming_Current
               maybeM (bcall $ ASTMb modSearchKey ASTType_Core) (return Nothing) $ \(core, _) -> do
               ecu  <- bcall $ EcuOfPrevNameAndPath modSearchKey
               opts <- bcall $ EHCOptsOf modNm
               crsi <- bcall $ CRSIOfName modSearchKey ASTType_Core
               let  
%%[[8
                    hasMain                = True
%%][50
                    hasMain                = ecuHasMain ecu
%%]]
                    core2RunInh            = crsiCore2RunInh crsi
                    sem                    = Core2CoreRunSem.cmod2CoreRun'' opts hasMain 0 core2RunInh core
                    -- core2RunInh'           = nm2ref `CoreRun.nm2refUnion` core2RunInh
               -- between module flow part, TBD: corerun, nmref part
               -- cpUpdSI (\crsi -> crsi {crsiCore2RunInh = core2RunInh'})
               -- per module part
               bUpdECU modNm ( ecuStoreCore2CoreRunSem sem
                             -- . ecuStoreCoreRun corerun
                             )
               breturn $ Just sem
-}
               
          FoldCore2CoreRunPMb modSearchKey@((modNm,_),_) astpipe -> do
               -- maybeM (bcall $ ASTPMb modSearchKey astpipe) (return Nothing) $ \(core, _, astpipe) -> do
               maybe2M (return $ astpMbFromCoreToCoreRun False astpipe)
                       (\(p,_) -> bcall $ ASTPMb modSearchKey p) (return Nothing) $ \(ASTResult {_astresAST=core}) -> do
               ecu  <- bcall $ EcuOfPrevNameAndPath modSearchKey
               opts <- bcall $ EHCOptsOf modNm
               crsi <- bcall $ CRSIOfNameP modSearchKey astpipe
               let  
%%[[8
                    hasMain                = True
%%][50
                    hasMain                = ecuHasMain ecu
%%]]
                    sem                    = Core2CoreRunSem.cmod2CoreRun'' opts hasMain (crsi ^. crsiCoreRunState ^. crcrsiNrOfModules) (crsi ^. crsiCoreRunState ^. crcrsiNm2RefMp) core
                    -- nm2ref'           = nm2ref `CoreRun.nm2refUnion` nm2ref
               cpUpdSI $ (crsiCoreRunState ^* crcrsiNrOfModules) ^$= (+1)
               -- between module flow part, TBD: corerun, nmref part
               -- cpUpdSI (\crsi -> crsi {crsiCore2RunInh = nm2ref'})
               -- per module part
               bUpdECU modNm ( ecuStoreCore2CoreRunSem sem
                             -- . ecuStoreCoreRun corerun
                             )
               breturn $ Just sem
               
%%]]

%%[[(50 corerun corerunin)
          FoldCoreRunMod modSearchKey@((modNm,_),_) -> do
               ecu  <- bcall $ EcuOfPrevNameAndPath modSearchKey
               crr  <- bcall $ ASTFromFile modSearchKey (AlwaysEq ASTFileTimeHandleHow_AbsenceIsError) ASTType_CoreRun (_ecuASTFileContent ecu, ASTFileUse_Src) ASTFileTiming_Current
               opts <- bcall $ EHCOptsOf modNm
               crsi <- bcall $ CRSIOfName modSearchKey ASTType_CoreRun
               let inh      = CoreRun2ChkSem.Inh_AGItf
                                  { CoreRun2ChkSem.opts_Inh_AGItf = opts
                                  , CoreRun2ChkSem.moduleNm_Inh_AGItf = modNm
                                  , CoreRun2ChkSem.moduleNr_Inh_AGItf = crsi ^. crsiCoreRunState ^. crcrsiNrOfModules
                                  -- , CoreRun2ChkSem.dataGam_Inh_AGItf = EHSem.dataGam_Inh_AGItf $ _crsiEHInh crsi
                                  }
                   crrSem   = CoreRun2ChkSem.crmodCheck' inh crr
                   hasMain  = CoreRun2ChkSem.hasMain_Syn_AGItf crrSem
                   modNm'   = CoreRun2ChkSem.realModuleNm_Syn_AGItf crrSem
                   impNmS   = Set.fromList $ CoreRun2ChkSem.impModNmL_Syn_AGItf crrSem
                   mod      = CoreRun2ChkSem.mod_Syn_AGItf crrSem
               (modNmNew, newPrev) <- newModNm modNm modNm' $
                 ( ecuStoreCoreRunSemMod crrSem
                 . ecuSetHasMain hasMain
                 . ecuStoreMod mod
                 . ecuStoreSrcDeclImpS impNmS
                 . (ecuCoreRun ^= CoreRun2ChkSem.crr_Syn_AGItf crrSem)
                 )
               cpUpdSI $ (crsiCoreRunState ^* crcrsiNrOfModules) ^$= (+1)
               breturn
                 ( crrSem
                 , modNmNew
                 , impNmS
                 , mod
                 , hasMain
                 , newPrev
                 )
  
          FoldCoreRunModPMb modSearchKey@((modNm,_),_) astpipe -> do
               maybe2M (return $ astpMbFromCoreRunSrc False astpipe)
                       (\(p,_) -> bcall $ ASTPMb modSearchKey p) (return Nothing) $ \(ASTResult {_astresAST=crr}) -> do
               opts <- bcall $ EHCOptsOf modNm
               crsi <- bcall $ CRSIOfNameP modSearchKey astpipe
               let inh      = CoreRun2ChkSem.Inh_AGItf
                                  { CoreRun2ChkSem.opts_Inh_AGItf = opts
                                  , CoreRun2ChkSem.moduleNm_Inh_AGItf = modNm
                                  , CoreRun2ChkSem.moduleNr_Inh_AGItf = crsi ^. crsiCoreRunState ^. crcrsiNrOfModules
                                  -- , CoreRun2ChkSem.dataGam_Inh_AGItf = EHSem.dataGam_Inh_AGItf $ _crsiEHInh crsi
                                  }
                   crrSem   = CoreRun2ChkSem.crmodCheck' inh crr
                   hasMain  = CoreRun2ChkSem.hasMain_Syn_AGItf crrSem
                   modNm'   = CoreRun2ChkSem.realModuleNm_Syn_AGItf crrSem
                   impNmS   = Set.fromList $ CoreRun2ChkSem.impModNmL_Syn_AGItf crrSem
                   mod      = CoreRun2ChkSem.mod_Syn_AGItf crrSem
               (modNmNew, newPrev) <- newModNm modNm modNm' $
                 ( ecuStoreCoreRunSemMod crrSem
                 . ecuSetHasMain hasMain
                 . ecuStoreMod mod
                 . ecuStoreSrcDeclImpS impNmS
                 . (ecuCoreRun ^= CoreRun2ChkSem.crr_Syn_AGItf crrSem)
                 )
               cpUpdSI $ (crsiCoreRunState ^* crcrsiNrOfModules) ^$= (+1)
               breturn $ Just
                 ( crrSem
                 , modNmNew
                 , impNmS
                 , mod
                 , hasMain
                 , newPrev
                 )
  
%%]]

          
          _ -> panic $ "BuildFunction.Run.bcall: not implemented: " ++ show bfun

        -- finalize
        end

        -- debug/trace
        getl cstk >>= \stk -> cpTr TraceOn_BuildFun $ ["<<<<< " ++ show bfun] ++ map show stk

        -- the result
        return res
  where
    -- lens access
    st    = crStateInfo ^* crsiBState
    cstk  = st ^* bstateCallStack

    -- call init/finalization
    start = cstk =$: (BFun bfun :)
    end   = cstk =$: tail
    
    -- memoize
    bmemo :: Typeable f => f res -> EHCompilePhaseT m ()
    bmemo res = do
        (BFun bfun : _) <- getl $ st ^* bstateCallStack
        case cast bfun of
          Just bfun -> do
               cpTr TraceOn_BuildFun $ ["memo  " ++ show bfun]
               st ^* bstateCache =$: bcacheInsert bfun res
          _ -> panic $ "BuildFunction.Run.bcall.bmemo: " ++ show bfun
    bmemo' :: Typeable xxx => xxx -> EHCompilePhaseT m ()
    bmemo' res = do
        (BFun bfun : _) <- getl $ st ^* bstateCallStack
        case cast bfun of
          Just bfun -> do
               cpTr TraceOn_BuildFun $ ["memo  " ++ show bfun]
               st ^* bstateCache =$: bcacheInsert bfun (Identity res)
          _ -> panic $ "BuildFunction.Run.bcall.bmemo': " ++ show bfun ++ ", " ++ show (typeOf bfun) ++ ", " ++ show (typeOf res)

    -- construct a reference to something and also yield corresponding result
    brefto' :: BFun' res -> BRef res -> EHCompilePhaseT m (Maybe res)
    brefto' bfun ref = bmemo ref >> bderef' ref
    
    brefto :: BFun' res -> BRef res -> EHCompilePhaseT m res
    brefto bfun ref = fmap (panicJust $ "BuildFunction.Run.bcall.brefto " ++ show bfun) $ brefto' bfun ref

    -- memoize & return
    breturn :: res -> EHCompilePhaseT m res
    breturn res = do
        bmemo (Identity res)
        return res

    -- lookup in cache
    lkup :: BFun' res -> BCache -> EHCompilePhaseT m (Maybe res)
    lkup bfun bcache =
        case bcacheLookup bfun bcache of
          Just (res :: Identity res) -> return $ Just $ runIdentity res
          _ -> case bcacheLookup bfun bcache of
            Just (ref :: BRef res) -> bderef' ref
            _ -> return Nothing

    -- factored out: new module name extracted from src file
    newModNm :: HsName -> HsName -> (EHCompileUnit -> EHCompileUnit) -> EHCompilePhaseT m (HsName, Maybe PrevSearchInfo)
    newModNm modNm modNm' upd = do
%%[[8
        let modNmNew = modNm
%%][50
        modNmNew <- ifM (bcall $ IsTopMod modNm)
          (do
            cpUpdCUWithKey modNm (\_ ecu -> (modNm', upd $ cuUpdKey modNm' ecu))
            when (modNm /= modNm') $ do
              crStateInfo ^* crsiBState ^* bstateCache ^* bcacheModNmForward =$: Map.insert modNm modNm'
              cpTr TraceOn_BuildRef ["newModNm rename: " ++ show modNm ++ " -> " ++ show modNm']
            return modNm'
          )
          (do
            bUpdECU modNm upd
            return modNm
          )
%%]]
        ecu <- bcall $ EcuOf modNmNew
        let newPrev = Just (modNmNew, (ecuSrcFilePath ecu, ecuFileLocation ecu))
%%[[50
        bUpdECU modNmNew $ ecuMbPrevSearchInfo ^= newPrev
%%]]
        return (modNmNew, newPrev)

    -- undefined result
    undefFor modNm = return $ panic $ "BuildFunction.Run.bcall (" ++ show bfun ++ ") undefined result related to " ++ show modNm
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived utils, wrappers around bcalls. Results need/are not cached.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
-- | Get AST from file, maybe...
-- Wrapper around bcalls.
bASTFromFileEither
  :: forall res m .
     (Typeable res, EHCCompileRunner m)
  => PrevFileSearchKey              		--- ^ module name and possibly known path
     -> Bool								--- ^ errors are returned instead of reported directly
     -> (AlwaysEq ASTFileTimeHandleHow) 	--- ^ how to deal with timestamp
     -> ASTType                         	--- ^ content type
     -> ASTSuffixKey                        --- ^ suffix and content variation
     -> ASTFileTiming                   	--- ^ timing (i.e. previous or current)
     -> EHCompilePhaseT m
          ( Either
              ( String, [Err] )
              ( res
              , BRef res
%%[[8
              , ()
%%][50
              , ClockTime
%%]]
              )
          )
bASTFromFileEither modSearchKey yieldErr chkTimeStamp asttype skey tkey = do
    let dflt = Left ("",[])
%%[[8
    let tm = ()
    do
%%][50
    mbtm@(~(Just tm)) <- bcall $ ModfTimeOfFile modSearchKey asttype skey tkey
    if isJust mbtm
      then do
%%]]
        -- TBD: faulty...
        -- (ref :: BRef res) <- bcall $ ASTRefFromFileMb modSearchKey chkTimeStamp asttype skey tkey
        eithref <- bcall $ ASTRefFromFileEither modSearchKey yieldErr chkTimeStamp asttype skey tkey
        case eithref of
          Right ref -> fmap (maybe dflt (\ast -> Right (ast, ref, tm))) $ bderef' ref
          Left  e   -> return $ Left e
%%[[50
      else return dflt
%%]]

-- | Get AST from file, maybe...
-- Wrapper around bcalls.
bASTFromFileMb
  :: -- forall res m .
     (Typeable res, EHCCompileRunner m)
  => PrevFileSearchKey              --- ^ module name and possibly known path
     -> (AlwaysEq ASTFileTimeHandleHow) --- ^ how to deal with timestamp
     -> ASTType                         --- ^ content type
     -> ASTSuffixKey                        --- ^ suffix and content variation
     -> ASTFileTiming                   --- ^ timing (i.e. previous or current)
     -> EHCompilePhaseT m (Maybe (res, BRef res))
bASTFromFileMb modSearchKey chkTimeStamp asttype skey tkey = 
  fmap (either (const Nothing) (Just . tup123to12)) $ bASTFromFileEither modSearchKey False chkTimeStamp asttype skey tkey
%%]

%%[8
%%]

%%[8
-- | Get possible Hs semantics
bGetHsSemMb
  :: EHCCompileRunner m
  => HsName
  -> EHCompilePhaseT m
       ( Maybe
         ( AST_HS_Sem_Check             -- all semantics
       ) )
bGetHsSemMb modNm = fmap (fmap extr) $ bcall $ FoldHsMb (mkPrevFileSearchKeyWithName modNm)
     where
%%[[8
       extr hsSem = hsSem
%%][50
       extr (hsSem, _) = hsSem
%%]]
%%]

%%[8
-- | Get possible Hs semantics
bGetHsSemPMb
  :: EHCCompileRunner m
  => HsName
  -> ASTPipe
  -> EHCompilePhaseT m
       ( Maybe
         ( AST_HS_Sem_Check             -- all semantics
       ) )
bGetHsSemPMb modNm astpipe = fmap (fmap extr) $ bcall $ FoldHsPMb (mkPrevFileSearchKeyWithName modNm) astpipe
     where
%%[[8
       extr hsSem = hsSem
%%][50
       extr (hsSem, _) = hsSem
%%]]
%%]

%%[8
-- | Check whether a particular flow of semantics is allowed, and mark it to have occurred
bAllowFlow :: EHCCompileRunner m => HsName -> Set.Set ASTType -> ASTSemFlowStage -> ASTType -> EHCompilePhaseT m Bool
bAllowFlow modNm forAstTypes flowstage asttype = do
   ecu <- bcall (EcuOf modNm)
   let nyf = not $ ecuHasAlreadyFlowed asttype flowstage ecu
       res = nyf && asttype `Set.member` forAstTypes
   when res $ cpUpdAlreadyFlowIntoCRSI modNm asttype flowstage
   return res

-- | Check whether a particular flow of semantics is allowed, and mark it to have occurred
bAllowFlowP
  :: EHCCompileRunner m
  => HsName
  -> ASTPipe
  -> ASTSemFlowStage
  -> (ASTPipe -> Maybe (ASTPipe,ASTAlreadyFlowIntoCRSIFromToInfo))
  -> EHCompilePhaseT m (Maybe ASTPipe)
bAllowFlowP modNm astpipe flowstage astpred = do
   ecu <- bcall (EcuOf modNm)
   let fnd@(~(Just (astpipeFnd,flowFnd)))
               = astpred astpipe
       asttype = astpType astpipeFnd
       key     = (flowstage, Just flowFnd)
       nyf     = not $ ecuHasAlreadyFlowedWith asttype key ecu
   if isJust fnd && nyf
     then do
       cpUpdAlreadyFlowIntoCRSIWith modNm asttype key
       return $ Just astpipeFnd
     else return Nothing
%%]

%%[8
-- | Is the HI file newer?
bHIIsNewerThan :: EHCCompileRunner m => HsName -> HsName -> EHCompilePhaseT m Bool
bHIIsNewerThan modNm1 modNm2 = do
    mbNewer <- bcall $ ASTFileIsNewerThan
      (mkPrevFileSearchKeyWithName modNm1, ASTType_HI, (ASTFileContent_Binary, ASTFileUse_Cache), ASTFileTiming_Prev)
      (mkPrevFileSearchKeyWithName modNm2, ASTType_HI, (ASTFileContent_Binary, ASTFileUse_Cache), ASTFileTiming_Prev)
    return $ maybe False id mbNewer
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% References
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
-- | Dereference an indirection into compilation state, possibly with a result, and a setter
bderef'' :: forall res m . (Typeable res, EHCCompileRunner m) => BRef res -> EHCompilePhaseT m (Maybe res, Maybe (res -> EHCompilePhaseT m ()))
bderef'' bref = do
    cr <- get
    let st   = cr ^. crStateInfo
        opts = st ^. crsiOpts

    res@(r1,r2) <- case bref of
      BRef_CRSI -> return (Just $ cr ^. crStateInfo, Nothing)

%%[[99
      BRef_ExposedPackages -> return (Just $ pkgExposedPackages $ ehcOptPkgDb opts, Nothing)
%%]]

      BRef_ECU modNm -> do
          mbecu <- bLookupECU modNm
          return (mbecu, Just $ \ecu -> bUpdECU modNm (const ecu))

      BRef_EHCOpts modNm -> do
%%[[8
          let o = opts
%%][99
          mbecu <- bLookupECU modNm
          let o = maybe opts id $ mbecu >>= ecuMbOpts
%%]]
          return (Just o, Nothing)

      BRef_AST modSearchKey@((modNm,_),_) asttype -> case asthandlerLookup asttype of
          Just (hdlr :: ASTHandler' res) -> 
            case _asthdlrASTLens hdlr of
              Just l -> do
                ecu <- bcall $ EcuOfPrevNameAndPath modSearchKey
                return (ecu ^. l, Just $ \ast -> bUpdECU modNm $ l ^= Just ast)
              _ -> dflt
          _ -> dflt
        where dflt = return (Nothing, Nothing)

      BRef_ASTFile modSearchKey@((modNm,_),_) asttype skey tkey -> case asthandlerLookup asttype of
          Just (hdlr :: ASTHandler' res) -> case astsuffixLookup skey $ _asthdlrSuffixRel hdlr of
            Just suffinfo -> case Map.lookup tkey $ _astsuffinfoASTLensMp suffinfo of
              Just l -> do
                ecu <- bcall $ EcuOfPrevNameAndPath modSearchKey
                return (ecu ^. l, Just $ \ast -> bUpdECU modNm $ l ^= Just ast)
              _ -> dflt
            _ -> dflt
          _ -> dflt
        where dflt = return (Nothing, Nothing)
    cpTr TraceOn_BuildRef ["bderef'': ok?=" ++ show (isJust r1) ++ "," ++ show (isJust r2) ++ ", ref=" ++ show bref]
    return res

-- | Dereference an indirection into compilation state
bderef' :: forall res m . (Typeable res, EHCCompileRunner m) => BRef res -> EHCompilePhaseT m (Maybe res)
bderef' bref = do
    cpTr TraceOn_BuildRef $ ["bderef' " ++ show bref]
    fmap fst $ bderef'' bref

-- | Dereference an indirection into compilation state
bderef :: forall res m . (Typeable res, EHCCompileRunner m) => BRef res -> EHCompilePhaseT m res
bderef bref = do
    fmap (panicJust $ "BuildFunction.Run.bderef " ++ show bref) $ bderef' bref
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASTPipe predicates/guards
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
astpMbFromHSToEH top p = case p of {ASTPipe_Derived ASTType_EH p' | astpType p' == ASTType_HS -> Just (if' top p p', (Just ASTType_EH, ASTType_HS)); _ -> Nothing}
astpMbFromEHToCore top p = case p of {ASTPipe_Derived ASTType_Core p' | astpType p' == ASTType_EH -> Just (if' top p p', (Just ASTType_Core, ASTType_EH)); _ -> Nothing}
astpMbFromHS top p = case p of {ASTPipe_Derived _ p' | astpType p' == ASTType_HS -> Just (if' top p p', (Nothing, ASTType_HS)); _ -> Nothing}
astpMbFromEH top p = case p of {ASTPipe_Derived _ p' | astpType p' == ASTType_EH -> Just (if' top p p', (Nothing, ASTType_EH)); _ -> Nothing}
%%]

%%[(8 core grin)
astpMbFromCoreToGrin top p = case p of {ASTPipe_Derived ASTType_Grin p' | astpType p' == ASTType_Core -> Just (if' top p p', (Just ASTType_Grin, ASTType_Core)); _ -> Nothing}
%%]

%%[(8 core corein)
astpMbFromCoreSrc top p = case p of {ASTPipe_Derived _ p'@(ASTPipe_Src ASTType_Core) -> Just (if' top p p', (Nothing, ASTType_Core)); _ -> Nothing}
%%]

%%[(8 corerun corerunin)
astpMbFromCoreRunSrc top p = case p of {ASTPipe_Derived _ p'@(ASTPipe_Src ASTType_CoreRun) -> Just (if' top p p', (Nothing, ASTType_CoreRun)); _ -> Nothing}
%%]

%%[(8 core corerun)
astpMbFromCoreToCoreRun top p = case p of {ASTPipe_Derived ASTType_CoreRun p' | astpType p' == ASTType_Core -> Just (if' top p p', (Just ASTType_CoreRun, ASTType_Core)); _ -> Nothing}
%%]


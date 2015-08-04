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
%%[8 import (qualified Data.Map as Map, qualified Data.Set as Set, qualified UHC.Util.FastSeq as Seq)
%%]
%%[8 import (Control.Applicative, Control.Monad.State, Control.Monad.Error)
%%]

%%[50 import(qualified UHC.Util.Rel as Rel)
%%]
%%[50 import (System.Directory)
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

          CRSIWithImps ((modNm,_),mbPrev) imps forAstTypes -> do
               impsmp <- bcall $ ImportsRecursiveWithImps mbPrev imps
               let compileOrder = scc [ (n, Set.toList i) | (n,i) <- Map.toList impsmp ]
               cpTr TraceOn_BuildSccImports $ [show modNm ++ " " ++ show imps] ++ [show compileOrder]
               bcall $ CRSIWithCompileOrder modNm compileOrder forAstTypes
%%]]

          CRSIOfName modSearchKey forAstType -> do
%%[[50
               ecu <- bcall $ EcuOfPrevNameAndPath modSearchKey
               (modNm, imps) <- bcall $ ImportsOfName modSearchKey
               let allowFlow = bAllowFlow modNm forAstTypes ASTSemFlowStage_PerModule
               
               -- first recursively take care of all imports
               bcall $ CRSIWithImps (mkPrevFileSearchKeyWithNameMbPrev modNm (_ecuMbPrevSearchInfo ecu)) imps forAstTypes
%%]]
%%[[50
               -- flow EH semantics forward into global state for current module, for a next stage in the pipeline
               -- From: cpFlowEHSem1
               guardMaybeM (allowFlow ASTType_EH) (bcall $ FoldEHMb $ mkPrevFileSearchKeyWithName modNm) (return ()) $ \ehSem -> do
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
                   cpUpdCU modNm ( ecuStoreHIUsedImpS usedImpS
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
               brefto bfun BRef_CRSI

             where forAstTypes = Set.fromList [minBound .. forAstType]

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
                    (cpUpdCU modNm (ecuSetIsTopMod True))
%%]]
               bmemo $ BRef_ECU modNm
               fmap (panicJust "EcuOfPrevNameAndPath") $ cpMbCU modNm
           
          FPathSearchForFile suff fn -> do
               let fp    = mkTopLevelFPath suff fn
                   modNm = mkHNm $ fpathBase fp
               cpTr TraceOn_BuildFPaths ["FPathSearchForFile: " ++ show modNm ++ ", " ++ fpathToStr fp]
               breturn (modNm, fp)
      
          FPathForAST modSearchKey@((modNmAsked,overr),_) asttype skey tkey -> do
               res@(fp, suffoverr, ecu) <- case overr of
                 ASTFileNameOverride_FPath fp -> (bcall $ EcuOf modNmAsked) >>= \ecu -> return (fp, ASTFileSuffOverride_AsIs, ecu)
                 _	                          -> (bcall $ EcuOfPrevNameAndPath modSearchKey) >>= \ecu -> return (ecuSrcFilePath ecu, ASTFileSuffOverride_Suff skey, ecu)
               breturn res
      
          ASTFromFile modSearchKey@((modNm,_),_) chkTimeStamp asttype skey tkey -> do
               maybeM (bASTFromFileMb modSearchKey chkTimeStamp asttype skey tkey) (undefFor modNm) $ \(res,_) -> breturn res

          ASTRefFromFile modSearchKey@((modNmAsked,overr),_) (AlwaysEq chkTimeStamp) asttype skey@(astfcont,_) tkey -> do
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
                                let errs = _asthdlrPostInputCheck astHdlr opts ecu modNm fpC x
                                if null errs
                                  then setret ref x
                                  else do
                                    cpSetLimitErrsWhen 1 ("Decode AST check " ++ _asthdlrName astHdlr) errs
                                    dflt'
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
                                seterrs es  = cpSetLimitErrsWhen 5 description es >> dflt'
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

            where dflt' :: EHCompilePhaseT m res
                  dflt' = undefFor modNmAsked
                  err' fp k m = do
                    cpSetLimitErrsWhen 1 ("Decode " ++ k ++ " for file " ++ fpathToStr fp) [strMsg $ "No " ++ m ++ " for " ++ k ++ " (" ++ show skey ++ "/" ++ show tkey ++ ")"]
                    dflt'
                  ret ref ast = bmemo' (Just ref) >> return (Just ref)

          AST modSearchKey@((modNm,_),_) asttypeAsked -> do
               maybeM (bcall $ ASTMb modSearchKey asttypeAsked) (undefFor modNm) $ \(res,_) -> breturn res

          ASTMb modSearchKey@((modNmAsked,_),_) asttypeAsked -> do
               -- the source file available to produce the asked AST, not necessarily used
               ecu <- bcall $ EcuOfPrevNameAndPath modSearchKey

%%[[8
               let modNm = modNmAsked
%%][50
               (modNm, _, _) <- bcall $ ModnameAndImports modSearchKey (_ecuASTType ecu)
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
                       where ref = BRef_AST modSearchKey asttypeAsked
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
                         , HI.hiiHIDeclImpModS        = ecuHIDeclImpNmS ecu						-- TBD: sort out, should not be from hi
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
                         -- , HI.hiiHIUsedImpModS = usedImpS		-- TBD: sort out
                         }
%%]]
                   retast hii2
%%]]
                 _ ->
                   if asttypeAsked == _ecuASTType ecu
                     then do
                       -- if the available and asked for AST are of the same type, get the file contents
                       bASTFromFileMb modSearchKey (AlwaysEq ASTFileTimeHandleHow_AbsenceIsError) asttypeAsked (_ecuASTFileContent ecu, _ecuASTFileUse ecu) ASTFileTiming_Current
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

%%[[50
          ModfTimeOfFile modSearchKey@((modNm,_),_) asttype skey tkey -> do
               (fp, suffoverr, ecu) <- bcall $ FPathForAST modSearchKey asttype skey tkey
               opts <- bcall $ EHCOptsOf modNm
               case (asthandlerLookup' asttype $ \hdlr -> do
                         suffinfo <- astsuffixLookup skey $ _asthdlrSuffixRel hdlr
                         let mblens = Map.lookup tkey $ _astsuffinfoModfTimeMp suffinfo
                         return (mblens, asthdlrMkInputFPath hdlr opts ecu suffoverr modNm fp)
                    ) of
                         Just (mblens, fp) -> tm opts ecu (maybe (const id) (\lens -> (lens ^=) . Just) mblens) fp
                         _                 -> breturn Nothing
            where
              tm opts ecu store fp = do
                  let n = fpathToStr fp
                  nExists <- liftIO $ doesFileExist n
                  when (ehcOptVerbosity opts >= VerboseDebug) $ liftIO $ putStrLn ("meta info of: " ++ show (ecuModNm ecu) ++ ", file: " ++ n ++ ", exists: " ++ show nExists)
                  if nExists 
                    then do
                      t <- liftIO $ fpathGetModificationTime fp
                      when (ehcOptVerbosity opts >= VerboseDebug) $ liftIO $ putStrLn ("time stamp of: " ++ show (ecuModNm ecu) ++ ", time: " ++ show t)
                      cpUpdCU modNm $ store t
                      breturn $ Just t
                    else
                      breturn Nothing

          DirOfModIsWriteable modNm -> do
               ecu <- bcall $ EcuOfName modNm
               let fp = ecuSrcFilePath ecu
               pm <- liftIO $ getPermissions (maybe "." id $ fpathMbDir fp)
               let res = writable pm
               -- liftIO $ putStrLn (fpathToStr fp ++ " writ " ++ show res)
               cpUpdCU modNm $ ecuDirIsWritable ^= res
               breturn res

          EcuCanCompile modNm -> do
               ecu  <- bcall $ EcuOfName modNm
               isWr <- bcall $ DirOfModIsWriteable modNm
               mbTm <- bcall $ ModfTimeOfFile (mkPrevFileSearchKeyWithName modNm) (ecu ^. ecuASTType) (ecu ^. ecuASTFileContent, ecu ^. ecuASTFileUse) ASTFileTiming_Current
               breturn $ isJust mbTm && isWr
                     
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
               hs   <- bcall $ ASTFromFile modSearchKey' (AlwaysEq ASTFileTimeHandleHow_AbsenceIsError) ASTType_HS (_ecuASTFileContent ecu, ASTFileUse_SrcImport) ASTFileTiming_Current
               crsi <- bcall $ CRSI
               opts <- bcall $ EHCOptsOf modNm
               let  -- (ecu,crsi,opts,_) = crBaseInfo modNm cr
                    inh        = crsiHSModInh crsi
                    hsSemMod   = HSSemMod.wrap_AGItf (HSSemMod.sem_AGItf hs)
                                                     (inh { HSSemMod.gUniq_Inh_AGItf        = crsi ^. crsiHereUID
                                                          , HSSemMod.moduleNm_Inh_AGItf     = modNm
                                                          })
                    hasMain= HSSemMod.mainValExists_Syn_AGItf hsSemMod
%%[[99
                    pragmas = HSSemMod.fileHeaderPragmas_Syn_AGItf hsSemMod
                    (ecuOpts,modifiedOpts)
                            = ehcOptUpdateWithPragmas pragmas opts
%%]]
               cpUpdCU modNm ( ecuStoreHSSemMod hsSemMod
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
               let doCPP        		= ehcOptCPP opts
                   mkMbPkgKeyDirL doCPP = if doCPP then Just pkgKeyDirL else Nothing
                   mbPkgKeyDirL 		= mkMbPkgKeyDirL doCPP
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
                 if (  not (ehcOptCPP opts2)				-- reinvoke if CPP has not been invoked before
                    || ehcOptCmdLineOptsDoneViaPragma opts2	-- or options have been set via pragma
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
               cpUpdSI (\crsi -> crsi {crsiModMp = Map.insert modNm mmi' mm})		-- this should be made explicit
               cpUpdCU modNm ( ecuStoreHIDeclImpS (HI.hiiHIDeclImpModS hiInfo)
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

          FoldHs modSearchKey@((modNm,_),_) -> do
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
               cpUpdCU modNm ( ecuStoreHSSem hsSem
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
               cpUpdCU modNm $! ecuStoreEHSem $! ehSem
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
               ifM (bcall $ EcuCanCompile modNm) 
                 (do liftIO $ fpathEnsureExists fpCPP
                     cpSystem' (Just $ fpathToStr fpCPP) preCPP
                     cpRegisterFilesToRm [fpCPP]
                     cpUpdCU modNm (ecuStoreCppFilePath fpCPP)
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
  
%%]]

%%[[(8 core)
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
               cpUpdCU modNm $ ecuStoreCoreSem coreSem
               breturn $ Just coreSem
%%]]

%%[[(8 core corerun)
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
               cpUpdCU modNm ( ecuStoreCore2CoreRunSem sem
                             -- . ecuStoreCoreRun corerun
                             )
               breturn $ Just sem
               
%%]]

%%[[(50 corerunin)
          FoldCoreRunMod modSearchKey@((modNm,_),_) -> do
               ecu  <- bcall $ EcuOfPrevNameAndPath modSearchKey
               crr  <- bcall $ ASTFromFile modSearchKey (AlwaysEq ASTFileTimeHandleHow_AbsenceIsError) ASTType_CoreRun (_ecuASTFileContent ecu, ASTFileUse_Src) ASTFileTiming_Current
               opts <- bcall $ EHCOptsOf modNm
               let inh      = CoreRun2ChkSem.Inh_AGItf
                                  { CoreRun2ChkSem.opts_Inh_AGItf = opts
                                  , CoreRun2ChkSem.moduleNm_Inh_AGItf = modNm
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
                 )
               breturn
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
            return modNm'
          )
          (do
            cpUpdCU modNm upd
            return modNm
          )
%%]]
        ecu <- bcall $ EcuOf modNmNew
        let newPrev = Just (modNmNew, (ecuSrcFilePath ecu, ecuFileLocation ecu))
%%[[50
        cpUpdCU modNmNew $ ecuMbPrevSearchInfo ^= newPrev
%%]]
        return (modNmNew, newPrev)

    -- undefined result
    undefFor modNm = return $ panic $ "BuildFunction.Run.bcall (" ++ show bfun ++ ") undefined result related to " ++ show modNm
%%]

%%[8
-- | Get AST from file, maybe...
-- Wrapper around bcalls.
bASTFromFileMb
  :: forall res m .
     (Typeable res, EHCCompileRunner m)
  => PrevFileSearchKey				--- ^ module name and possibly known path
     -> (AlwaysEq ASTFileTimeHandleHow)	--- ^ how to deal with timestamp
     -> ASTType							--- ^ content type
     -> ASTSuffixKey						--- ^ suffix and content variation
     -> ASTFileTiming					--- ^ timing (i.e. previous or current)
     -> EHCompilePhaseT m (Maybe (res, BRef res))
bASTFromFileMb modSearchKey chkTimeStamp asttype skey tkey = do
%%[[50
    mbtm <- bcall $ ModfTimeOfFile modSearchKey asttype skey tkey
    if isJust mbtm
      then do
%%]]
        -- TBD: faulty...
        -- (ref :: BRef res) <- bcall $ ASTRefFromFile modSearchKey chkTimeStamp asttype skey tkey
        mbref <- bcall $ ASTRefFromFile modSearchKey chkTimeStamp asttype skey tkey
        case mbref of
          Just ref -> fmap (fmap $ \ast -> (ast, ref)) $ bderef' ref
          _ -> return Nothing
%%[[50
      else return Nothing
%%]]
%%]

%%[8
-- | Get possible Hs semantics
bGetHsSemMb
  :: EHCCompileRunner m
  => HsName
  -> EHCompilePhaseT m
       ( Maybe
         ( HSSem.Syn_AGItf				-- all semantics
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
-- | Check whether a particular flow of semantics is allowed, and mark it to have occurred
bAllowFlow :: EHCCompileRunner m => HsName -> Set.Set ASTType -> ASTSemFlowStage -> ASTType -> EHCompilePhaseT m Bool
bAllowFlow modNm forAstTypes flowstage asttype = do
   ecu <- bcall (EcuOf modNm)
   let nyf = ecuHasAlreadyFlowed asttype flowstage ecu
       res = nyf && asttype `Set.member` forAstTypes
   when res $ cpUpdAlreadyFlowIntoCRSI modNm asttype flowstage
   return res
%%]

%%[8
-- | Dereference an indirection into compilation state, possibly with a result, and a setter
bderef'' :: forall res m . (Typeable res, EHCCompileRunner m) => BRef res -> EHCompilePhaseT m (Maybe res, Maybe (res -> EHCompilePhaseT m ()))
bderef'' bref = do
    cr <- get
    let opts = cr ^. crStateInfo ^. crsiOpts
    case bref of
      BRef_CRSI -> return (Just $ cr ^. crStateInfo, Nothing)

%%[[99
      BRef_ExposedPackages -> return (Just $ pkgExposedPackages $ ehcOptPkgDb opts, Nothing)
%%]]

      BRef_ECU modNm -> return (crMbCU modNm cr, Just $ \ecu -> cpUpdCU modNm (const ecu))

      BRef_EHCOpts modNm -> return (Just choose, Nothing)
        where 
%%[[8
              choose = opts
%%][99
              choose = maybe opts id $ crMbCU modNm cr >>= ecuMbOpts
%%]]

      BRef_AST modSearchKey@((modNm,_),_) asttype -> case asthandlerLookup asttype of
          Just (hdlr :: ASTHandler' res) -> 
            case _asthdlrASTLens hdlr of
              Just l -> do
                ecu <- bcall $ EcuOfPrevNameAndPath modSearchKey
                return (ecu ^. l, Just $ \ast -> cpUpdCU modNm $ l ^= Just ast)
              _ -> dflt
          _ -> dflt
        where dflt = return (Nothing, Nothing)

      BRef_ASTFile modSearchKey@((modNm,_),_) asttype skey tkey -> case asthandlerLookup asttype of
          Just (hdlr :: ASTHandler' res) -> case astsuffixLookup skey $ _asthdlrSuffixRel hdlr of
            Just suffinfo -> case Map.lookup tkey $ _astsuffinfoASTLensMp suffinfo of
              Just l -> do
                ecu <- bcall $ EcuOfPrevNameAndPath modSearchKey
                return (ecu ^. l, Just $ \ast -> cpUpdCU modNm $ l ^= Just ast)
              _ -> dflt
            _ -> dflt
          _ -> dflt
        where dflt = return (Nothing, Nothing)

-- | Dereference an indirection into compilation state
bderef' :: forall res m . (Typeable res, EHCCompileRunner m) => BRef res -> EHCompilePhaseT m (Maybe res)
bderef' bref = do
    cpTr TraceOn_BuildFun $ ["deref' " ++ show bref]
    fmap fst $ bderef'' bref

-- | Dereference an indirection into compilation state
bderef :: forall res m . (Typeable res, EHCCompileRunner m) => BRef res -> EHCompilePhaseT m res
bderef bref = do
    fmap (panicJust $ "BuildFunction.Run.deref " ++ show bref) $ bderef' bref
%%]




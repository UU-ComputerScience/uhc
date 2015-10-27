%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile: running
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) module {%{EH}EHC.CompilePhase.Run}
%%]

%%[(8 corerun) import({%{EH}EHC.Common})
%%]
%%[(8 corerun) import({%{EH}EHC.CompileUnit})
%%]
%%[(8 corerun) import({%{EH}EHC.CompileRun})
%%]

%%[(8 corerun) import(Data.Maybe)
%%]
%%[(8 corerun) import(Control.Monad.State)
%%]
%%[(8 corerun) import(Control.Exception)
%%]

%%[(8 corerun) import (UHC.Util.Lens)
%%]
%%[(8 corerun) hs import(qualified UHC.Util.RelMap as Rel)
%%]
%%[(8 corerun) hs import(qualified Data.Set as Set)
%%]

-- debug
%%[(99 corerun) hs import({%{EH}EHC.CompilePhase.Output})
%%]
%%[(99 corerun) import ({%{EH}EHC.ASTHandler}, {%{EH}EHC.ASTHandler.Instances})
%%]
%%[(99 corerun) import(qualified {%{EH}Config} as Cfg)
%%]
%%[(8 corerun) import({%{EH}Base.Trace})
%%]

-- Acccess to Core
%%[(8 corerun) import({%{EH}EHC.CompilePhase.Parsers})
%%]

-- CoreRun
%%[(8 corerun) import({%{EH}Core.ToCoreRun}, {%{EH}CoreRun})
%%]
%%[(8888 corerun) import({%{EH}CoreRun.Pretty})
%%]

-- Running CoreRun
%%[(8 corerun) import({%{EH}CoreRun.Run})
%%]
%%[(8888 corerun) import({%{EH}CoreRun.Run.Val.RunImplStk} as RI)
%%]
%%[(8 corerun) import({%{EH}CoreRun.Run.Val.RunExplStk} as RE)
%%]

-- Build fun
%%[(8 corerun) import({%{EH}EHC.BuildFunction.Run})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Run Core
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) export(cpRunCoreRun)
-- | Run CoreRun.
-- TBD: fix dependence on whole program linked
cpRunCoreRun :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpRunCoreRun modNm = do
    cr <- get
    let (ecu,_,opts,_) = crBaseInfo modNm cr
        mbCore = _ecuMbCore ecu
%%[[8
        (mainMod,impModL) = (modNm,[])
%%][50
%%]]
    cpMsg modNm VerboseNormal "Run Core"
    when (isJust mbCore) $ do
      let mod = cmod2CoreRun $ fromJust mbCore
      res <- liftIO $ catch
        (runCoreRun opts [] mod $ cmodRun opts mod)
        (\(e :: SomeException) -> hFlush stdout >> (return $ Left $ strMsg $ "cpRunCoreRun: " ++ show e))
      either (\e -> cpSetLimitErrsWhen 1 "Run Core(Run) errors" [e])
%%[[8
             (liftIO . putStrLn . show . pp)
%%][100
             (\_ -> return ())
%%]]
             res
%%]

%%[(8 corerun) export(cpRunCoreRun2)
-- | Run CoreRun.
-- 20150130: TBD: does not work yet
cpRunCoreRun2 :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpRunCoreRun2 modNm = do
    cr <- get
    let (ecu,_,opts,_) = crBaseInfo modNm cr
        mbCore = _ecuMbCore ecu
%%[[8
    let (impModL, mainMod) = ([], cmod2CoreRun $ fromJust mbCore)
        hasMain = True
%%][50
    let hasMain = ecuHasMain ecu
    (impModL, mainMod) <- fmap (fromJust . initlast) $
      case crPartitionMainAndImported cr $ map head $ _crCompileOrder cr of
        (_, impl) -> do
          cores <- forM (impl ++ [modNm]) cpGetPrevCore
          return $ flip evalState emptyNm2RefMp $ do
            forM (zip cores [0..]) $ \(cr,modnr) -> do
              prevNm2Ref <- get
              let (m,nm2ref,_) = cmod2CoreRun' opts hasMain (Just modnr) prevNm2Ref cr
              put $ nm2refUnion nm2ref prevNm2Ref
              return m
%%]]
    cpMsg modNm VerboseNormal "Run Core"
    res <- liftIO $ catch
      (runCoreRun opts impModL mainMod $ cmodRun opts mainMod)
      (\(e :: SomeException) -> hFlush stdout >> (return $ Left $ strMsg $ "cpRunCoreRun: " ++ show e))
    either (\e -> cpSetLimitErrsWhen 1 "Run Core(Run) errors" [e])
%%[[8
           (liftIO . putStrLn . show . pp)
%%][100
           (\_ -> return ())
%%]]
           res
%%]

%%[(8 corerun) export(cpRunCoreRun3)
-- | Run CoreRun.
cpRunCoreRun3 :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpRunCoreRun3 modNm = do
    cr <- get
    let (ecu,_,opts,_) = crBaseInfo modNm cr
        mbCore = _ecuMbCore ecu
%%[[8
    let (impModL, mainMod) = ([], cmod2CoreRun $ fromJust mbCore)
        hasMain = True
%%][50
    let hasMain = ecuHasMain ecu
    (impModL, mainMod) <- fmap (fromJust . initlast) $
      case crPartitionMainAndImported cr $ map head $ _crCompileOrder cr of
        (_, impl) -> do
          forM (impl ++ [modNm]) cpGetPrevCoreRun
%%]]
    cpMsg modNm VerboseNormal "Run Core"
    res <- liftIO $ catch
      (runCoreRun opts impModL mainMod $ cmodRun opts mainMod)
      (\(e :: SomeException) -> hFlush stdout >> (return $ Left $ strMsg $ "cpRunCoreRun: " ++ show e))
    either (\e -> cpSetLimitErrsWhen 1 "Run Core(Run) errors" [e])
%%[[8
           (liftIO . putStrLn . show . pp)
%%][100
           (\_ -> return ())
%%]]
           res
%%]

%%[(8 corerun) export(cpRunCoreRun4)
-- | Run CoreRun. Variant for new build plan/driver
-- TBD: fix dependence on whole program linked, in progress as cpRunCoreRun5
cpRunCoreRun4 :: EHCCompileRunner m => BuildGlobal -> PrevFileSearchKey -> ASTBuildPlan -> EHCompilePhaseT m ()
cpRunCoreRun4 bglob modSearchKey@(PrevFileSearchKey {_pfsrchKey=FileSearchKey {_fsrchNm=modNm}}) astplan@(ASTBuildPlan {_astbplPipe=astpipe}) = do
    maybeM (bcall $ ASTPlMb bglob modSearchKey astplan) (return ()) $ \(ASTResult {_astresAST=(mod :: AST_CoreRun)}) -> do
      {-
      crsi <- bcall $ CRSIOfNamePl modSearchKey astplan
      let impModNmL = crsi ^. crsiCoreRunState ^. crcrsiReqdModules
      impModL <- forM impModNmL $ \nm ->
        maybeM (bcall $ ASTPMb (mkPrevFileSearchKeyWithName nm) astpipe)
          (do cpSetLimitErrsWhen 1 "Run Core(Run) errors" [rngLift emptyRange Err_Str $ "Cannot load CoreRun module: " ++ show nm]
              return $ panic "cpRunCoreRun4: not allowed to use AST result!!"
          ) $
          \(ASTResult {_astresAST=(mod :: AST_CoreRun)}) -> return mod
      -}
      opts <- bcall $ EHCOptsOf modSearchKey
      cpMsg modNm VerboseNormal "Run Core (4)"
      res <- liftIO $ catch
        (runCoreRun opts [] mod $ cmodRun opts mod)
        (\(e :: SomeException) -> hFlush stdout >> (return $ Left $ strMsg $ "cpRunCoreRun4: " ++ show e))
      either (\e -> cpSetLimitErrsWhen 1 "Run Core(Run) errors" [e])
%%[[8
             (liftIO . putStrLn . show . pp)
%%][100    
             (\_ -> return ())
%%]]    
             res
%%]



%%[(8 corerun) export(cpRunCoreRun5)
-- | Run CoreRun. Variant for new build plan/driver
-- TBD: fix dependence on whole program linked
cpRunCoreRun5 :: EHCCompileRunner m => BuildGlobal -> PrevFileSearchKey -> ASTBuildPlan -> EHCompilePhaseT m ()
cpRunCoreRun5 bglob modSearchKey@(PrevFileSearchKey {_pfsrchKey=FileSearchKey {_fsrchNm=modNm}}) astplan@(ASTBuildPlan {_astbplPipe=astpipe}) = do
    maybeM (bcall $ ASTPlMb bglob modSearchKey astplan) (return ()) $ \(ASTResult {_astresAST=(mod :: AST_CoreRun)}) -> do
%%[[99
      -- debug
      -- ecu <- bcall $ EcuOf modNm
      -- cpOutputSomeModule (const mod) astHandler'_CoreRun ASTFileContent_Text "-cpRunCoreRun5" Cfg.suffixDotlessOutputTextualCoreRun (ecuModNm ecu)
%%]]
      crsi <- bcall $ CRSIOfNamePl bglob modSearchKey astplan
      let impModNmL = (crsi ^. crsiCoreRunState ^. crcrsiReqdModules) \\ [modNm]
      cpTrPP TraceOn_BldMod $
        [ "cpRunCoreRun5 mod=" >|< modNm
        , "astplan:" >#< astplan
        , "imps:" >-< indent 2 (vlist impModNmL)
        , "crsi nm2ref mods:" >-< indent 2 (vlist $ Set.toList $ Set.map fromJust $ Set.filter isJust $ Set.map hsnQualifier $ Rel.dom $ crsi ^. crsiCoreRunState ^. crcrsiNm2RefMp)
        ]
        -- ++ [ n >#< ppCommas rs | (n,rs) <- Rel.toDomList $ crsi ^. crsiCoreRunState ^. crcrsiNm2RefMp ]
      impModL <- forM impModNmL $ \nm ->
        maybeM (bcall $ ASTPMb bglob (mkPrevFileSearchKeyWithName nm) astpipe)
          (do cpSetLimitErrsWhen 1 "Run Core(Run) errors" [rngLift emptyRange Err_Str $ "Cannot load CoreRun module: " ++ show nm]
              return $ panic "cpRunCoreRun5: not allowed to use AST result!!"
          ) $
          \(ASTResult {_astresAST=(mod :: AST_CoreRun)}) -> return mod
      opts <- bcall $ EHCOptsOf modSearchKey
      cpMsg modNm VerboseNormal "Run Core (5)"
      {-
      res <- liftIO $ catch
        (runCoreRun opts impModL mod $ cmodRun opts mod)
        (\(e :: SomeException) -> hFlush stdout >> (return $ Left $ strMsg $ "cpRunCoreRun5: " ++ show e))
      -}
      res <- liftIO $ runCoreRun2 opts impModL mod $ cmodRun opts mod
      either (\e -> cpSetLimitErrsWhen 1 "Run Core(Run) errors" [e])
%%[[8
             (liftIO . putStrLn . show . pp)
%%][100    
             (\r -> when (CoreRunOpt_PrintResult `Set.member` ehcOptCoreRunOpts opts) $
                      liftIO $ putStrLn $ show $ pp r
             )
%%]]    
             res
%%]


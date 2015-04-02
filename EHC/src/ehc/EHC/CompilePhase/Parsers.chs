%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile Phase building blocks: parsers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

CompilePhase building blocks: parsers

%%[8 module {%{EH}EHC.CompilePhase.Parsers}
%%]

%%[8 import({%{EH}Base.ParseUtils})
%%]
%%[8 import(UU.Parsing, UU.Parsing.Offside)
%%]
%%[8 import(qualified UHC.Util.ScanUtils as ScanUtils, {%{EH}Scanner.Common})
%%]
%%[8 import(UHC.Util.ParseUtils)
%%]
%%[50 import(Control.Exception as CE)
%%]

%%[8 import(UHC.Util.Lens)
%%]

%%[8 import(Control.Monad.Error, Control.Monad.State)
%%]

%%[8 import(qualified Data.Map as Map)
%%]

%%[8 import({%{EH}EHC.Common})
%%]
%%[8 import({%{EH}EHC.CompileUnit})
%%]
%%[8 import({%{EH}EHC.CompileRun})
%%]

-- AST handler
%%[8 import({%{EH}EHC.ASTHandler.Instances})
%%]

-- Build function
%%[8 import({%{EH}EHC.BuildFunction.Run})
%%]

-- EH parser
%%[8 import(qualified {%{EH}EH} as EH, qualified {%{EH}EH.Parser} as EHPrs)
%%]
-- HS parser
%%[8 import(qualified {%{EH}HS} as HS, qualified {%{EH}HS.Parser} as HSPrs)
%%]
-- HI parser
%%[50 import(qualified {%{EH}HI} as HI)
%%]
-- Core parser
%%[(8 corein) import(qualified {%{EH}Core} as Core, qualified {%{EH}Core.Parser} as CorePrs)
%%]
-- CoreRun parser
%%[(8 corerun) import( qualified {%{EH}CoreRun} as CoreRun)
%%]
-- TyCore parser
%%[(50 codegen tycore) import(qualified {%{EH}TyCore} as C)
%%]
-- Grin parser
%%[(8 codegen grinparser) import(qualified {%{EH}GrinCode} as Grin, qualified {%{EH}GrinCode.Parser} as GrinParser)
%%]

-- serialization
%%[50 import(qualified UHC.Util.Binary as Bin, UHC.Util.Serialize)
%%]
-- config
%%[50 import(qualified {%{EH}Config} as Cfg)
%%]
%%[50 import(qualified {%{EH}SourceCodeSig} as Sig)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
-- | Generalization of parser invocation
cpParseWithFPath
  :: ( EHCCompileRunner m
     )
     => ASTHandler' a
     -> EHParseOpts
     -> Maybe FPath														-- possibly overriding FilePath instead of default derived from state for this module name
     -> HsName															-- module name
     -> EHCompilePhaseT m ()
cpParseWithFPath
      astHdlr popts mbFp modNm
 = do { cr <- get
      ; let (_,opts) 	= crBaseInfo' cr
            sopts    	= _asthdlrParseScanOpts astHdlr opts popts
            description = "Parse (" ++ (if ScanUtils.scoLitmode sopts then "Literate " else "") ++ _asthdlrName astHdlr ++ " syntax) of module `" ++ show modNm ++ "`"
            seterrs 	= cpSetLimitErrsWhen 5 description
      ; case _asthdlrParser astHdlr opts popts of
          Just (ASTParser p) -> do
               (res,errs) <- parseWithFPath sopts popts p (maybe (ecuFilePath (crCU modNm cr)) id mbFp)
               cpUpdCU modNm (_asthdlrEcuStore astHdlr res)
               unless (ehpoptsStopAtErr popts) $ seterrs errs
          _ -> seterrs [strMsg $ "No parser for " ++ _asthdlrName astHdlr]
      }
%%]
      
%%[8 export(cpParseEH)
cpParseEH :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpParseEH modNm
  = cpParseWithFPath astHandler'_EH defaultEHParseOpts Nothing modNm
%%]

%%[(8888 grinparser) export(cpParseGrin)
cpParseGrin :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpParseGrin modNm
  = do { cr <- get
       ; let  (ecu,_,opts,fp) = crBaseInfo modNm cr
              fpC     = fpathSetSuff "grin" fp
       ; cpMsg' modNm VerboseALot "Parsing" Nothing fpC
       ; cpParsePlain GrinParser.pModule grinScanOpts ecuStoreGrin "Parse grin" fpC modNm
       ; return ()
       }
%%]

%%[8.cpParseHs export(cpParseHs)
cpParseHs :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpParseHs = cpParseWithFPath astHandler'_HS (defaultEHParseOpts) Nothing
%%]

%%[99 -8.cpParseHs export(cpParseHs)
cpParseHs :: EHCCompileRunner m => Bool -> HsName -> EHCompilePhaseT m ()
cpParseHs litmode = cpParseWithFPath astHandler'_HS (defaultEHParseOpts {ehpoptsLitMode=litmode}) Nothing
%%]

%%[50.cpParseHsImport export(cpParseHsImport)
cpParseHsImport :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpParseHsImport = cpParseWithFPath astHandler'_HS (defaultEHParseOpts {ehpoptsStopAtErr=True, ehpoptsForImport=True}) Nothing
%%]

%%[99 -50.cpParseHsImport export(cpParseHsImport)
cpParseHsImport :: EHCCompileRunner m => Bool -> HsName -> EHCompilePhaseT m ()
cpParseHsImport litmode = cpParseWithFPath astHandler'_HS (defaultEHParseOpts {ehpoptsStopAtErr=True, ehpoptsLitMode=litmode, ehpoptsForImport=True}) Nothing
%%]

%%[(8 corein) export(cpParseCoreWithFPath)
cpParseCoreWithFPath :: EHCCompileRunner m => Maybe FPath -> HsName -> EHCompilePhaseT m ()
cpParseCoreWithFPath = cpParseWithFPath astHandler'_Core (defaultEHParseOpts)
%%]

%%[(8 corerun) export(cpParseCoreRunWithFPath)
cpParseCoreRunWithFPath :: EHCCompileRunner m => Maybe FPath -> HsName -> EHCompilePhaseT m ()
cpParseCoreRunWithFPath = cpParseWithFPath astHandler'_CoreRun (defaultEHParseOpts)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: Binary reading
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
cpDecodeHIInfo :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpDecodeHIInfo modNm
  = do { cr <- get
       ; let  (ecu,_,opts,fp) = crBaseInfo modNm cr
              fpH     = asthdlrMkInputFPath astHandler'_HI opts ecu (ASTFileContent_Binary, ASTFileUse_Cache) modNm fp
       ; cpMsg' modNm VerboseALot "Decoding" Nothing fpH
       ; hiinfo <- liftIO $
           CE.catch (getSGetFile (fpathToStr fpH) (HI.sgetHIInfo opts))
                    (\(_ :: SomeException) -> return $ HI.emptyHIInfo {HI.hiiValidity = HI.HIValidity_Absent})
       ; when (ehcOptVerbosity opts > VerboseALot)
              (do { liftIO $ putPPLn (pp hiinfo)
                  })
%%[[99
       ; let canCompile = ecuCanCompile ecu
%%]]
       ; case HI.hiiValidity hiinfo of
%%[[99
           HI.HIValidity_WrongMagic | not canCompile
             -> cpSetLimitErrsWhen 1 "Read HI"
                  [rngLift emptyRange Err_WrongMagic
                     (show modNm)
                     (fpathToStr fpH)
                  ]
           HI.HIValidity_Inconsistent | not canCompile
             -> cpSetLimitErrsWhen 1 "Read HI (of previous compile) of module"
                  [rngLift emptyRange Err_InconsistentHI
                     (show modNm)
                     (fpathToStr fpH)
                     [Sig.timestamp, Cfg.installVariant opts, show $ ehcOptTarget opts, show $ ehcOptTargetFlavor opts]
                     [HI.hiiSrcTimeStamp hiinfo   , HI.hiiCompiler hiinfo  , show $ HI.hiiTarget hiinfo, show $ HI.hiiTargetFlavor hiinfo]
                  ]
%%]]
           _ -> cpUpdCU modNm (ecuStorePrevHIInfo {-- $ HI.hiiPostCheckValidity opts -} hiinfo)
       }
%%]

%%[50
-- | Decode from serialized file and store result in the compileunit for the module modNm
cpDecode :: (EHCCompileRunner m, Serialize x) => Maybe String -> EcuUpdater x -> HsName -> EHCompilePhaseT m ()
cpDecode mbSuff store modNm
  = do { cr <- get
       ; let  (ecu,_,opts,fp) = crBaseInfo modNm cr
              fpC     = maybe id fpathSetSuff mbSuff fp
       ; cpMsg' modNm VerboseALot ("Decoding (" ++ show mbSuff ++ ")") Nothing fpC
       ; x <- liftIO $ getSerializeFile (fpathToStr fpC)
       ; cpUpdCU modNm (store x)
       }
%%]

%%[50 export(cpDecode'')
-- | Decode from serialized file and store result in the compileunit for the module modNm, return True if decoding could be done
cpDecode'' :: EHCCompileRunner m => ASTHandler' ast -> ASTSuffixKey -> ASTFileTiming -> HsName -> EHCompilePhaseT m Bool
cpDecode'' astHdlr skey tkey modNm
  = do { cr <- get
       ; let (ecu,_,opts,fp)    = crBaseInfo modNm cr
             mbi@(~(Just info)) = astsuffixLookup skey $ _asthdlrSuffixRel astHdlr
             mbl@(~(Just lens)) = Map.lookup tkey $ _astsuffinfoASTLensMp info
             -- fpC                = fpathSetSuff (_astsuffinfoSuff info) fp
             fpC                = asthdlrMkInputFPath astHdlr opts ecu skey modNm fp
       ; if isJust mbi && isJust mbl
         then do
           cpMsg' modNm VerboseALot "Decoding" Nothing fpC
           mbx@(~(Just x)) <- liftIO $ _asthdlrGetSerializeFileIO astHdlr opts fpC
           if isJust mbx
             then do
               let errs = _asthdlrPostInputCheck astHdlr opts ecu modNm fpC x
               if null errs
                 then do 
                   cpUpdCU modNm (lens ^= Just x)
                   return True
                 else do
                   cpSetLimitErrsWhen 1 ("Decode " ++ _asthdlrName astHdlr) errs
                   return False
             else return False
         else return False
       }

-- | Decode from serialized file and store result in the compileunit for the module modNm
cpDecode' :: EHCCompileRunner m => ASTHandler' ast -> ASTSuffixKey -> ASTFileTiming -> HsName -> EHCompilePhaseT m ()
cpDecode' astHdlr skey tkey modNm = do
    okDecode <- cpDecode'' astHdlr skey tkey modNm
    unless okDecode $ cpSetLimitErrsWhen 1 ("Decode " ++ _asthdlrName astHdlr) [strMsg $ "No decoder/lens for " ++ _asthdlrName astHdlr ++ " (" ++ show skey ++ "/" ++ show tkey ++ ")"]
%%]

%%[(50 codegen grin) export(cpDecodeGrin)
cpDecodeGrin :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpDecodeGrin = cpDecode (Just "grin") ecuStoreGrin
%%]

%%[(50 codegen) export(cpDecodeCore)
cpDecodeCore :: EHCCompileRunner m => Maybe String -> HsName -> EHCompilePhaseT m ()
-- cpDecodeCore = cpDecode' astHdlr skey timing
cpDecodeCore suff = cpDecode suff ecuStoreCore
%%]

%%[(50 corerun) export(cpDecodeCoreRun)
cpDecodeCoreRun :: EHCCompileRunner m => Maybe String -> HsName -> EHCompilePhaseT m ()
cpDecodeCoreRun suff = cpDecode suff ecuStoreCore
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: on top of parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(cpGetPrevHI)
cpGetPrevHI :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpGetPrevHI modNm
  = do { cr <- get
       ; cpMsg modNm VerboseDebug "cpGetPrevHI"
       ; let  ecu        = crCU modNm cr
       -- ; when (isJust (ecuMbHITime ecu))
       --        (cpParseHI modNm)
       ; when (isJust (_ecuMbHIInfoTime ecu)) $
              -- cpDecodeHIInfo modNm
              cpDecode' astHandler'_HI (ASTFileContent_Binary, ASTFileUse_Cache) ASTFileTiming_Prev modNm
       }
%%]

%%[(50 codegen) export(cpGetPrevCore)
cpGetPrevCore :: EHCCompileRunner m => HsName -> EHCompilePhaseT m Core.CModule
cpGetPrevCore modNm
  = do { cr <- get
       ; cpMsg modNm VerboseDebug "cpGetPrevCore"
       -- ; let  ecu    = crCU modNm cr
       -- ; when (isJust (_ecuMbCoreTime ecu) && isNothing (_ecuMbCore ecu)) $
              -- cpDecodeCore (Just Cfg.suffixDotlessBinaryCore) modNm
              -- cpDecode' astHandler'_Core (ASTFileContent_Binary, ASTFileUse_Cache) ASTFileTiming_Prev modNm
       -- ; fmap (fromJust . _ecuMbCore) $ gets (crCU modNm)
       ; bcall $ ASTFromFile (modNm,Nothing) ASTType_Core (ASTFileContent_Binary, ASTFileUse_Cache) ASTFileTiming_Prev
       }
%%]

%%[(50 corerun) export(cpGetPrevCoreRun)
cpGetPrevCoreRun :: EHCCompileRunner m => HsName -> EHCompilePhaseT m CoreRun.Mod
cpGetPrevCoreRun modNm
  = do { cr <- get
       ; cpMsg modNm VerboseDebug "cpGetPrevCoreRun"
       ; let  ecu    = crCU modNm cr
       ; when (isJust (_ecuMbCoreRunTime ecu) && isNothing (_ecuMbCoreRun ecu))
              (cpDecodeCoreRun (Just Cfg.suffixDotlessBinaryCoreRun) modNm)
       ; fmap (fromJust . _ecuMbCoreRun) $ gets (crCU modNm)
       }
%%]

%%[(50 codegen grin) export(cpGetPrevGrin)
cpGetPrevGrin :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpGetPrevGrin modNm
  = do { cr <- get
       ; cpMsg modNm VerboseDebug "cpGetPrevGrin"
       ; let  ecu    = crCU modNm cr
       ; when (isJust (_ecuMbGrinTime ecu) && isNothing (_ecuMbGrin ecu))
              (cpDecodeGrin modNm) -- (cpParseGrin modNm)
       }
%%]



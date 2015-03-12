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
%%[99 import(Control.Exception as CE)
%%]

%%[8 import(Control.Monad.Error, Control.Monad.State)
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

%%[8888
-- | Generalization of parser invocation
cpParseWithFPath
  :: (EHCCompileRunner m, PP msg)
     => ASTHandler a
     -> (ScanUtils.ScanOpts -> FilePath -> Handle -> IO inp)			-- tokenize/scan file
     -> (inp -> (a,[msg]))												-- parse tokens
     -> ([Err] -> EHCompilePhaseT m out)								-- monadic output from errors
     -> Maybe FPath														-- possibly overriding FilePath instead of default derived from state for this module name
     -> HsName															-- module name
     -> EHCompilePhaseT m out
cpParseWithFPath
      astHdlr
      scan parse seterrs
      mbFp modNm
 = do { cr <- get
      ; let (_,opts) = crBaseInfo' cr
      ; (fn,fh) <- liftIO $ openFPath (maybe (ecuFilePath (crCU modNm cr)) id mbFp) ReadMode False
      ; tokens  <- liftIO $ scan (_asthdlrParseScanOpts astHdlr opts defaultEHParseOpts) fn fh
      ; let (res,msgs) = parse tokens
            errs       = map (rngLift emptyRange mkPPErr) msgs
      ; cpUpdCU modNm (_asthdlrEcuStore astHdlr res)
      ; liftIO $ res `seq` hClose fh
      ; seterrs errs
      }

-- cpParseOffsideWithFPath :: HSPrs.HSParser a -> ScanUtils.ScanOpts -> EcuUpdater a -> String -> Maybe FPath -> HsName -> EHCompilePhase ()
-- `HSPrs.HSParser a' is a type synonym for `OffsideParser [Token] Pair Token (Maybe Token) a' but is not expanded as such...
cpParseOffsideWithFPath
  :: EHCCompileRunner m
     => ASTHandler a
     -> OffsideParser [Token] Pair Token (Maybe Token) a
     -> String
     -> Maybe FPath
     -> HsName
     -> EHCompilePhaseT m ()
cpParseOffsideWithFPath astHdlr parser description mbFp modNm
  = cpParseWithFPath astHdlr offsideScanHandle (parseOffsideToResMsgs parser) (cpSetLimitErrsWhen 5 description) mbFp modNm
%%]
      
%%[8
-- | Generalization of parser invocation
cpParseWithFPath'
  :: ( EHCCompileRunner m
     )
     => ASTHandler a
     -> EHParseOpts
     -> Maybe FPath														-- possibly overriding FilePath instead of default derived from state for this module name
     -> HsName															-- module name
     -> EHCompilePhaseT m ()
cpParseWithFPath'
      astHdlr popts mbFp modNm
 = do { cr <- get
      ; let (_,opts) 	= crBaseInfo' cr
            sopts    	= _asthdlrParseScanOpts astHdlr opts popts
            description = "Parse (" ++ (if ScanUtils.scoLitmode sopts then "Literate " else "") ++ _asthdlrName astHdlr ++ " syntax) of module"
            seterrs 	= cpSetLimitErrsWhen 5 description
      ; case _asthdlrParser astHdlr opts popts of
          Just (ASTParser p) -> do
               (res,errs) <- parseWithFPath sopts popts p (maybe (ecuFilePath (crCU modNm cr)) id mbFp)
               cpUpdCU modNm (_asthdlrEcuStore astHdlr res)
               unless (ehpoptsStopAtErr popts) $ seterrs errs
          _ -> seterrs [strMsg $ "No parser for " ++ _asthdlrName astHdlr]
      }
%%]
      
%%[8888 export(cpParseOffside)
cpParseOffside
  :: EHCCompileRunner m
  => ASTHandler a
  -> HSPrs.HSParser a
  -> String
  -> HsName
  -> EHCompilePhaseT m ()
cpParseOffside astHdlr parser description modNm
 = cpParseOffsideWithFPath astHdlr parser description Nothing modNm
%%]

%%[8 export(cpParseEH)
cpParseEH :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpParseEH modNm
  = cpParseWithFPath' astHandler_EH defaultEHParseOpts Nothing modNm
  -- = cpParseOffside astHandler_EH EHPrs.pAGItf "Parse (EH syntax) of module"
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
cpParseHs modNm
  = cpParseWithFPath' astHandler_HS (defaultEHParseOpts) Nothing modNm
{-
cpParseHs modNm
  = do { cr <- get
       ; let (_,opts) = crBaseInfo' cr
       ; cpParseOffside astHandler_HS (HSPrs.pAGItf opts) "Parse (Haskell syntax) of module" modNm
       }
-}
%%]

%%[99 -8.cpParseHs export(cpParseHs)
cpParseHs :: EHCCompileRunner m => Bool -> HsName -> EHCompilePhaseT m ()
cpParseHs litmode modNm
  = cpParseWithFPath' astHandler_HS (defaultEHParseOpts {ehpoptsLitMode=litmode}) Nothing modNm
{-
cpParseHs litmode modNm
  = do { cr <- get
       ; let  (ecu,_,opts,_) = crBaseInfo modNm cr
       ; cpParseOffsideWithFPath
           (astHandler_HS {_asthdlrParseScanOpts = \opts popts -> (_asthdlrParseScanOpts astHandler_HS opts popts) {ScanUtils.scoLitmode = litmode}})
           (HSPrs.pAGItf opts)
           ("Parse (" ++ (if litmode then "Literate " else "") ++ "Haskell syntax) of module")
           Nothing modNm
       }
-}
%%]

%%[5050 export(cpParseOffsideStopAtErr)
cpParseOffsideStopAtErr :: EHCCompileRunner m => HSPrs.HSParser a -> ScanUtils.ScanOpts -> EcuUpdater a -> HsName -> EHCompilePhaseT m ()
cpParseOffsideStopAtErr parser scanOpts store modNm
 = do { cr <- get
      ; (fn,fh) <- liftIO $ openFPath (ecuFilePath (crCU modNm cr)) ReadMode False
      ; tokens  <- liftIO $ offsideScanHandle scanOpts fn fh
      ; let (res,_) = parseOffsideToResMsgsStopAtErr parser tokens
      ; cpUpdCU modNm (store res)
      }
%%]

%%[50.cpParseHsImport export(cpParseHsImport)
cpParseHsImport :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpParseHsImport modNm
  = cpParseWithFPath' astHandler_HS (defaultEHParseOpts {ehpoptsStopAtErr=True, ehpoptsForImport=True}) Nothing modNm
{-
  = do { cr <- get
       ; let (_,opts) = crBaseInfo' cr
       ; cpParseOffsideStopAtErr (HSPrs.pAGItfImport opts) (hsScanOpts opts) ecuStoreHS modNm
       }
-}
%%]

%%[99 -50.cpParseHsImport export(cpParseHsImport)
cpParseHsImport :: EHCCompileRunner m => Bool -> HsName -> EHCompilePhaseT m ()
cpParseHsImport litmode modNm
  = cpParseWithFPath' astHandler_HS (defaultEHParseOpts {ehpoptsStopAtErr=True, ehpoptsLitMode=litmode, ehpoptsForImport=True}) Nothing modNm
{-
  = do { cr <- get
       ; let (_,opts) = crBaseInfo' cr
       ; cpParseOffsideStopAtErr (HSPrs.pAGItfImport opts) ((hsScanOpts opts) {ScanUtils.scoLitmode = litmode}) ecuStoreHS modNm
       }
-}
%%]

%%[(8 corein) export(cpParseCoreWithFPath)
cpParseCoreWithFPath :: EHCCompileRunner m => Maybe FPath -> HsName -> EHCompilePhaseT m ()
cpParseCoreWithFPath = cpParseWithFPath' astHandler_Core (defaultEHParseOpts)
{-
  = do (_,opts) <- gets crBaseInfo'
       cpParseWithFPath astHandler_Core scanHandle (parseToResMsgs $ CorePrs.pCModule opts) (cpSetLimitErrsWhen 5 "Parse Core") mbFp modNm
-}
%%]

%%[(8 corein) export(cpParseCoreRunWithFPath)
cpParseCoreRunWithFPath :: EHCCompileRunner m => Maybe FPath -> HsName -> EHCompilePhaseT m ()
cpParseCoreRunWithFPath = cpParseWithFPath' astHandler_CoreRun (defaultEHParseOpts)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: Binary reading
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(cpDecodeHIInfo)
cpDecodeHIInfo :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpDecodeHIInfo modNm
  = do { cr <- get
       ; let  (ecu,_,opts,fp) = crBaseInfo modNm cr
%%[[50
              fpH     = fpathSetSuff "hi" fp
%%][99
              -- if outputdir is specified, use that location to possibly read hi from.
              fpH     = mkInOrOutputFPathFor (InputFrom_Loc $ ecuFileLocation ecu) opts modNm fp "hi"
%%]]
       ; cpMsg' modNm VerboseALot "Decoding" Nothing fpH
       ; hiinfo <- liftIO $
           CE.catch (do { i <- getSGetFile (fpathToStr fpH) (HI.sgetHIInfo opts)
                               -- getSerializeFile (fpathToStr fpH)
                               -- Bin.getBinaryFPath fpH
                        ; return i
                        })
                    (\(_ :: SomeException) -> return $ HI.emptyHIInfo {HI.hiiValidity = HI.HIValidity_Absent})
       ; when (ehcOptVerbosity opts > VerboseALot)
              (do { liftIO $ putPPLn (pp hiinfo)
                  })
%%[[99
       ; let canCompile = crModCanCompile modNm cr
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
       ; cpMsg' modNm VerboseALot "Decoding" Nothing fpC
       ; x <- liftIO $ getSerializeFile (fpathToStr fpC)
       ; cpUpdCU modNm (store x)
       }
%%]

%%[(50 codegen grin) export(cpDecodeGrin)
cpDecodeGrin :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpDecodeGrin = cpDecode (Just "grin") ecuStoreGrin
%%]

%%[(50 codegen) export(cpDecodeCore)
cpDecodeCore :: EHCCompileRunner m => Maybe String -> HsName -> EHCompilePhaseT m ()
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
       ; when (isJust (ecuMbHIInfoTime ecu))
              (cpDecodeHIInfo modNm)
       }
%%]

%%[(50 codegen) export(cpGetPrevCore)
cpGetPrevCore :: EHCCompileRunner m => HsName -> EHCompilePhaseT m Core.CModule
cpGetPrevCore modNm
  = do { cr <- get
       ; cpMsg modNm VerboseDebug "cpGetPrevCore"
       ; let  ecu    = crCU modNm cr
       ; when (isJust (ecuMbCoreTime ecu) && isNothing (ecuMbCore ecu))
              (cpDecodeCore (Just Cfg.suffixDotlessBinaryCore) modNm)
              -- (cpParseCore modNm)
       ; fmap (fromJust . ecuMbCore) $ gets (crCU modNm)
       }
%%]

%%[(50 corerun) export(cpGetPrevCoreRun)
cpGetPrevCoreRun :: EHCCompileRunner m => HsName -> EHCompilePhaseT m CoreRun.Mod
cpGetPrevCoreRun modNm
  = do { cr <- get
       ; cpMsg modNm VerboseDebug "cpGetPrevCoreRun"
       ; let  ecu    = crCU modNm cr
       ; when (isJust (ecuMbCoreRunTime ecu) && isNothing (ecuMbCoreRun ecu))
              (cpDecodeCoreRun (Just Cfg.suffixDotlessBinaryCoreRun) modNm)
       ; fmap (fromJust . ecuMbCoreRun) $ gets (crCU modNm)
       }
%%]

%%[(50 codegen grin) export(cpGetPrevGrin)
cpGetPrevGrin :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
cpGetPrevGrin modNm
  = do { cr <- get
       ; cpMsg modNm VerboseDebug "cpGetPrevGrin"
       ; let  ecu    = crCU modNm cr
       ; when (isJust (ecuMbGrinTime ecu) && isNothing (ecuMbGrin ecu))
              (cpDecodeGrin modNm) -- (cpParseGrin modNm)
       }
%%]



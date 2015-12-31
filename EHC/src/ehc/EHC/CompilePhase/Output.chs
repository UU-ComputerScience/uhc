%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile output generation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Output generation, on stdout or file

%%[8 module {%{EH}EHC.CompilePhase.Output}
%%]

-- general imports
%%[8 import(UHC.Util.Lens, qualified UHC.Util.FastSeq as Seq)
%%]
%%[8 import(qualified Data.Map as Map, qualified Data.Set as Set)
%%]
%%[50 import(UHC.Util.Time, System.Directory)
%%]

%%[8 import(Control.Monad.State)
%%]

%%[8 import(UHC.Util.Lens)
%%]

%%[8 import({%{EH}EHC.Common})
%%]
%%[8 import({%{EH}EHC.CompileUnit})
%%]
%%[8 import({%{EH}EHC.CompileRun.Base})
%%]

%%[8 import(qualified {%{EH}Config} as Cfg)
%%]

%%[50 import(qualified {%{EH}SourceCodeSig} as Sig)
%%]

-- AST handling
%%[8 import({%{EH}EHC.ASTHandler.Instances})
%%]

-- EH semantics
%%[99 import(qualified {%{EH}EH.Main} as EHSem)
%%]

-- HI syntax and semantics
%%[50 import(qualified {%{EH}HI} as HI)
%%]

-- Core semantics
-- TBD: this depends on grin gen, but should also be available for Core, so in a CoreXXXSem
%%[(8 core) import(qualified {%{EH}Core.ToGrin} as Core2GrSem)
%%]
%%[(8 codegen) import({%{EH}Core.Trf.EraseExtractTysigCore})
%%]

-- Cmm output
%%[(8888 codegen cmm) import({%{EH}Cmm} as Cmm,{%{EH}Cmm.ToC}(cmmMod2C), {%{EH}Cmm.Pretty})
%%]

-- serialization
%%[50 import(qualified UHC.Util.Binary as Bin, UHC.Util.Serialize)
%%]
-- for debugging only
%%[50 import({%{EH}Gam})
%%]

-- module admin
%%[50 import({%{EH}Module.ImportExport})
%%]

-- gam related utils
%%[(99 hmtyinfer ) import({%{EH}Gam.Utils})
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: abstract writing of output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8888
-- | Abstraction for writing a module to output with variation in suffices, old (<= 20150302) version
cpOutputSomeModules'
  ::    EHCCompileRunner m => 
        (EHCOpts -> EHCompileUnit -> FPath -> FilePath -> mod -> IO ())
     -> (EHCOpts -> HsName -> FPath -> String -> FPath)
     -> (Int -> String -> String)
     -> String
     -> HsName
     -> [(String,mod)]
     -> EHCompilePhaseT m [FPath]
cpOutputSomeModules' write mkfp mknmsuff suff modNm mods = do
    cr <- get
    let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
    forM (zip [1..] mods) $ \(nr,(nmsuff,mod)) -> do
      let fpC     = mkfp opts modNm fp (suff ++ mknmsuff nr nmsuff) -- for now nmsuff after suff, but should be inside name
          fnC     = fpathToStr fpC
      liftIO $ unless (ecuSrcHasSuffix suff ecu) $ do
        fpathEnsureExists fpC
        write opts ecu fpC fnC mod
      return fpC
%%]

%%[8 export(cpOutputSomeModules)
-- | Abstraction for writing some module to output with variation in suffices
cpOutputSomeModules
  :: EHCCompileRunner m
     => Maybe EHCOpts
     -> ASTHandler' mod -- (EHCOpts -> EHCompileUnit -> FPath -> FilePath -> mod -> IO Bool)
     -> ASTFileContent
     -> (Int -> String -> String)
     -> String
     -> HsName
     -> [(String,mod)]
     -> EHCompilePhaseT m [Maybe FPath]
cpOutputSomeModules mbOpts astHdlr how mknmsuff suff modNm mods = do
    cr <- get
    let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
    forM (zip [1..] mods) $ \(nr,(nmsuff,mod)) -> do
      let fpC     = _asthdlrMkOutputFPath astHdlr opts modNm fp (suff ++ mknmsuff nr nmsuff) -- for now nmsuff after suff, but should be inside name
          fnC     = fpathToStr fpC
      okWrite <- if ecuSrcHasSuffix suff ecu
        then return False
        else liftIO $ do
          -- fpathEnsureExists fpC
          asthdlrOutputIO astHdlr how opts ecu modNm fpC fnC mod
      return $ if okWrite then Just fpC else Nothing
%%]

%%[8 export(cpOutputSomeModule)
-- | Abstraction for writing a module, using cpOutputSomeModules
cpOutputSomeModule
  :: EHCCompileRunner m
     => (EHCompileUnit -> mod)
     -> ASTHandler' mod
     -> ASTFileContent
     -> String
     -> String
     -> HsName
     -> EHCompilePhaseT m (Maybe FPath)
cpOutputSomeModule getMod astHdlr how nmsuff suff modNm
  =  do  {  cr <- get
         ;  let  (ecu,_,_,_) = crBaseInfo modNm cr
                 mod    = getMod ecu
         ;  cpMsg modNm VerboseALot $ "Emit " ++ _asthdlrName astHdlr
         ;  fmap head $ cpOutputSomeModules Nothing astHdlr how (\_ nm -> nm) suff modNm [(nmsuff,mod)]
         }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: specific output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(cpOutputCore)
cpOutputCore :: EHCCompileRunner m => ASTFileContent -> String -> String -> HsName -> EHCompilePhaseT m FPath
cpOutputCore how nmsuff suff modNm =
    fmap (panicJust "cpOutputCore.cpOutputSomeModule") $
      cpOutputSomeModule (^. ecuCore) astHandler'_Core how nmsuff suff modNm
%%]

%%[(8 codegen grin) export(cpOutputGrin)
cpOutputGrin :: EHCCompileRunner m => ASTFileContent -> String -> HsName -> EHCompilePhaseT m FPath
cpOutputGrin how nmsuff modNm =
    fmap (panicJust "cpOutputGrin.cpOutputSomeModule") $
      cpOutputSomeModule (^. ecuGrin) astHandler'_Grin how nmsuff "grin" modNm
%%]

%%[(8888 cmm) export(cpOutputCmmModules)
cpOutputCmmModules
  :: EHCCompileRunner m => 
        ASTFileContent
     -> (Int -> String -> String)
     -> String -> HsName
     -> [(String,AST_Cmm)]
     -> EHCompilePhaseT m [FPath]
cpOutputCmmModules _ mknmsuff suff modNm mods
  = cpOutputSomeModules' write mkOutputFPath mknmsuff suff modNm mods
  where write opts _ fpC fnC cmmMod = do
          putPPFPath fpC (ppCmmModule cmmMod) 100
%%]

%%[(8888 cmm) export(cpOutputCmm)
cpOutputCmm :: EHCCompileRunner m => ASTFileContent -> String -> HsName -> EHCompilePhaseT m FPath
cpOutputCmm binary suff modNm
  =  do  { cr <- get
         ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                mbCmm  = _ecuMbCmm ecu
                cmm    = panicJust "cpOutputCmm" mbCmm
         ; cpMsg modNm VerboseALot "Emit Cmm"
         ; fmap head $ cpOutputCmmModules binary (\_ nm -> nm) "cmm" modNm [(suff,cmm)]
         }
%%]

%%[(8 codegen grin) export(cpOutputByteCodeC)
cpOutputByteCodeC :: EHCCompileRunner m => String -> HsName -> EHCompilePhaseT m ()
cpOutputByteCodeC suff modNm
  =  do  {  cr <- get
         ;  let  (ecu,_,opts,fp) = crBaseInfo modNm cr
                 bc       = panicJust "cpOutputByteCodeC bytecode" $ ecuMbBytecodeSem ecu
                 fpC      = mkOutputFPath opts modNm fp suff
%%[[(8 cmm)
                 cmm      = panicJust "cpOutputByteCodeC cmm" $ _ecuMbCmm ecu
                 fpCmm    = mkOutputFPath opts modNm fp (suff ++ "-cmm")
%%]]
         ;  cpMsg' modNm VerboseALot "Emit ByteCode C" Nothing fpC     -- '
%%[[99
         ;  cpRegisterFilesToRm [fpC]
%%]]
         ;  liftIO $ putPPFPath fpC bc 150
%%[[(8 cmm)
         -- 20111220: temporary, until Cmm is main path
         ;  when (ehcOptPriv opts)
                 (do { let cmmPP = cmmMod2C opts cmm
                     ; liftIO $ putPPFPath fpCmm cmmPP 150
                     })
%%]]
         }
%%]

%%[50 export(cpOutputHI)
cpOutputHI :: EHCCompileRunner m => String -> HsName -> EHCompilePhaseT m ()
cpOutputHI suff modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mmi    = panicJust "cpOutputHI.crsiModMp" $ Map.lookup modNm $ crsiModMp crsi
                 hii1   = ecu ^. ecuHIInfo
                 impNmS = ecuImpNmS ecu
                 hii2   = hii1 { HI.hiiValidity             = HI.HIValidity_Ok
                               , HI.hiiModuleNm             = modNm
                               , HI.hiiExps                 = mmiExps       mmi
                               , HI.hiiHiddenExps           = mmiHiddenExps mmi
                               , HI.hiiHasMain              = ecuHasMain ecu
                               , HI.hiiTarget               = ehcOptTarget opts
                               , HI.hiiTargetFlavor         = ehcOptTargetFlavor opts
                               , HI.hiiSrcTimeStamp         = Sig.timestamp
                               , HI.hiiSrcSig               = Sig.sig
                               , HI.hiiSrcVersionMajor      = Cfg.verMajor Cfg.version
                               , HI.hiiSrcVersionMinor      = Cfg.verMinor Cfg.version
                               , HI.hiiSrcVersionMinorMinor = Cfg.verMinorMinor Cfg.version
                               , HI.hiiSrcVersionSvn        = Cfg.verSvnRevision Cfg.version
                               , HI.hiiCompileFlags         = optsDiscrRecompileRepr opts
                               , HI.hiiCompiler             = Cfg.installVariant opts
                               , HI.hiiTransClosedUsedModMp = Map.unions $
                                                                Map.singleton modNm impNmS : [ ecuTransClosedUsedModMp $ crCU m cr | m <- Set.toList impNmS ]
                               , HI.hiiTransClosedOrphanModS= Set.unions $
                                                                [ Set.unions [if ecuIsOrphan me then Set.singleton m else Set.empty, ecuTransClosedOrphanModS me]
                                                                | m <- Set.toList impNmS
                                                                , let me = crCU m cr
                                                                ]
                               }
%%[[50
                 hii3   = hii2
%%][9999
                 ehInh  = crsi ^. crsiEHInh
                 hii3 = HI.hiiIncludeCacheOfImport (ecuAnHIInfo . flip crCU cr) (mentrelFilterMpExtendViaValGam modNm (EHSem.valGam_Inh_AGItf ehInh) (ecuUsedNames ecu)) hii2
                 -- hii3   = hii2
%%]]
                 fpH    = mkOutputFPath opts modNm fp suff
                 fnH    = fpathToStr fpH
         ;  cpMsg modNm VerboseALot "Emit HI"
         ;  hiExists <- liftIO $ doesFileExist fnH
         ;  when (hiExists)
                 (liftIO $ removeFile fnH)
         ;  when (ehcOptVerbosity opts > VerboseALot)
                 (do { liftIO $ putPPLn ("hii3: " >#< hii3)
%%[[(99 codegen hmtyinfer)
                     ; liftIO $ putPPLn ("orph: " >#< vlist [ m >#< (fmap Set.toList $ HI.hiiMbOrphan $ me ^. ecuHIInfo) | m <- Set.toList impNmS, let me = crCU m cr ])
%%]]
%%[[99
                     ; liftIO $ putPPLn ("used nms: " >#< (pp $ show $ ecuUsedNames ecu))
%%]]
                     })
{-
         ;  liftIO $ do { fpathEnsureExists fpH
                        ; putSerializeFile fnH hii3
                        }
-}
         ;  liftIO $ asthdlrOutputIO astHandler'_HI ASTFileContent_Binary opts ecu modNm fpH fnH hii3
         ;  now <- liftIO $ getClockTime
         ;  cpUpdCU modNm ( ecuStoreHIInfoTime now
%%[[99
                          . ecuStoreHIInfo hii3
%%]]
                          )
         }

%%]



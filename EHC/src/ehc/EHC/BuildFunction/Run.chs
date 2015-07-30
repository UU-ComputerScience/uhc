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
%%[8 import ({%{EH}EHC.Common}, {%{EH}EHC.CompileRun}, {%{EH}EHC.CompileUnit}, {%{EH}EHC.FileSuffMp})
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

-- EH semantics
%%[8 import(qualified {%{EH}EH} as EH)
%%]
%%[8 import(qualified {%{EH}EH.Main} as EHSem)
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
          CRSIWithCompileOrder modNmFrom compileOrder -> do
               case compileOrder of
                 -- the initial state, for now just what the state happens to be
                 [] -> return ()
                 
                 -- flow/recurse over compile order
                 ([modNm]:remCompilerOrder) -> do
                   crsi <- bcall $ CRSIWithCompileOrder modNm remCompilerOrder
                   return ()
                 
                 -- mutual recursive modules
                 o -> cpSetLimitErrs 1 "compilation run" [rngLift emptyRange Err_MutRecModules mutRecL]
                   where mutRecL = filter ((> 1) . length) o

               bcall $ CRSI

          CRSIWithImps ((modNm,_),mbPrev) imps -> do
               impsmp <- bcall $ ImportsRecursiveWithImps mbPrev imps
               let compileOrder = reverse $ scc [ (n, Set.toList i) | (n,i) <- Map.toList impsmp ]
               cpTr TraceOn_BuildSccImports $ [show modNm ++ " " ++ show imps] ++ [show compileOrder]
               bcall $ CRSIWithCompileOrder modNm compileOrder
%%]]

          CRSIOfName modSearchKey -> do
%%[[8
               bcall $ CRSI
%%][50
               ecu <- bcall $ EcuOfPrevNameAndPath modSearchKey
               (modNm', imps) <- bcall $ ImportsOfName modSearchKey
               bcall $ CRSIWithImps (mkPrevFileSearchKeyWithNameMbPrev modNm' (_ecuMbPrevSearchInfo ecu)) imps
%%]]

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
               (mbEcu,_) <- bderef' $ BRef_ECU modNm
               -- For interaction with old build system, in case of already existing ecu and no dependency on file, take that one
               case mbEcu of
                 Just ecu -> bcall $ EcuOf modNm
                 _        -> bcall $ EcuOfNameAndPath (modNm, ASTFileNameOverride_AsIs)
           
          EcuOfNameAndPath mf -> do
               bcall $ EcuOfPrevNameAndPath (mf,Nothing)
           
          EcuOfPrevNameAndPath ((modNm,overrFp),mbPrev) -> do
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
               when (ehcOptVerbosity opts >= VerboseDebug) $ liftIO $ do
                    putStrLn $ show modNm ++ ": " ++ show (fmap fpathToStr mbFp) ++ ": " ++ show (map fpathToStr fpsFound)
                    putStrLn $ "searchPath: " ++ show searchPath'
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
      
          ASTFromFile modSearchKey@((modNmAsked,overr),_) (AlwaysEq chkTimeStamp) asttype skey@(astfcont,_) tkey -> do
               (ecu, fp, suffoverr) <- case overr of
                 ASTFileNameOverride_FPath fp -> (bcall $ EcuOf modNmAsked) >>= \ecu -> return (ecu, fp, ASTFileSuffOverride_AsIs)
                 _	                          -> (bcall $ EcuOfPrevNameAndPath modSearchKey) >>= \ecu -> return (ecu, ecuSrcFilePath ecu, ASTFileSuffOverride_Suff skey)
                 
               let modNm = ecuModNm ecu
                   ref   = BRef_ASTFile modSearchKey asttype skey tkey
                   
%%[[8
               let mbtm = Just undefined
%%][50
               mbtm <- bcall $ ModfTimeOfFile modNm asttype skey tkey
%%]]
               opts <- bcall $ EHCOptsOf modNm
               (mbRes :: Maybe res, mbset) <- bderef' ref
               let mbhdlr = asthandlerLookup asttype
                   mkfp hdlr = asthdlrMkInputFPath hdlr opts ecu suffoverr modNm fp

               case (mbRes, mbset, mbhdlr) of
                 (Just res, _, _) -> do
                      bmemo ref
                      return res 
                 
                 (_, Just set, Just (astHdlr :: ASTHandler' res)) | chkTimeStamp == ASTFileTimeHandleHow_Ignore || isJust mbtm {- | isJust mbi && isJust mbl -} -> case astfcont of
%%[[50
                      ASTFileContent_Binary -> do
                        cpMsg' modNm VerboseALot "Decoding" Nothing fpC
                        cpTr TraceOn_BuildFPaths ["ASTFromFile ASTFileContent_Binary: " ++ show modNm ++ ", fp=" ++ fpathToStr fp ++ " -> fpC=" ++ fpathToStr fpC]
                        mbx@(~(Just x)) <- liftIO $ _asthdlrGetSerializeFileIO astHdlr opts fpC
                        if isJust mbx
                          then do
                            let errs = _asthdlrPostInputCheck astHdlr opts ecu modNm fpC x
                            if null errs
                              then ret ref x
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
                            (res,errs) <- parseWithFPath sopts popts p fpC
                            if null errs || ehpoptsOkToStopAtErr popts
                              then ret ref res
                              else seterrs errs
                          _ -> do
                            seterrs [strMsg $ "No parser for " ++ _asthdlrName astHdlr]

                      fc -> err $ "ast content handler " ++ show fc

                   where mbi@(~(Just info)) = astsuffixLookup skey $ _asthdlrSuffixRel astHdlr
                         mbl@(~(Just lens)) = Map.lookup tkey $ _astsuffinfoASTLensMp info
                         fpC                = mkfp astHdlr
                         err                = err' fpC (_asthdlrName astHdlr)
                         ret ref res        = set res >> bmemo ref >> return res

                 _ | isNothing mbhdlr               -> err1 "ast handler"
                   | isNothing mbset                -> err2 mbhdlr "ast setter"
                   | chkTimeStamp == ASTFileTimeHandleHow_AbsenceIsError && isNothing mbtm
                                                    -> err2 mbhdlr "file time info (probably non existent)"
                   | otherwise                      -> dflt'
                   where err1               = err' fp (show asttype)
                         err2 (Just h)      = err' (mkfp h) (_asthdlrName h)

            where dflt' = return $ panic $ "BuildFunction.Run.bcall (" ++ show bfun ++ ") undefined result related to " ++ show modNmAsked
                  err' fp k m = do
                    cpSetLimitErrsWhen 1 ("Decode " ++ k ++ " for file " ++ fpathToStr fp) [strMsg $ "No " ++ m ++ " for " ++ k ++ " (" ++ show skey ++ "/" ++ show tkey ++ ")"]
                    dflt'

          AST modSearchKey@((modNmAsked,_),_) asttypeAsked -> do
               -- the source file available to produce the asked AST
               ecu <- bcall $ EcuOfPrevNameAndPath modSearchKey
               let asttype  = _ecuASTType ecu
                   skey     = (_ecuASTFileContent ecu, ASTFileUse_Src)
                   tkey     = ASTFileTiming_Current
                   refAsked = BRef_AST modSearchKey asttypeAsked

               (modNm, _, _) <- bcall $ ModnameAndImports modSearchKey (_ecuASTType ecu)
               (mbRes :: Maybe res, mbset) <- bderef' refAsked
               
               opts <- bcall $ EHCOptsOf modNm
               
               case asttypeAsked of
%%[[50
                 ASTType_HI -> do
                   hsSem <- getHsSem modNm
                   ehSem <- bcall $ FoldEH (mkPrevFileSearchKeyWithName modNm)
                   let hii = HI.emptyHIInfo
                         {
						   -- cpFlowHsSem1
						   HI.hiiFixityGam            = HSSem.gathFixityGam_Syn_AGItf hsSem
                         , HI.hiiHIDeclImpModS        = ecuHIDeclImpNmS ecu						-- TBD: sort out, should not be from hi
						   -- cpFlowEHSem1, 
%%[[(50 codegen hmtyinfer)
                         , HI.hiiMbOrphan      = EHSem.mbOrphan_Syn_AGItf ehSem
%%]]
%%[[(50 hmtyinfer)
                         , HI.hiiDataGam       = EHSem.gathDataGam_Syn_AGItf    ehSem
                         , HI.hiiValGam        = EHSem.gathValGam_Syn_AGItf     ehSem
                         , HI.hiiTyGam         = EHSem.gathTyGam_Syn_AGItf      ehSem
                         , HI.hiiTyKiGam       = EHSem.gathTyKiGam_Syn_AGItf    ehSem
                         , HI.hiiPolGam        = EHSem.gathPolGam_Syn_AGItf     ehSem
                         , HI.hiiClGam         = EHSem.gathClGam_Syn_AGItf      ehSem
                         , HI.hiiClDfGam       = EHSem.gathClDfGam_Syn_AGItf    ehSem
                         , HI.hiiCHRStore      = EHSem.gathChrStore_Syn_AGItf   ehSem
                         -- , HI.hiiLamMp         = lm
%%]]
                         -- , HI.hiiHIUsedImpModS = usedImpS		-- TBD: sort out
                         }
                   retast refAsked mbset hii
%%]]

                 _ -> do
                   -- if the available and asked for AST are of the same type, get the file contents
                   if asttypeAsked == _ecuASTType ecu
                     then do
                       ast  <- bcall $ ASTFromFile modSearchKey (AlwaysEq ASTFileTimeHandleHow_AbsenceIsError) asttype skey tkey
                       breturn ast
                     else do
                       -- case analysis on what is asked for and what is given
                       case (asttypeAsked, asttype) of
                         (ASTType_EH, ASTType_HS) -> do
                            hsSem <- getHsSem modNm
                            -- cpTranslateHs2EH
                            retast refAsked mbset $ HSSem.eh_Syn_AGItf hsSem
                         (ASTType_Core, _) -> do
                            unless (asttype == ASTType_EH) $ do 
                              (_ :: EH.AGItf) <- bcall $ AST modSearchKey ASTType_EH
                              return ()
                            ehSem <- bcall $ FoldEH (mkPrevFileSearchKeyWithName modNm)
                            -- cpTranslateEH2Core
                            retast refAsked mbset $ cmodTrfElimNonCodegenConstructs opts $ EHSem.cmodule_Syn_AGItf  ehSem

             where
               -- set and return ast
               retast :: Typeable ast => BRef res -> Maybe (res -> EHCompilePhaseT m ()) -> ast -> EHCompilePhaseT m res
               retast ref mbset ast = 
                    case cast ast of
                      Just ast' -> maybe (breturn ast') (\set -> set ast' >> bmemo ref >> return ast') mbset
                      _ -> do cpTr TraceOn_BuildTypeables ["retast: " ++ show (typeOf ref) ++ ", " ++ show (typeOf ast)]
                              panic $ "Should not happen: retast: " ++ show ref
       
               -- get HS semantics
               getHsSem modNm = do
%%[[8
                    hsSem <-
%%][50
                    (hsSem, _) <-
%%]]
                      bcall $ FoldHs (mkPrevFileSearchKeyWithName modNm)
                    return hsSem

%%[[50
          ModfTimeOfFile modNm asttype skey tkey -> case
            (asthandlerLookup' asttype $ \hdlr -> do
                 suffinfo <- astsuffixLookup skey $ _asthdlrSuffixRel hdlr
                 let mblens = Map.lookup tkey $ _astsuffinfoModfTimeMp suffinfo
                 return
                   ( mblens
                   , \opts ecu fp -> _asthdlrMkInputFPath hdlr opts ecu modNm fp (_astsuffinfoSuff suffinfo)
                   )
            ) of
                 Just (mblens, mkfp) -> do
                        cr <- get
                        let (ecu,_,opts,fp) = crBaseInfo modNm cr
                        tm opts ecu (maybe (const id) (\lens -> (lens ^=) . Just) mblens) (mkfp opts ecu fp)
                 _ -> breturn Nothing
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
               mbTm <- bcall $ ModfTimeOfFile modNm (ecu ^. ecuASTType) (ecu ^. ecuASTFileContent, ecu ^. ecuASTFileUse) ASTFileTiming_Current
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
                 { mieimplLamMp             = Core2GrSem.lamMp_Inh_CodeAGItf $ crsiCoreInh crsi
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
               ecu      <- bcall $ EcuOfPrevNameAndPath modSearchKey
               hs       <- bcall $ ASTFromFile modSearchKey (AlwaysEq ASTFileTimeHandleHow_AbsenceIsError) ASTType_HS (_ecuASTFileContent ecu, ASTFileUse_Src) ASTFileTiming_Current
               opts     <- bcall $ EHCOptsOf modNm
               crsi     <- bcall $ CRSIOfName modSearchKey
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

               return
                 ( hsSem
%%[[50
                 -- , ecuSrcDeclImpNmS ecu
                 , hasMain
%%]]
                 )
               
          FoldEH modSearchKey@((modNm,_),_) -> do
               ecu  <- bcall $ EcuOfPrevNameAndPath modSearchKey
               eh   <- bcall $ ASTFromFile modSearchKey (AlwaysEq ASTFileTimeHandleHow_AbsenceIsError) ASTType_EH (ASTFileContent_Text, ASTFileUse_Src) ASTFileTiming_Current
               opts <- bcall $ EHCOptsOf modNm
               crsi <- bcall $ CRSIOfName modSearchKey
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
               return ehSem

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
               ecu  <- bcall $ EcuOfPrevNameAndPath modSearchKey
               core <- bcall $ ASTFromFile modSearchKey (AlwaysEq ASTFileTimeHandleHow_AbsenceIsError) ASTType_Core (_ecuASTFileContent ecu, ASTFileUse_Src) ASTFileTiming_Current
               opts <- bcall $ EHCOptsOf modNm
               crsi <- bcall $ CRSIOfName modSearchKey
               let  coreInh  = crsiCoreInh crsi
                    coreSem  = Core2GrSem.wrap_CodeAGItf
                                 (Core2GrSem.sem_CodeAGItf (Core.CodeAGItf_AGItf core))
                                 (coreInh { Core2GrSem.gUniq_Inh_CodeAGItf                         = crsi ^. crsiHereUID
                                          , Core2GrSem.opts_Inh_CodeAGItf                          = opts
%%[[50
                                          , Core2GrSem.importUsedModules_Inh_CodeAGItf             = ecuImportUsedModules ecu
%%]]
                                          })
               cpUpdCU modNm $ ecuStoreCoreSem coreSem
               breturn coreSem
%%]]

%%[[(8 core corerun)
          FoldCore2CoreRun modSearchKey@((modNm,_),_) -> do
               ecu  <- bcall $ EcuOfPrevNameAndPath modSearchKey
               core <- bcall $ ASTFromFile modSearchKey (AlwaysEq ASTFileTimeHandleHow_AbsenceIsError) ASTType_Core (_ecuASTFileContent ecu, ASTFileUse_Src) ASTFileTiming_Current
               opts <- bcall $ EHCOptsOf modNm
               crsi <- bcall $ CRSIOfName modSearchKey
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
               breturn sem
               
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

    -- construct a reference to something and also yield corresponding result
    brefto' :: BFun' res -> BRef res -> EHCompilePhaseT m (Maybe res)
    brefto' bfun ref = bmemo ref >> bderef ref
    
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
            Just (ref :: BRef res) -> bderef ref
            _ -> return Nothing

    -- factored out: new module name extracted from src file
    newModNm :: HsName -> HsName -> (EHCompileUnit -> EHCompileUnit) -> EHCompilePhaseT m (HsName, Maybe PrevSearchInfo)
    newModNm modNm modNm' upd = do
        modNmNew <- ifM (bcall $ IsTopMod modNm)
          (do
            cpUpdCUWithKey modNm (\_ ecu -> (modNm', upd $ cuUpdKey modNm' ecu))
            return modNm'
          )
          (do
            cpUpdCU modNm upd
            return modNm
          )
        ecu <- bcall $ EcuOf modNmNew
        let newPrev = Just (modNmNew, (ecuSrcFilePath ecu, ecuFileLocation ecu))
        cpUpdCU modNmNew $ ecuMbPrevSearchInfo ^= newPrev
        return (modNmNew, newPrev)
%%]



%%[8 export(bderef)
-- | Dereference an indirection into compilation state, possibly with a result, and a setter
bderef' :: forall res m . (Typeable res, EHCCompileRunner m) => BRef res -> EHCompilePhaseT m (Maybe res, Maybe (res -> EHCompilePhaseT m ()))
bderef' bref = do
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
bderef :: forall res m . (Typeable res, EHCCompileRunner m) => BRef res -> EHCompilePhaseT m (Maybe res)
bderef bref = do
    cpTr TraceOn_BuildFun $ ["deref " ++ show bref]
    fmap fst $ bderef' bref
%%]




%%[0 hs
{-# LANGUAGE GADTs #-}
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

%%[99 import ({%{EH}Base.PackageDatabase})
%%]

-- parsing, scanning
%%[8 import ({%{EH}Base.ParseUtils}, UHC.Util.ScanUtils as ScanUtils)
%%]

-- source handling
%%[8 import ({%{EH}EHC.ASTHandler}, {%{EH}EHC.ASTHandler.Instances})
%%]

-- general imports
%%[8 import (UHC.Util.Lens)
%%]
%%[8888 import (qualified UHC.Util.RelMap as Rel)
%%]
%%[8 import (Data.Typeable)
%%]
%%[8 import (qualified Data.Map as Map)
%%]
%%[8 import (Control.Monad.State, Control.Monad.Error)
%%]

%%[50 import (System.Directory)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Build function calling/running
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(bcall)
-- | Execute a build function, possibly caching/memoizing a result
bcall :: forall res m . (Typeable res) => EHCCompileRunner m => BFun' res -> EHCompilePhaseT m res
bcall bfun = do
    bcache <- getl $ st ^* bstateCache
    
    mbCachedRes <- lkup bfun bcache
    case mbCachedRes of
      Just res -> return res
      _ -> do
        -- prepare
        start
        -- actual execution
        res <- case bfun of
          EcuOfName modNm -> do
               bcall $ EcuOfNameAndPath Nothing (modNm, Nothing)
           
          EHCOptsOf modNm -> do
               fmap (panicJust "EHCOptsOf") $ bderef (BRef_EHCOpts modNm)

          EcuOfNameAndPath mbPrev (modNm,mbFp) -> do
               opts <- bcall $ EHCOptsOf modNm
               let isTopModule = isJust mbFp
                   searchPath = ehcOptImportFileLocPath opts
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
               fmap (panicJust "EcuOfNameAndPath") $ cpMbCU modNm
           
          FPathSearchForFile suff fn -> do
               let fp    = mkTopLevelFPath suff fn
                   modNm = mkHNm $ fpathBase fp
               breturn (modNm, fp)
      
          ASTFromFile mf asttype skey@(astfcont,_) tkey -> do
               ecu <- bcall $ EcuOfNameAndPath Nothing mf
               let modNm = ecuModNm ecu
                   fp    = ecuFilePath ecu
                   ref   = BRef_AST modNm asttype skey tkey
               opts <- bcall $ EHCOptsOf modNm
%%[[8
               let mbtm = Just undefined
%%][50
               mbtm <- bcall $ ModfTimeOfFile modNm asttype skey tkey
%%]]
               (_ :: Maybe res, mbset) <- bderef' ref
               case (mbtm, mbset, asthandlerLookup asttype) of
                 (Just _, Just set, Just (astHdlr :: ASTHandler' res)) {- | isJust mbi && isJust mbl -} -> case astfcont of
%%[[50
                      ASTFileContent_Binary -> do
                        cpMsg' modNm VerboseALot "Decoding" Nothing fpC
                        mbx@(~(Just x)) <- liftIO $ _asthdlrGetSerializeFileIO astHdlr opts fpC
                        if isJust mbx
                          then do
                            let errs = _asthdlrPostInputCheck astHdlr opts ecu modNm fpC x
                            if null errs
                              then do 
                                set x
                                bmemo ref
                                return x
                              else do
                                cpSetLimitErrsWhen 1 ("Decode AST check " ++ _asthdlrName astHdlr) errs
                                dflt'
                          else err fp "decoder"
%%]]
                      ASTFileContent_Text -> do
                        let --
%%[[8
                            popts       = defaultEHParseOpts
%%][50
                            popts       = _astsuffinfoUpdParseOpts info defaultEHParseOpts
%%]]
                            sopts       = _asthdlrParseScanOpts astHdlr opts popts
                            description = "Parse (" ++ (if ScanUtils.scoLitmode sopts then "Literate " else "") ++ _asthdlrName astHdlr ++ " syntax) of module `" ++ show modNm ++ "`"
                            seterrs     = cpSetLimitErrsWhen 5 description
                        case _asthdlrParser astHdlr opts popts of
                          Just (ASTParser p) -> do
                            (res,errs) <- parseWithFPath sopts popts p fp -- (maybe (ecuFilePath (crCU modNm cr)) id mbFp)
                            -- cpUpdCU modNm (_asthdlrEcuStore astHdlr res)
                            if null errs
                              then do
                                set res
                                bmemo ref
                                return res
                              else do
                                if ehpoptsStopAtErr popts
                                  then return ()
                                  else seterrs errs
                                dflt'
                          _ -> do
                            seterrs [strMsg $ "No parser for " ++ _asthdlrName astHdlr]
                            dflt'

                      _ -> err fp "ast content handler"

                   where mbi@(~(Just info)) = astsuffixLookup skey $ _asthdlrSuffixRel astHdlr
                         mbl@(~(Just lens)) = Map.lookup tkey $ _astsuffinfoASTLensMp info
                         fpC                = asthdlrMkInputFPath astHdlr opts ecu skey modNm fp
                         err fp             = err' fp (_asthdlrName astHdlr)
                 _ -> err' fp "" "ast handler/setter"
            where dflt' = return undefined
                  err' fp k m = do
                    cpSetLimitErrsWhen 1 ("Decode " ++ k ++ " for file " ++ fpathToStr fp) [strMsg $ "No " ++ m ++ " for " ++ k ++ " (" ++ show skey ++ "/" ++ show tkey ++ ")"]
                    dflt'

%%[[50
          ModfTimeOfFile modNm asttype skey tkey -> case
            (asthandlerLookup' asttype $ \hdlr -> do
                 suffinfo <- astsuffixLookup skey $ _asthdlrSuffixRel hdlr
                 lens <- Map.lookup tkey $ _astsuffinfoModfTimeMp suffinfo
                 return (_astsuffinfoSuff suffinfo, lens)
            ) of
                 Just (suff, lens) -> do
                        cr <- get
                        let (ecu,_,opts,fp) = crBaseInfo modNm cr
                        tm opts ecu ((lens ^=) . Just) (fpathSetSuff suff fp)
                 _ -> return Nothing
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
                      return $ Just t
                    else return Nothing
%%]]
          
          _ -> panic $ "BuildFunction.Run.bcall: not implemented: " ++ show bfun

        -- finalize
        end
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
          Just bfun -> st ^* bstateCache =$: bcacheInsert bfun res
          _ -> panic $ "BuildFunction.Run.bcall.bmemo: " ++ show bfun

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
%%]



%%[8 export(bderef)
-- | Dereference an indirection into compilation state, possibly with a result, and a setter
bderef' :: forall res m . (Typeable res, EHCCompileRunner m) => BRef res -> EHCompilePhaseT m (Maybe res, Maybe (res -> EHCompilePhaseT m ()))
bderef' bref = do
    cr <- get
    case bref of
      BRef_ECU modNm -> return (crMbCU modNm cr, Just $ \ecu -> cpUpdCU modNm (const ecu))
      BRef_EHCOpts modNm -> return (Just choose, Nothing)
        where opts = cr ^. crStateInfo ^. crsiOpts
%%[[8
              choose = opts
%%][99
              choose = maybe opts id $ crMbCU modNm cr >>= ecuMbOpts
%%]]
      BRef_AST modNm asttype skey tkey -> case asthandlerLookup asttype of
          Just (hdlr :: ASTHandler' res) -> case astsuffixLookup skey $ _asthdlrSuffixRel hdlr of
            Just suffinfo -> case Map.lookup tkey $ _astsuffinfoASTLensMp suffinfo of
              Just l -> do
                ecu <- bcall $ EcuOfName modNm
                return (ecu ^. l, Just $ \ast -> cpUpdCU modNm $ l ^= Just ast)
              _ -> dflt
            _ -> dflt
          _ -> dflt
        where dflt = return (Nothing, Nothing)

-- | Dereference an indirection into compilation state
bderef :: forall res m . (Typeable res, EHCCompileRunner m) => BRef res -> EHCompilePhaseT m (Maybe res)
bderef bref = fmap fst $ bderef' bref
%%]




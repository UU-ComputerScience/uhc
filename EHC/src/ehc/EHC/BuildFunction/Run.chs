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
%%[8 import ({%{EH}EHC.BuildFunction})
%%]

-- compiler driver
%%[8 import ({%{EH}EHC.Common}, {%{EH}EHC.CompileRun}, {%{EH}EHC.CompileUnit}, {%{EH}EHC.FileSuffMp})
%%]
%%[8888 import ({%{EH}EHC.Main.Compile})
%%]

%%[99 import ({%{EH}Base.PackageDatabase})
%%]

-- general imports
%%[8 import (UHC.Util.Lens)
%%]
%%[8 import (Data.Typeable)
%%]
%%[8 import (Control.Monad.State)
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
               bmemoRef $ BRef_ECU modNm
               fmap (panicJust "EcuOfNameAndPath") $ cpMbCU modNm
           
          FPathSearchForFile suff fn -> do
               let fp    = mkTopLevelFPath suff fn
                   modNm = mkHNm $ fpathBase fp
               breturn (modNm, fp)
      
          _ -> panic $ "BuildFunction.Run.bcall: not implemented: " ++ show bfun

        -- finalize
        end
        return res
  where
    st    = crStateInfo ^* crsiBState

    start = st ^* bstateCallStack =$: (BFun bfun :)
    end   = st ^* bstateCallStack =$: tail
    
    -- memoize
    bmemoRef :: BRef res -> EHCompilePhaseT m ()
    bmemoRef res = do
        (BFun bfun : _) <- getl $ st ^* bstateCallStack
        case cast bfun of
          Just bfun -> st ^* bstateCache =$: bcacheInsert bfun res
          _ -> panic $ "BuildFunction.Run.bcall.bmemoRef: " ++ show bfun

    -- memoize & return (duplicate code w.r.t. bmemoRef: TBD fix)
    breturn :: res -> EHCompilePhaseT m res
    breturn res = do
        (BFun bfun : _) <- getl $ st ^* bstateCallStack
        case cast bfun of
          Just bfun -> st ^* bstateCache =$: bcacheInsert bfun (Identity res)
          _ -> panic $ "BuildFunction.Run.bcall.breturn: " ++ show bfun
        return res

    lkup :: BFun' res -> BCache -> EHCompilePhaseT m (Maybe res)
    lkup bfun bcache =
        case bcacheLookup bfun bcache of
          Just (res :: Identity res) -> return $ Just $ runIdentity res
          _ -> case bcacheLookup bfun bcache of
            Just (ref :: BRef res) -> bderef ref
            _ -> return Nothing


%%]

%%[8 export(bderef)
-- | Dereference an indirection into compilation state
bderef :: EHCCompileRunner m => BRef res -> EHCompilePhaseT m (Maybe res)
bderef bref = do
    cr <- get
    case bref of
      BRef_ECU modNm -> return $ crMbCU modNm cr
      BRef_EHCOpts modNm -> return $ Just choose
        where opts = cr ^. crStateInfo ^. crsiOpts
%%[[8
              choose = opts
%%][99
              choose = maybe opts id $ crMbCU modNm cr >>= ecuMbOpts
%%]]
%%]


      {-
      -- Applicative part
      Pure res -> return res
      App  f a -> do
          f' <- bcall f
          a' <- bcall a
          return $ f' a'
      -}
      

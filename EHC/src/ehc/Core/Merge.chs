%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Core merge
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 core) module {%{EH}Core.Merge}
%%]

%%[(50 core) hs import({%{EH}Opts},{%{EH}Base.Common},{%{EH}Ty},{%{EH}Core} hiding (cModMerge))
%%]

%%[(50 core) hs import({%{EH}AbstractCore})
%%]

%%[(50 core) hs import({%{EH}Module.Merge})
%%]

%%[(50 core) import(Data.List,Data.Maybe,qualified Data.Set as Set,qualified Data.Map as Map)
%%]
%%[(50 core) import(UHC.Util.Utils)
%%]

%%[(50 core) import(Control.Monad.Identity, Control.Monad.State, Data.Array)
%%]
%%[(50 core) import(qualified UHC.Util.FastSeq as Seq)
%%]
%%[(50 core) import({%{EH}Core.FvS}, {%{EH}Core.ModAsMap})
%%]
%%[(90 core) import({%{EH}Core.ExtractFFE})
%%]

-- debug
%%[(5050 core) import({%{EH}Base.Debug},UHC.Util.Pretty)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module merge by pulling in only that which is required
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 core) hs
instance ModPuller
           CModule
           (CModuleDatabase, Map.Map HsName CModuleDatabase)
           (HsName,CExportL,CImportL,CDeclMetaL)
           CModule CExpr CBindCateg CBind
  where
    mpullSplit mmain@(CModule_Mod modNm _ _ _ _) mimpL =
        ( cmoddbMainExpr modDbMain
        , 
%%[[50
          Nothing
%%][90
          Just (CBindCateg_FFE, [ (effeBind e, effeFvS e) | m <- mmain : mimpL, e <- cmodExtractFFE m ])
%%]]
        , ( modDbMain
          , Map.unions [ Map.singleton (cmoddbModNm db) db | db <- modDbMain : modDbImp ]
          )
        , ( modNm
          -- TBD: combine this in some way with the FFE implicit exports...
          , Set.toList $ Set.fromList $ concatMap cmoddbExports $ modDbMain : modDbImp
          , Set.toList $ Set.fromList $ concatMap cmoddbImports $ modDbMain : modDbImp
          , concatMap cmoddbMeta $ modDbMain : modDbImp
        ) )
      where modDbMain =     cexprModAsDatabase mmain
            modDbImp  = map cexprModAsDatabase mimpL
            modDbMp   = Map.unions [ Map.singleton (cmoddbModNm db) db | db <- modDbMain : modDbImp ]

    mpullUsedBindings n (modDbMain,modDbMp) = return $ do
        db <- maybe (Just modDbMain) (\m -> Map.lookup m modDbMp) $ hsnQualifier n
        (bi,_) <- cmoddbLookup n db
        let (cat,bsarr) = cmoddbBindArr db ! bi
            bs = elems bsarr
        return
          ( cat, bs
          , Set.fromList $ map cbindNm bs
          )

    mpullRelevantExprs = cbindExprs
    
    mpullFreeVars = cexprFvS

    mpullBindingsAddToMod (modNm,allExports,allImports,allMeta) rootExpr bs _ = CModule_Mod modNm allExports allImports allMeta (acoreLetN bs $ rootExpr)
%%]

%%[(50 core) hs export(cModMerge)
-- | merge by pulling
cModMerge :: (CModule,[CModule]) -> CModule
cModMerge mods@(mmain,mimpL)
  = mkM mmain
  where (mkM,_) = modMergeByPullingIn mods
%%]


%%[0
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

%%[(50 core) import(Data.List,Data.Maybe,qualified Data.Set as Set,qualified Data.Map as Map)
%%]
%%[(50 core) import(UHC.Util.Utils)
%%]

%%[(50 core) import(Control.Monad.State, Data.Array)
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

cModMergeByPullingIn is independent of AST, so should be placed in shared module when used for (say) GRIN

%%[(50 core) hs export(cModMergeByPullingIn)
data PullState cat bind
  = PullState
      { pullstBinds     :: Seq.Seq (CDbBindLetInfo'2 cat bind)  -- pulled in bindings
      , pullstPulledNmS :: !HsNameS                             -- pulled in names  
      , pullstToDo      :: ![HsName]                            -- todo
      }

emptyPullState :: PullState cat bind
emptyPullState = PullState Seq.empty Set.empty []

-- | merge by pulling in that which is required only
cModMergeByPullingIn
  ::                                -- ^ function giving bindings for name
     (HsName                        -- ^ name
      -> Maybe
           ( cat                    -- ^ category
           , [bind]                 -- ^ and bindings
           , HsNameS                -- ^ pulled in names (might be > 1 for mutual recursiveness)
     )     )
     -> (expr -> HsNameS)           -- ^ extract free vars
     -> (bind -> [expr])            -- ^ extract relevant exprs for binding
     -> ([(cat,[bind])] -> mod -> mod)
                                    -- ^ update module with pulled bindings
     -> expr                        -- ^ start of pulling in, usually top level name "main"
     -> (cat,[(bind,HsNameS)])		-- ^ exports, providing additional pull starting points
     -> ( (mod -> mod)              -- ^ conversion of resulting module
        , HsNameS                   -- ^ modules from which something was taken
        )
cModMergeByPullingIn
     pullIn getExprFvS getBindExprs updMod
     rootExpr (exportCateg,rootExports)
  = ( updMod (Seq.toList $ pullstBinds final)
    , Set.map (panicJust "cModMergeByPullingIn" . hsnQualifier) $ pullstPulledNmS final
    )
  where final = st {pullstBinds = pullstBinds st `Seq.union` Seq.fromList [ (exportCateg,[b]) | (b,_) <- rootExports ]}
              where st = execState pull init
        init  = emptyPullState
                  {pullstToDo = Set.toList $ Set.unions $ getExprFvS rootExpr : map snd rootExports}
        pull  = do
          s <- get
          case pullstToDo s of
            (nm:nmRest)
              | nm `Set.notMember` pullstPulledNmS s && isJust mbPull
                -> do let pulledNms = pullstPulledNmS s `Set.union` pulled
                          newNms
                            = -- (\x -> tr "cModMergeByPullingIn.pulledNms" (nm >#< show x) x) $
                              (Set.unions $ map (Set.unions . map getExprFvS . getBindExprs) binds)
                              `Set.difference` pulledNms
                      put $
                        s { pullstToDo = Set.toList newNms ++ nmRest
                          , pullstBinds = Seq.singleton (cat,binds) `Seq.union` pullstBinds s
                          , pullstPulledNmS = pulledNms
                          }
                      pull
              | otherwise
                -> do put $
                        s { pullstToDo = nmRest
                          }
                      pull
              where mbPull@(~(Just (cat,binds,pulled))) = pullIn nm
            _ -> return ()
%%]

%%[(50 core) hs export(cModMerge)
-- | merge by pulling
cModMerge :: ([CModule],CModule) -> CModule
cModMerge (mimpL,mmain)
  = mkM mmain
  where (mkM,_)   = cModMergeByPullingIn lkupPull cexprFvS cbindExprs
                                         (\bs (CModule_Mod modNm _ _ _) -> CModule_Mod modNm allImports allMeta (acoreLetN bs $ rootExpr) ) -- allTags)
                                         rootExpr rootExports
        rootExpr  = cmoddbMainExpr modDbMain
        -- allTags   = concatMap cmoddbTagsMp $ modDbMain : modDbImp
        allMeta   = concatMap cmoddbMeta    $ modDbMain : modDbImp
        allImports= Set.toList $ Set.fromList $ concatMap cmoddbImports $ modDbMain : modDbImp
        modDbMain =     cexprModAsDatabase mmain
        modDbImp  = map cexprModAsDatabase mimpL
        modDbMp = Map.unions [ Map.singleton (cmoddbModNm db) db | db <- modDbMain : modDbImp ]
        lkupMod  n = -- (\x -> tr "cModMerge.lkupMod" (n >#< fmap cmoddbModNm x) x) $
                     maybe (Just modDbMain) (\m -> Map.lookup m modDbMp) $ hsnQualifier n
        lkupPull n = do
           db <- lkupMod n
           (bi,_) <- cmoddbLookup n db
           let (cat,bsarr) = cmoddbBindArr db ! bi
               bs = elems bsarr
           return ( cat, bs
                  , -- (\x -> tr "cModMerge.lkupPull" (n >#< show x) x) $
                    Set.fromList $ map cbindNm bs
                  )
%%[[50
        rootExports = (CBindCateg_Rec,[])
%%][90
        rootExports = (CBindCateg_FFE,ffes)
                    where ffes = [ (effeBind e, effeFvS e) | m <- mmain : mimpL, e <- cmodExtractFFE m ]
%%]]
%%]
        lkupMod  n = do
           m <- (\x -> tr "cModMerge.lkupMod" (n >#< x) x) $
                hsnQualifier n
           Map.lookup m modDbMp


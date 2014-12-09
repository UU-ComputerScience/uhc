%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module merge, for now only in context of codegen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 codegen) module {%{EH}Module.Merge}
%%]

%%[(50 codegen) hs import({%{EH}Base.Common})
%%]

%%[(50 codegen) import(Data.List,Data.Maybe,qualified Data.Set as Set,qualified Data.Map as Map)
%%]
%%[(50 codegen) import(UHC.Util.Utils)
%%]

%%[(50 codegen) import(Control.Monad.Identity, Control.Monad.State, Data.Array)
%%]
%%[(50 codegen) import(qualified UHC.Util.FastSeq as Seq)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Supporting types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 codegen) hs export(ModDbBindLetInfo'')
-- | the binding info required for let bind
type ModDbBindLetInfo'' f cat bind = (cat,f bind)
type ModDbBindLetInfo'2   cat bind = ModDbBindLetInfo'' [] cat            bind
%%]

%%[(50 codegen) hs export(ModDbBindArray')
-- | actual bindings stored in separate array to allow for sharing
type ModDbBindArray' cat bind = Array Int (ModDbBindLetInfo'' (Array Int) cat bind)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module puller class itf
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 codegen) hs export(ModPuller(..))
-- | Abstraction of how pulling from a module to something else is done
class ModPuller modFr modsDb modsRem modTo expr cat bind
{- 20140928 TBD: not properly refactored yet, in particular the functional dpds are too restrictive...
-}
    | modFr -> expr cat bind modsRem modsDb modTo
    , expr -> modFr
    , cat -> modFr
    , bind -> modFr
    , modsRem -> modFr
    , modsDb -> modFr
    , modTo -> modFr
  where
    -- | Split main + imported into (1) root expr, (2) root bindings also visible and to be included, (3) part on which merging takes place and (4) a remainder
    mpullSplit :: modFr -> [modFr] -> (expr, Maybe (cat,[(bind,HsNameS)]), modsDb, modsRem)

    -- | Extract bindings for a name, consisting of a category, set of bindings, and the bound names (including the one asked for) pulled in.
    mpullUsedBindings :: Monad m => HsName -> modsDb -> m (Maybe (cat, [bind], HsNameS))
    
    -- | Extract expr's relevant for inducing further pullins
    mpullRelevantExprs :: bind -> [expr]
    
    -- | Extract free names/vars from expr
    mpullFreeVars :: expr -> HsNameS
    
    -- | Combine bindings + root expr into result module
    mpullBindingsAddToMod :: modsRem -> expr -> [(cat,[bind])] -> modTo -> modTo
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module merge by pulling in only that which is required
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 codegen) hs
data PullState cat bind
  = PullState
      { pullstBinds     :: Seq.Seq (ModDbBindLetInfo'2 cat bind)  	-- ^ pulled in bindings
      , pullstPulledNmS :: !HsNameS                             	-- ^ pulled in names  
      , pullstToDo      :: ![HsName]                            	-- ^ names todo
      }

emptyPullState :: PullState cat bind
emptyPullState = PullState Seq.empty Set.empty []
%%]

%%[(50 codegen) hs
type ModMergeT cat bind m a = StateT (PullState cat bind) m a
%%]

%%[(50 codegen) hs export(modMergeByPullingInM)
-- | merge by pulling in that which is required only, monadically
modMergeByPullingInM
  :: ( ModPuller modFr modsDb modsRem modTo expr cat bind
     , Monad m
     )
  => (modFr, [modFr])			    -- ^ main and imported
     -> ModMergeT cat bind m
          ( (modTo -> modTo)        -- conversion of resulting module
          , HsNameS                 -- modules from which something was taken
          )
modMergeByPullingInM (modMain,modImpL) = do
    let (rootExpr,mbExports,modDb,modRem) = mpullSplit modMain modImpL
    put $ emptyPullState {pullstToDo = Set.toList $ Set.unions $ mpullFreeVars rootExpr : (maybe [] (map snd . snd) mbExports)}
    pull modDb
    when (isJust mbExports) $
      let (Just (exportCateg,rootExports)) = mbExports
      in  modify $ \st -> st {pullstBinds = pullstBinds st `Seq.union` Seq.fromList [ (exportCateg,[b]) | (b,_) <- rootExports ]}
    final <- get
    return
      ( mpullBindingsAddToMod modRem rootExpr (Seq.toList $ pullstBinds final)
      , Set.map (panicJust "modMergeByPullingInM" . hsnQualifier) $ pullstPulledNmS final
      )
  where
    pull modDb = do
      s <- get
      case pullstToDo s of
        (nm:nmRest)
          | nm `Set.notMember` pullstPulledNmS s {- && isJust mbPull -} -> do
              mbPull@(~(Just (cat,binds,pulled))) <- lift $ mpullUsedBindings nm modDb
              if isJust mbPull
                then do
                  let pulledNms = pullstPulledNmS s `Set.union` pulled
                      newNms
                        = (Set.unions $ map (Set.unions . map mpullFreeVars . mpullRelevantExprs) binds)
                          `Set.difference` pulledNms
                  put $
                    s { pullstToDo = Set.toList newNms ++ nmRest
                      , pullstBinds = Seq.singleton (cat,binds) `Seq.union` pullstBinds s
                      , pullstPulledNmS = pulledNms
                      }
                  pull modDb
                else
                  dfltContinue
          | otherwise -> dfltContinue
          where -- mbPull@(~(Just (cat,binds,pulled))) = pullIn nm
                dfltContinue = do
                  put $ s { pullstToDo = nmRest }
                  pull modDb
        _ -> return ()
%%]

%%[(50 codegen) hs export(modMergeByPullingIn)
-- | merge by pulling in that which is required only
modMergeByPullingIn
  :: ModPuller modFr modsDb modsRem modTo expr cat bind
  => (modFr, [modFr])		     	-- ^ main and imported
     -> ( (modTo -> modTo)          -- conversion of resulting module
        , HsNameS                   -- modules from which something was taken
        )
modMergeByPullingIn mods = flip evalState emptyPullState $ modMergeByPullingInM mods
%%]


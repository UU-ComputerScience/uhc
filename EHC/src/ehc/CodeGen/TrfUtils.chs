%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utilities for transformations on intermediate code representations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs module {%{EH}CodeGen.TrfUtils}
%%]

%%[(8 codegen) import(qualified Data.Map as Map, qualified Data.Set as Set)
%%]
%%[(8 codegen) import(Control.Monad, Control.Monad.State.Strict, Control.Monad.Reader)
%%]

%%[(8 codegen) import({%{EH}Base.Target},{%{EH}Base.Optimize})
%%]

%%[(8 codegen) import({%{EH}EHC.Common})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface to transformations, used internally as state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(TrfState(..),mkEmptyTrfState)
-- | Environmental info for transformations. TBD 20140409: sort out what is really necessary
data TrfReader
  = TrfReader
      { trfrdMustDump			:: EHCOpts -> Bool				-- ^ must dump?
      , trfrdCanDoOptScope		:: [OptimizationScope] -> Bool	-- ^ trf must be done for this optimization scope?
      , trfrdOpts				:: EHCOpts						-- ^ global options
      , trfrdModNm				:: HsName						-- ^ module name
      }

-- | State info for transformations
data TrfState
		mod		-- module structure
		extra	-- extra state info, extension
  = TrfState
      { trfstMod    			:: !mod							-- ^ most recent transformed module
      , trfstModStages			:: [(String,Maybe mod,ErrL)]	-- ^ intermediate stages with errors, if dumping also with module
      , trfstUniq           	:: !UID							-- ^ unique counter, threaded in/out
      , trfstExtra				:: extra						-- ^ optional extension of state info
      }

mkEmptyTrfState :: mod -> extra -> TrfState mod extra
mkEmptyTrfState m e = TrfState m [] uidStart e

-- | The monad for transformations
type TrfM mod extra = ReaderT TrfReader (State (TrfState mod extra))
%%]

%%[(8 codegen)
-- | Freshness
instance MonadFreshUID (TrfM mod extra) where
  freshInfUID = modifyGets $ \s@(TrfState{trfstUniq=u}) ->
    let (n,h) = mkNewLevUID u
    in  (h,s {trfstUniq = n})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Running the transformations + checks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(runTrf)
-- | Run transformations.
--   The 'optimScope' tells at which compilation phase (per module, whole program) the transformations are done, default only per module
runTrf
  :: EHCOpts
     -> HsName
     -> (EHCOpts -> Bool)								-- ^ need to dump?
     -> ([OptimizationScope] -> Bool)					-- ^ can run for optimization scope?
     -> TrfState mod extra
     -> (TrfM mod extra ())
     -> TrfState mod extra
runTrf opts modNm mustDump canDoWRTOptScope trfst trf
  = execState (runReaderT trf (TrfReader mustDump canDoWRTOptScope opts modNm)) trfst
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utils to build transformations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(liftTrfModPlain)
-- | Lift a module transformation function to the state monad
liftTrfModPlain
  :: (MonadState (TrfState mod extra) m, MonadReader TrfReader m) =>
     [OptimizationScope]							-- ^ only when in this optimization scope, ie limiting the run
     -> String										-- ^ name of trf
     -> (mod -> mod)								-- ^ trf
     -> m ()
liftTrfModPlain os nm t
  = liftTrfWithExtraInfo os nm (flip const) (\_ c -> (Just $ t c,(),[]))
%%]

%%[(8 codegen) export(liftCheckMod)
-- | Only check
liftCheckMod
  :: (MonadState (TrfState mod extra) m, MonadReader TrfReader m) =>
     [OptimizationScope]								-- ^ only when in this optimization scope
     -> String											-- ^ name of trf
     -> (TrfState mod extra -> mod -> ErrL)				-- ^ check
     -> m ()
liftCheckMod os nm t
  = liftTrfWithExtraInfo os nm (flip const) (\s c -> let e = t s c in (Nothing,(),e))
%%]

%%[(8 codegen) export(liftTrfModWithState)
-- | Lift a module transformation function taking also state to the state monad
liftTrfModWithState
  :: (MonadState (TrfState mod extra) m, MonadReader TrfReader m) =>
     [OptimizationScope]								-- ^ only when in this optimization scope
     -> String											-- ^ name of trf
     -> (TrfState mod extra -> mod -> mod)				-- ^ trf
     -> m ()
liftTrfModWithState os nm t
  = liftTrfWithExtraInfo os nm (flip const) (\s c -> (Just $ t s c,(),[]))
%%]

%%[(8 codegen) export(liftTrfModWithStateExtra)
liftTrfModWithStateExtra
  :: (MonadState (TrfState mod extra) m, MonadReader TrfReader m) =>
     [OptimizationScope]								-- ^ only when in this optimization scope
     -> String											-- ^ name of trf
     -> (t -> TrfState mod extra -> TrfState mod extra)	-- ^ state update with extra info
     -> (TrfState mod extra -> mod -> (mod,t))			-- ^ trf
     -> m ()
liftTrfModWithStateExtra os nm update2 t
  = liftTrfWithExtraInfo os nm update2 (\s c -> let (c',e) = t s c in (Just c',e,[]))
%%]

%%[(8 codegen) export(liftTrfWithExtraInfo)
-- | Lift a module transformation function dealing with some arbitrary extra info to the state monad, factoring out yes/no dumping and error gathering
liftTrfWithExtraInfo
  :: (MonadState (TrfState mod extra) m, MonadReader TrfReader m) =>
     [OptimizationScope]								-- ^ only when in this optimization scope
     -> String											-- ^ name of trf
     -> (t -> TrfState mod extra -> TrfState mod extra)				-- ^ state update with extra info
     -> (TrfState mod extra -> mod -> (Maybe mod, t, ErrL))	-- ^ trf, dealing with extra info as well
     -> m ()
liftTrfWithExtraInfo os nm update2 t = do
  canDoWRTOptScope <- asks trfrdCanDoOptScope
  opts <- asks trfrdOpts
  if canDoWRTOptScope os 
    then do
      mustDump <- asks trfrdMustDump
      modify $ \s@(TrfState{trfstMod=c, trfstModStages=stages}) ->
        let (c',extra,errl) = t s c
        in  update2 extra
			$ s { trfstMod           = maybe c id c'
				, trfstModStages     = stages ++ [(nm,if mustDump opts then c' else Nothing,errl)]
				}
    else return ()
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Monad utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(modifyGets)
-- | Combi of modify and get: modify and also return newly set value. TBD 20140409: get rid of this...?
modifyGets :: MonadState s m => (s -> (a,s)) -> m a
modifyGets update
  = do { s <- get
       ; let (x,s') = update s
       ; put s'
       ; return x
       }
%%]


%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Run Core infrastructure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs module {%{EH}CoreRun.Run}
%%]

%%[(8 corerun) hs import({%{EH}Base.HsName.Builtin},{%{EH}Base.Common},{%{EH}Opts},{%{EH}Ty},{%{EH}Error},{%{EH}Gam},{%{EH}Gam.DataGam})
%%]

%%[(8 corerun) hs import({%{EH}CoreRun})
%%]

%%[(8 corerun) hs import(qualified UHC.Util.FastSeq as Seq, qualified Data.Map as Map)
%%]

%%[(8 corerun) hs import(Data.Maybe, Data.Monoid)
%%]

%%[(8 corerun) hs import(Control.Monad, Control.Monad.Error)
%%]

%%[(8888 corerun) hs import(Control.Monad.RWS.Strict)
%%]
%%[(8 corerun) hs import(Control.Monad.State.Strict)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Monad infrastructure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8888 corerun) hs export(RunRd(..), emptyRunRd)
data RunRd
  = RunRd
      {- cenvLamMp       :: LamMp
      -}

emptyRunRd
  = RunRd
%%]

%%[(8888 corerun) hs export(RunWr(..), emptyRunWr)
data RunWr
  = RunWr
      {- cenvLamMp       :: LamMp
      -}

emptyRunWr
  = RunWr

instance Monoid RunWr where
  mappend _ _ = RunWr
  mempty      = emptyRunWr
%%]

%%[(8 corerun) hs export(RunSt(..), emptyRunSt)
data RunSt
  = RunSt
      {- cenvLamMp       :: LamMp
      -}

emptyRunSt
  = RunSt
%%]

%%[(8 corerun) hs export(RunSem(..))
-- | Factored out stuff, not much in it but intended to accomodate variability in running
class (Monad m, MonadIO m, Functor m) => RunSem s m a | s -> a where
  -- | Provide initial state
  rsemInitState :: m s -- RunT' s m s

  -- | Setup whatever needs to be setup
  rsemSetup :: [Mod] -> Mod -> RunT' s m ()

  -- | Exp
  rsemExp :: Exp -> RunT' s m a

  -- | Force evaluation
  rsemEvl :: a -> RunT' s m a
%%]
  -- | Top level module expr startup
  runMod :: RunT' s m a -> RunT' s m a

  -- | Application
  runApp :: RunT' s m a -> RunT' s m (CRArray a) -> RunT' s m a

  -- | Delay by forming a thunk
  runThk :: RunT' s m a -> RunT' s m a

  -- | Empty
  runEmp :: RunT' s m a

  -- | Extract binding
  runRef :: RRef -> RunT' s m a


%%[(8 corerun) hs
%%]

%%[(8 corerun) hs export(RunT', RunT)
-- type RunT' s m a = ErrorT Err (RWST r w s m) a
-- type RunT        m a = RunT' RunRd RunWr RunSt m a
type RunT' s m a = ErrorT Err (StateT s m) a
type RunT    m a = RunT' RunSt m a
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Running
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- For now, only a specialised variant, later do the splitting into abstraction and variation (via classes).
%%[(8 corerun) hs export(runCoreRun)
runCoreRun
  :: (RunSem s m a)
  => EHCOpts
     -> [Mod]
     -> Mod
     -> RunT' s m a
     -> m (Either Err a) -- RunT' s m a
runCoreRun opts modImpL mod r = do
  s <- rsemInitState
  flip evalStateT s $ runErrorT $ do
    rsemSetup modImpL mod
    r
%%]



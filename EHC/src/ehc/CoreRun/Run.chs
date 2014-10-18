%%[0 lhs2tex
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

%%[(8 corerun) hs import({%{EH}CoreRun}, {%{EH}CoreRun.Prim})
%%]

%%[(8 corerun) hs import(qualified UHC.Util.FastSeq as Seq, qualified Data.Map as Map)
%%]

%%[(8 corerun) hs import(UHC.Util.Pretty)
%%]

%%[(8 corerun) hs import(Data.Maybe, Data.Monoid)
%%]

%%[(8 corerun) hs import(Control.Monad, Control.Monad.Error)
%%]

%%[(8 corerun) hs import(Control.Monad.RWS.Strict)
%%]
%%[(8888 corerun) hs import(Control.Monad.State.Strict)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Monad infrastructure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(RunRd(..), emptyRunRd)
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
class (Monad m, MonadIO m, Functor m) => RunSem r s m a | s -> a r, r -> a s where
  -- | Provide initial state
  rsemInitState :: m s
  rsemInitReader :: m r

  -- | Setup whatever needs to be setup
  rsemSetup :: EHCOpts -> [Mod] -> Mod -> RunT' r s m ()

  -- | Set tracing on/off
  rsemSetTrace :: Bool -> RunT' r s m ()
  rsemSetTrace _ = return ()

  -- | Exp
  rsemExp :: Exp -> RunT' r s m a

  -- | SExp
  rsemSExp :: SExp -> RunT' r s m a

  -- | Alt
  rsemAlt :: Alt -> RunT' r s m a

  -- | Force evaluation, subsumes rsemDeref
  rsemEvl :: a -> RunT' r s m a

  -- | Dereference: get rid of intermediate indirections
  rsemDeref :: a -> RunT' r s m a

  -- | Apply primitive to arguments
  rsemPrim :: RunPrim -> CRArray a -> RunT' r s m a
%%]
  -- | Top level module expr startup
  runMod :: RunT' r s m a -> RunT' r s m a

  -- | Application
  runApp :: RunT' r s m a -> RunT' r s m (CRArray a) -> RunT' r s m a

  -- | Delay by forming a thunk
  runThk :: RunT' r s m a -> RunT' r s m a

  -- | Empty
  runEmp :: RunT' r s m a

  -- | Extract binding
  runRef :: RRef -> RunT' r s m a


%%[(8 corerun) hs
%%]

%%[(8 corerun) hs export(RunT', RunT)
-- type RunT' s m a = ErrorT Err (RWST r w s m) a
-- type RunT        m a = RunT' RunRd RunWr RunSt m a
-- type RunT' s m a = ErrorT Err (StateT s m) a
type RunT' r s m a = ErrorT Err (RWST r () s m) a
type RunT      m a = RunT' RunRd RunSt m a
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Error
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(err)
err :: (RunSem r s m a, PP msg) => msg -> RunT' r s m b
err msg = throwError $ rngLift emptyRange Err_PP $ pp msg
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Running
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- For now, only a specialised variant, later do the splitting into abstraction and variation (via classes).
%%[(8 corerun) hs export(runCoreRun)
runCoreRun
  :: (RunSem r s m a)
  => EHCOpts
     -> [Mod]
     -> Mod
     -> RunT' r s m a
     -> m (Either Err a) -- RunT' r s m a
runCoreRun opts modImpL mod m = do
  s <- rsemInitState
  r <- rsemInitReader
  (e,_,_) <-
    runRWST (runErrorT $ do
              rsemSetup opts modImpL mod
              m)
            r s
  return e
%%]



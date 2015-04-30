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

%%[(8 corerun) hs import({%{EH}CoreRun} as CR, {%{EH}CoreRun.Prim})
%%]

%%[(8 corerun) hs import(qualified UHC.Util.FastSeq as Seq, qualified Data.Map as Map)
%%]

%%[(8 corerun) hs import(UHC.Util.Pretty)
%%]

%%[(8 corerun) hs import(Data.Maybe, Data.Monoid)
%%]

%%[(8 corerun) hs import(Data.IORef)
%%]

%%[(8 corerun) hs import(Control.Monad, Control.Monad.Error, Control.Monad.RWS.Strict) export(module Control.Monad.RWS.Strict, module Control.Monad, module Control.Monad.Error)
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
class (Monad m, MonadIO m, Functor m) => RunSem r s v m a
    --- | r -> a s
    --- , s -> a r
    | r s -> v a
    where
  -- | Provide initial state
  rsemInitial :: m (r,s,a)

  -- | Setup whatever needs to be setup
  rsemSetup :: EHCOpts -> [Mod] -> Mod -> RunT' r s v m r

  -- | Setup tracing
  rsemSetupTracing :: EHCOpts -> RunT' r s v m ()
  rsemSetupTracing opts = do
        let dotr  = CoreOpt_RunTrace `elem` ehcOptCoreOpts opts
            dotre = CoreOpt_RunTraceExtensive `elem` ehcOptCoreOpts opts
        rsemSetTrace (dotre || dotr) dotre

  -- | Set tracing on/off, 2nd param to tell to do it extensively
  rsemSetTrace :: Bool -> Bool -> RunT' r s v m ()
  rsemSetTrace _ _ = return ()

  -- | Exp
  rsemExp :: Exp -> RunT' r s v m a

  -- | SExp
  rsemSExp :: SExp -> RunT' r s v m a

  -- | Alt
  rsemAlt :: CR.Alt -> RunT' r s v m a
  rsemAlt a = do
    case a of
      Alt_Alt {expr_Alt_Alt=e} -> rsemExp e
  {-# INLINE rsemAlt #-}

  -- | Force evaluation, subsumes rsemDeref
  rsemEvl :: v -> RunT' r s v m a

  -- | Dereference: get rid of intermediate indirections
  rsemDeref :: v -> RunT' r s v m a

  -- | Apply primitive to arguments
  rsemPrim :: RunPrim -> CRArray v -> RunT' r s v m a

  -- | Push, i.e. lift/put from v to internal machinery
  rsemPush :: v -> RunT' r s v m a
  -- | Pop, i.e. lift/get from internal machinery to v
  rsemPop :: a -> RunT' r s v m v
  -- | Construct a data constr/tuple
  rsemNode :: Int -> CRMArray v -> RunT' r s v m v
  
  -- | GC new level of roots (followed by multiple pushes followed by single pop for all)
  rsemGcEnterRootLevel :: RunT' r s v m ()
  rsemGcEnterRootLevel = return ()
  {-# INLINE rsemGcEnterRootLevel #-}
  -- | GC push as root
  rsemGcPushRoot :: v -> RunT' r s v m ()
  rsemGcPushRoot _ = return ()
  {-# INLINE rsemGcPushRoot #-}
  -- | GC pop as root
  rsemGcLeaveRootLevel :: RunT' r s v m ()
  rsemGcLeaveRootLevel = return ()
  {-# INLINE rsemGcLeaveRootLevel #-}
%%]

%%[(8 corerun) hs export(rsemTopUpd)
-- | Update the top value of the maintained stack
rsemTopUpd :: RunSem r s v m a => (v -> v) -> a -> RunT' r s v m a
rsemTopUpd upd x = rsemPop x >>= (rsemPush . upd)
%%]

%%[(8 corerun) hs export(RunT', RunT)
-- type RunT' s m a = ErrorT Err (RWST r w s m) a
-- type RunT        m a = RunT' RunRd RunWr RunSt m a
-- type RunT' s m a = ErrorT Err (StateT s m) a
type RunT' r s v m a = ErrorT Err (RWST r () s m) a
type RunT      v m a = RunT' RunRd RunSt v m a
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Error
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(err)
err :: (RunSem r s v m a, PP msg) => msg -> RunT' r s v m b
err msg = throwError $ rngLift emptyRange Err_PP $ pp msg
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Running
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- For now, only a specialised variant, later do the splitting into abstraction and variation (via classes).
%%[(8 corerun) hs export(runCoreRun)
runCoreRun
  :: forall r s v m a .
     (RunSem r s v m a)
  => EHCOpts
     -> [Mod]
     -> Mod
     -> RunT' r s v m a
     -> m (Either Err v) -- RunT' r s v m a
runCoreRun opts modImpL mod m = do
  (r, s, _ :: a) <- rsemInitial
  -- let s = error "runCoreRun.RWS"
  --     r = error "runCoreRun.Reader"
  (e, _, _) <-
    runRWST (runErrorT $ do
              r' <- rsemSetup opts modImpL mod
              local (const r') $
                (m >>= rsemPop >>= rsemDeref >>= rsemPop))
            r s
  return e
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utils: monad, IORef
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(modifyIORefM)
modifyIORefM :: IORef a -> (a -> IO a) -> IO ()
modifyIORefM r m = readIORef r >>= m >>= writeIORef r
{-# INLINE modifyIORefM #-}
%%]

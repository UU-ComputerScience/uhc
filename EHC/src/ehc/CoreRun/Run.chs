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

%%[(8 corerun) hs import(Data.Maybe)
%%]

%%[(8 corerun) hs import(Control.Monad, Control.Monad.RWS.Strict, Control.Monad.Error)
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

%%[(8 corerun) hs export(RunWr(..), emptyRunWr)
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
class (Monad m, MonadIO m) => RunSem m a where
  -- | Top level module expr startup
  runMod :: RunT m a -> RunT m a

  -- | Application
  runApp :: RunT m a -> [RunT m a] -> RunT m a

  -- | Force evaluation
  runEvl :: RunT m a -> RunT m a

  -- | Delay by forming a thunk
  runThk :: RunT m a -> RunT m a

  -- | Empty
  runEmp :: RunT m a

  -- | Lift Exp
  runExp :: Exp -> RunT m a

%%]

%%[(8 corerun) hs
%%]

%%[(8 corerun) hs export(RunT)
type RunT m a = ErrorT Err (RWST RunRd RunWr RunSt m) a
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Running
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(cmodRun)
cmodRun
  :: (RunSem m a)
  => EHCOpts
     -> Mod
     -> RunT m a
cmodRun opts mod
  = rMod mod
  where -- module
        rMod m = case m of
          Mod_Mod {body_Mod_Mod=e} -> runMod $ rExp e

        -- expr
        rExp e = case e of
          e -> runExp e
%%]



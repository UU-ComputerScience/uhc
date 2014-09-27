%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Run Core to plainly yield a value
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs module {%{EH}CoreRun.Run.Val}
%%]

%%[(8 corerun) hs import({%{EH}Base.HsName.Builtin},{%{EH}Base.Common},{%{EH}Opts},{%{EH}Ty},{%{EH}Error},{%{EH}Gam},{%{EH}Gam.DataGam})
%%]

%%[(8 corerun) hs import({%{EH}CoreRun}, {%{EH}AbstractCore}, {%{EH}CoreRun.Run})
%%]

%%[(8 corerun) hs import({%{EH}CoreRun.Pretty}, UHC.Util.Pretty)
%%]

%%[(8 corerun) hs import(Control.Monad, Control.Monad.RWS.Strict, Control.Monad.Error)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Value
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs
data RVal
  = RVal_Lam
      { rvalBody		:: !Exp
      }
  deriving Show

instance PP RVal where
  pp rval = case rval of
    RVal_Lam b -> pp b
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RunSem
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs
instance (Monad m, MonadIO m) => RunSem m RVal where
  runMod = undefined -- :: RunT m a -> RunT m a

  runApp f as = do -- :: RunT m a -> [RunT m a] -> RunT m a
    f' <- f
    case f' of
      RVal_Lam {rvalBody=b}
        -> runExp b
      _ -> throwError $ rngLift emptyRange Err_PP $ "App fails:" >#< f'

  runEvl = undefined -- :: RunT m a -> RunT m a

  runThk = undefined -- :: RunT m a -> RunT m a

  runEmp = undefined -- :: RunT m a

  runExp = undefined -- :: Exp -> RunT m a

%%]


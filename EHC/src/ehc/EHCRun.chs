%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Just run an intermediate format, very simplistical wrapper around library running, to be beefed up later
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) module Main
%%]

%%[(8 corerun) import({%{EH}EHC.Main.Utils})
%%]

%%[(8 corerun) import(qualified {%{EH}Config} as Cfg)
%%]

%%[(8 corerun) import({%{EH}Base.API}, {%{EH}CoreRun.API})
%%]

%%[(8 corerun) import({%{EH}Opts})
%%]

%%[(8 corerun) import(UHC.Util.Pretty, UHC.Util.FPath)
%%]

%%[(8 corerun) import(System.Exit, System.Console.GetOpt, System.IO, Control.Monad, System.Environment, Data.List, qualified Data.ByteString.Char8 as B)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main, compiling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun).main
-- | Top level main. TBD: hooks & customization
main :: IO ()
main = do
    args <- getArgs
%%[[99
    progName <- getProgName
%%]]
    let opts0         = defaultEHCOpts
%%[[99
                          {ehcProgName = mkFPath progName}
%%]]
        oo@(o,n,errs) = ehcrunCmdLineOptsApply args opts0
        opts          = maybe opts0 id o
    
    case ehcOptImmQuit opts of
      Just immq     -> handleImmQuitOption ehcrunCmdLineOpts ["rcr"] immq opts
      _             -> case (n,errs) of
        ([fname], []) -> run opts fname
        (_      , es) -> do
          putStr (head errs)
          exitFailure

  where run opts fname = do
            inp <- B.readFile fname
            case parseModFromString $ B.unpack inp of
              Left  es  -> forM_ es putStrLn
              Right mod -> do
                res <- runCoreRunIO opts mod
                case res of
                  Left  e   -> putStrLn $ show $ pp e
                  Right val -> putStrLn $ show $ pp val
{-
      _       ->
        putStrLn $ "Usage: " ++ progName ++ " file.rcr"
-}
%%]


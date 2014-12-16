%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Just run an intermediate format, very simplistical wrapper around library running, to be beefed up later
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) module Main
%%]

%%[(8 corerun) import(qualified {%{EH}Config} as Cfg)
%%]

%%[(8 corerun) import({%{EH}Base.API}, {%{EH}CoreRun.API})
%%]

%%[(8 corerun) import({%{EH}Opts})
%%]

%%[(8 corerun) import(UHC.Util.Pretty)
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
    progName <- getProgName
    let oo@(o,n,errs) = ehcrunCmdLineOptsApply args defaultEHCOpts
        opts          = maybe defaultEHCOpts id o
    
    case ehcOptImmQuit opts of
      Just immq     -> handleImmQuitOption immq opts
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

%%[(8 corerun)
handleImmQuitOption :: ImmediateQuitOption -> EHCOpts -> IO ()
handleImmQuitOption immq opts
  = case immq of
      ImmediateQuitOption_Help
        -> do {
                progName  <- getProgName
              ; let inputSuffixes = ["rcr"]
              ; putStrLn (usageInfo (  "version: " ++ Cfg.verInfo Cfg.version
                                    ++ "\n\nUsage: " ++ progName ++ " [options] [file[" ++ (concat $ intersperse "|" $ map ('.':) inputSuffixes) ++ "]]\n\noptions:"
                                    )
                                    ehcrunCmdLineOpts)
              }
      ImmediateQuitOption_Version
        -> putStrLn $ Cfg.verInfo Cfg.version
%%]

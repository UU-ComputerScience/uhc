%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Just run an intermediate format, very simplistical wrapper around library running, to be beefed up later
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) module Main
%%]

%%[(8 corerun) import({%{EH}Base.API}, {%{EH}CoreRun.API})
%%]

%%[(8 corerun) import(UHC.Util.Pretty)
%%]

%%[(8 corerun) import(System.Console.GetOpt, System.IO, Control.Monad, System.Environment, qualified Data.ByteString.Char8 as B)
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
    
    case args of
      [fname] -> do
        inp <- B.readFile fname
        case parseModFromString $ B.unpack inp of
          Left  es  -> forM_ es putStrLn
          Right mod -> do
            res <- runCoreRunIO defaultEHCOpts mod
            case res of
              Left  e   -> putStrLn $ show $ pp e
              Right val -> putStrLn $ show $ pp val

      _       ->
        putStrLn $ "Usage: " ++ progName ++ " file.rcr"

%%]



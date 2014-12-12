%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Just run an intermediate format, very simplistical wrapper around library running, to be beefed up later
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module Main
%%]

%%[8 import({%{EH}Base.API}, {%{EH}CoreRun.API})
%%]

%%[8 import(UHC.Util.Pretty)
%%]

%%[8 import(System.Console.GetOpt, System.IO, Control.Monad, System.Environment, qualified Data.ByteString.Char8 as B)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main, compiling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8.main
-- | Top level main. TBD: hooks & customization
main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    -- putStrLn $ show args
    
    case args of
      [fname] -> {- withFile fname ReadMode $ \hInp -> -} do
        -- putStrLn "Hi1"
        -- inp <- B.hGetContents hInp
        inp <- B.readFile fname
        -- putStrLn $ show $ B.length inp
        -- putStrLn "Hi2"
        case parseModFromString $ B.unpack inp of
          Left  es  -> forM_ es putStrLn
          Right mod -> do
            res <- runCoreRunIO defaultEHCOpts mod
            case res of
              Left  e   -> putStrLn $ show e
              Right val -> putStrLn $ show $ pp val

      _       ->
        putStrLn $ "Usage: " ++ progName ++ " file.rcr"

%%]



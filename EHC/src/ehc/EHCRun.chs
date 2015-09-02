%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Just run an intermediate format, very simplistical wrapper around library running, to be beefed up later
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module Main
%%]

%%[(8 corerun) import({%{EH}EHC.Main}, {%{EH}EHC.Main.Utils})
%%]

%%[(8 corerun) import({%{EH}Base.API}, {%{EH}CoreRun.API})
%%]

%%[(8 corerun) import(UHC.Util.Pretty, UHC.Util.FPath)
%%]

%%[(8 corerun) import(System.Exit, System.FilePath, System.Console.GetOpt, System.IO, Control.Monad, System.Environment, Data.List, qualified Data.ByteString.Char8 as B)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main, compiling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
-- | Top level main. TBD: hooks & customization
main :: IO ()
main = do
%%[[(8 corerun)
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
      Just immq     -> handleImmQuitOption ehcrunCmdLineOpts ["rcr", "tcrr", "cr", "bcr", "tcr"] immq opts
      _             -> case (n,errs) of
        ([fname], []) -> do
          let (bname,ext) = splitExtension fname
          case ext of
            -- ".tcrr" -> runRCR opts fname
            ".crr"  -> runRCR opts fname
            e | e `elem` [".cr", ".bcr", ".tcr", ".tcrr"]
                   -> mainEHC $ opts
                        { ehcOptMbTarget = JustOk Target_None_Core_AsIs
                        , ehcOptCoreOpts = {- CoreOpt_RunTrace : -} CoreOpt_Run : CoreOpt_LoadOnly : ehcOptCoreOpts opts
%%[[50
                        , ehcOptOptimizationScope = OptimizationScope_WholeCore
%%]]
                        , ehcOptVerbosity = VerboseQuiet
                        , ehcOptAltDriver = not $ ehcOptAltDriver opts
                        }
            _      -> return ()
        (_      , es) -> do
          putStr (head errs)
          exitFailure

  where runRCR opts fname = do
            inp <- B.readFile fname
            case parseModFromString $ B.unpack inp of
              Left  es  -> forM_ es putStrLn
              Right mod -> do
                res <- runCoreRunIO opts mod
                case res of
                  Left  e   -> putStrLn $ show $ pp e
                  Right val -> putStrLn $ show $ pp val
%%][1
    putStrLn "Not installed, CoreRun must be enabled"
%%]]
%%]




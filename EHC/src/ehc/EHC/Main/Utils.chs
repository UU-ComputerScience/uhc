%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}EHC.Main.Utils}
%%]

%%[1 import({%{EH}EHC.Common}, {%{EH}EHC.FileSuffMp}) export(module {%{EH}EHC.FileSuffMp})
%%]

%%[8 import({%{EH}Base.Target}, {%{EH}Base.Optimize}(allOptimizeMp))
%%]

%%[1 import(qualified {%{EH}Config} as Cfg)
%%]
%%[1 import({%{EH}Opts}) export(module {%{EH}Opts})
%%]
%%[50 import(qualified {%{EH}SourceCodeSig} as Sig)
%%]

%%[1 import(System.Console.GetOpt, System.Environment)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handling of immediate quit options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(handleImmQuitOption)
-- | Handle a commandline option directly
handleImmQuitOption :: GetOptCmdLineOpts -> [String] -> ImmediateQuitOption -> EHCOpts -> IO ()
handleImmQuitOption cmdLineOpts inputSuffixes immq opts
  = case immq of
      ImmediateQuitOption_Help
        -> do {
%%[[1
                progName  <- getProgName
%%][99
                let progName = fpathToStr (ehcProgName opts)
%%]]
              ; putStrLn (usageInfo (  "version: " ++ Cfg.verInfo Cfg.version ++ ", aspects: " ++ ehcOptAspects opts
                                    ++ "\n\nUsage: " ++ progName ++ " [options] [file[" ++ (concat $ intersperse "|" $ map ('.':) inputSuffixes) ++ "] ...]\n\noptions:"
                                    )
                                    cmdLineOpts)
              }
      ImmediateQuitOption_Version
        -> putStrLn $ Cfg.verInfo Cfg.version
%%[[50
                      ++ ", timestamp " ++ Sig.timestamp
%%]]
      ImmediateQuitOption_Meta_Variant
        -> putStrLn Cfg.ehcDefaultVariant
%%[[1
      ImmediateQuitOption_Meta_Targets
        -> putStr ""
      ImmediateQuitOption_Meta_TargetDefault
        -> putStr "no-target"
%%][(8 codegen)
      ImmediateQuitOption_Meta_Targets
        -> putStr showSupportedTargets
      ImmediateQuitOption_Meta_TargetDefault
        -> putStr (show defaultTarget)
      ImmediateQuitOption_Meta_Optimizations
        -> putStr (showStringMapKeys allOptimizeMp " ")
%%]]
%%[[99
      ImmediateQuitOption_VersionDotted
        -> putStrLn (Cfg.verFull Cfg.version)
      ImmediateQuitOption_VersionAsNumber
        -> putStrLn (Cfg.verAsNumber Cfg.version)
{-
      ImmediateQuitOption_Meta_ExportEnv mvEnvOpt
        -> exportEHCEnvironment
             (mkEhcenvKey (Cfg.verFull Cfg.version) (fpathToStr $ ehcProgName opts) Cfg.ehcDefaultVariant)
             (env {ehcenvInstallRoot = installRootDir, ehcenvVariant = variant})
        where env = ehcOptEnvironment opts
              (installRootDir,variant)
                = case fmap (wordsBy (`elem` ",;")) mvEnvOpt of
                    Just (d:v:_) -> (d,v)
                    Just (d:_)   -> (d,ehcenvVariant env)
                    _            -> (ehcenvInstallRoot env,ehcenvVariant env)
      ImmediateQuitOption_Meta_DirEnv
        -> do { d <- ehcenvDir (mkEhcenvKey (Cfg.verFull Cfg.version) (fpathToStr $ ehcProgName opts) Cfg.ehcDefaultVariant)
              ; putStrLn d
              }
-}
      ImmediateQuitOption_Meta_Pkgdir_System
        -> do { let d = Cfg.mkInstallPkgdirSystem opts
              ; putStrLn d
              }
      ImmediateQuitOption_Meta_Pkgdir_User
        -> do { let d = Cfg.mkInstallPkgdirUser opts
              ; putStrLn d
              }
%%]]
%%]


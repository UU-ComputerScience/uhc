%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

JScript compilation

%%[(8 codegen jscript) module {%{EH}EHC.CompilePhase.CompileJScript}
%%]

%%[(8 codegen jscript) import(System.Directory)
%%]

-- general imports
%%[(8 codegen jscript) import({%{EH}EHC.Common})
%%]
%%[(8 codegen jscript) import({%{EH}EHC.CompileUnit})
%%]
%%[(8 codegen jscript) import({%{EH}EHC.CompileRun})
%%]

%%[(8 codegen jscript) import(qualified {%{EH}Config} as Cfg)
%%]
%%[(8 codegen jscript) import({%{EH}EHC.Environment})
%%]
%%[(8 codegen jscript) import({%{EH}Base.Target})
%%]

%%[(8 codegen jscript) import({%{EH}Core.ToJScript})
%%]
%%[(8 codegen jscript) import({%{EH}Base.Bits},{%{EH}JScript.Pretty})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: JScript linking
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen jscript) export(cpJScript)
cpJScript :: String -> [String] -> EHCompilePhase ()
cpJScript archive files
  = do { cr <- get
       ; let (_,opts) = crBaseInfo' cr
             cmd = mkShellCmd $ [Cfg.shellCmdCat] ++ files ++ [">", archive]
       ; when (ehcOptVerbosity opts >= VerboseALot) (lift $ putStrLn cmd)
       ; cpSystem cmd 
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: JScript compilation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen jscript) export(cpCompileJScript)
cpCompileJScript :: FinalCompileHow -> [HsName] -> HsName -> EHCompilePhase ()
cpCompileJScript how othModNmL modNm
  = do { cr <- get
       ; let  (ecu,_,opts,fp) = crBaseInfo modNm cr
              mbJs            = ecuMbJScript ecu
              fpO m f = mkPerModuleOutputFPath opts True m f Cfg.suffixJScriptLib
              fpM     = fpO modNm fp
              fpExec  = mkPerExecOutputFPath opts modNm fp (Just "js")
       ; when (isJust mbJs && targetIsJScript (ehcOptTarget opts))
              (do { cpMsg modNm VerboseALot "Emit JScript"
                  ; when (ehcOptVerbosity opts >= VerboseDebug)
                         (do { lift $ putStrLn $ "fpO   : " ++ fpathToStr fpM
                             ; lift $ putStrLn $ "fpExec: " ++ fpathToStr fpExec
                             })
                  ; let ppMod = ppJScriptModule (fromJust mbJs)
                  ; lift $ putPPFPath fpM ("//" >#< modNm >-< ppMod) 1000
                  ; case how of
                      FinalCompile_Exec
                        -> do { cpJScript (fpathToStr fpExec) (rts ++ (map fpathToStr $ oth ++ [fpM]))
                              }
                        where rts = map (Cfg.mkInstalledRts opts Cfg.mkJScriptLibFilename Cfg.INST_LIB (Cfg.installVariant opts)) Cfg.libnamesRts
%%[[8
                              oth = []
%%][20
                              oth | ehcOptWholeProgOptimizationScope opts = []
                                  | otherwise                             = [ fpO m fp | m <- othModNmL, let (_,_,_,fp) = crBaseInfo m cr ]
%%]]
                      _ -> return ()
                  }
              )
       }                 
%%]




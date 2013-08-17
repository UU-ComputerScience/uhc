%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile Cmm
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Cmm compilation

%%[(8 codegen cmm) module {%{EH}EHC.CompilePhase.CompileCmm}
%%]

%%[(88 codegen cmm) import(System.Directory, Data.List(intercalate), Data.Either, System.Exit)
%%]

-- general imports
%%[(88 codegen cmm) import({%{EH}EHC.Common})
%%]
%%[(88 codegen cmm) import({%{EH}EHC.CompileUnit})
%%]
%%[(88 codegen cmm) import({%{EH}EHC.CompileRun})
%%]

%%[(88 codegen cmm) import(qualified {%{EH}Config} as Cfg)
%%]
%%[(88 codegen cmm) import({%{EH}EHC.Environment})
%%]
%%[(88 codegen cmm) import({%{EH}Base.Target})
%%]

%%[(8 codegen cmm) import({%{EH}Cmm.ToC})
%%]
%%[(8 codegen cmm javascript) import({%{EH}Cmm.ToJavaScript})
%%]
%%[(88 codegen cmm) import({%{EH}Base.Bits},{%{EH}Cmm.Pretty})
%%]

%%[(8 codegen cmm)
-- dummy
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: Cmm linking
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(88 codegen cmm) export(cpCmm)
cpCmm :: String -> [String] -> EHCompilePhase ()
cpCmm archive files
  = do { cr <- get
       ; let (_,opts) = crBaseInfo' cr
             cmd = mkShellCmd $ [Cfg.shellCmdCat] ++ files -- ++ [">", archive]
       ; when (ehcOptVerbosity opts >= VerboseALot) (lift $ putStrLn $ showShellCmd cmd)
       ; cpSystem' (Just archive) cmd 
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: Cmm compilation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(88 codegen cmm) export(cpCompileCmm)
cpCompileCmm :: FinalCompileHow -> [HsName] -> HsName -> EHCompilePhase ()
cpCompileCmm how othModNmL modNm
  = do { cr <- get
       ; let  (ecu,_,opts,fp) = crBaseInfo modNm cr
              mbJs            = ecuMbCmm ecu
              fpO m f = mkPerModuleOutputFPath opts True m f Cfg.suffixCmmLib
              fpM     = fpO modNm fp
              fpExec  = mkPerExecOutputFPath opts modNm fp (Just "js")
              fpHtml  = mkPerExecOutputFPath opts modNm fp (Just "html")

       ; when (isJust mbJs && targetIsCmm (ehcOptTarget opts))
              (do { cpMsg modNm VerboseALot "Emit Cmm"
                  ; when (ehcOptVerbosity opts >= VerboseDebug)
                         (do { lift $ putStrLn $ "fpO   : " ++ fpathToStr fpM
                             ; lift $ putStrLn $ "fpExec: " ++ fpathToStr fpExec
                             ; lift $ putStrLn $ show (ehcOptImportFileLocPath opts)
                             ; lift $ putStrLn $ "module dependencies:" ++ intercalate "," (jsModDeps (fromJust mbJs))
                             })
%%[[8
                  ; let ppMod = ppCmmModule (fromJust mbJs)
%%][50
                  ; let ppMod = vlist $ [p] ++ (if ecuIsMainMod ecu then [pmain] else [])
                              where (p,pmain) = ppCmmModule (fromJust mbJs)
%%]]
                  ; let fpDeps  = map fpathFromStr (jsModDeps (fromJust mbJs))
                  ; let searchPath = ehcOptImportFileLocPath opts

                  ; jsDepsFound <- jsDepsToFPaths searchPath fpDeps
                  
                  ; let someJsDepsNotFound = either (const True) (const False)

                  ; when (someJsDepsNotFound jsDepsFound) 
                         (do { let Left notFound = jsDepsFound
                             ; err $ "Could not find external js dependencies: " ++ intercalate "," (map fpathToStr notFound)
                             })

                  ; let Right jsDeps = jsDepsFound

                  ; lift $ putPPFPath fpM ("//" >#< modNm >-< ppMod) 1000
                  ; case how of
                      FinalCompile_Exec
%%[[50
                        | ehcOptWholeProgOptimizationScope opts
                        -> do { cpCmm (fpathToStr fpExec) (rts ++ [fpathToStr fpM])
                              ; mkHtml fpHtml ((map fpathToStr jsDeps) ++ [fpathToStr fpExec])
                              }
%%]]
                        | otherwise
                        -> do { cpCmm (fpathToStr fpExec) [fpathToStr fpM]
                              ; mkHtml fpHtml $ ( map fpathToStr jsDeps )
                                               ++ rts 
                                               ++ [ fpathToStr (fpO m fp) | m <- othModNmL, let (_,_,_,fp) = crBaseInfo m cr ] 
                                               ++ [ fpathToStr fpExec ]
                              }
                        where rts = map (Cfg.mkInstalledRts opts Cfg.mkCmmLibFilename Cfg.INST_LIB (Cfg.installVariant opts)) Cfg.libnamesRts
%%[[8
                              oth = []
%%][50
                              oth | ehcOptWholeProgOptimizationScope opts = []
                                  | otherwise                             = [ fpO m fp | m <- othModNmL, let (_,_,_,fp) = crBaseInfo m cr ]
%%]]
                      _ -> return ()
                  }
              )
       }
  where mkHtml fpHtml jsL
          = lift $ putPPFPath fpHtml (ppHtml) 1000
          where scr x = "<script type=\"text/javascript\" src=\"" >|< x >|< "\"></script>"
                ppHtml
                  = "<!DOCTYPE html><html><head><title>" >|< modNm >|< "</title>"
                    >-< vlist (map scr jsL)
                    >-< "</head>"
                    >-< "<body>"
                    >-< "</body>"
                    >-< "</html>"

        findJsDep :: FileLocPath -> FPath -> EHCompilePhase (Maybe FPath)
        findJsDep searchPath dep = lift $ searchPathForReadableFile (map filelocDir searchPath) [Just "js"] dep

        jsDepsToFPaths :: FileLocPath -> [FPath] -> EHCompilePhase (Either [FPath] [FPath])
        jsDepsToFPaths searchPath deps = do
          paths <- mapM (\dep -> do {
                    ; depFound <- findJsDep searchPath dep
                    ; maybe (return $ Left dep) (return . Right) depFound
                    }) deps

          let allLeft          = lefts paths
              hasUnfoundJsDeps = not $ null $ allLeft

          if hasUnfoundJsDeps
            then return $ Left allLeft
            else return $ Right $ rights paths

        err :: String -> EHCompilePhase ()
        err x = do 
          lift $ hPutStrLn stderr ("error: " ++ x)
          lift $ exitFailure
%%]




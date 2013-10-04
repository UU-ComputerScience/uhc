%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile Javascript
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

JavaScript compilation

%%[(8 codegen javascript) module {%{EH}EHC.CompilePhase.CompileJavaScript}
%%]

%%[(8 codegen javascript) import(System.Directory, Data.List(intercalate), Data.Either, System.Exit)
%%]
%%[(8 codegen javascript) import(Control.Monad.State)
%%]

-- general imports
%%[(8 codegen javascript) import({%{EH}EHC.Common})
%%]
%%[(8 codegen javascript) import({%{EH}EHC.CompileUnit})
%%]
%%[(8 codegen javascript) import({%{EH}EHC.CompileRun})
%%]
%%[(8 codegen javascript) import({%{EH}EHC.CompilePhase.Output})
%%]

%%[(8 codegen javascript) import(qualified {%{EH}Config} as Cfg)
%%]
%%[(8 codegen javascript) import({%{EH}EHC.Environment})
%%]
%%[(8 codegen javascript) import({%{EH}Base.Target})
%%]

%%[(8 codegen javascript) import({%{EH}Core.ToJavaScript})
%%]
%%[(8 codegen javascript) import({%{EH}Base.Bits},{%{EH}JavaScript.Pretty})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: JavaScript linking
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen javascript) export(cpJavaScript)
cpJavaScript :: String -> [String] -> EHCompilePhase ()
cpJavaScript archive files
  = do { cr <- get
       ; let (_,opts) = crBaseInfo' cr
             cmd = mkShellCmd $ [Cfg.shellCmdCat] ++ files -- ++ [">", archive]
       ; when (ehcOptVerbosity opts >= VerboseALot) (lift $ putStrLn $ showShellCmd cmd)
       ; cpSystem' (Just archive) cmd 
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: JavaScript compilation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen javascript) export(cpCompileJavaScript)
cpCompileJavaScript :: FinalCompileHow -> [HsName] -> HsName -> EHCompilePhase ()
cpCompileJavaScript how othModNmL modNm
  = do { cr <- get
       ; let  (ecu,_,opts,fp) = crBaseInfo modNm cr
              mbJs            = ecuMbJavaScript ecu
              fpO m f = outputMkFPathJavaScriptModule opts m f Cfg.suffixJavaScriptLib
              -- fpM     = fpO modNm fp
              fpExec  = mkPerExecOutputFPath opts modNm fp (Just "js")
              fpHtml  = mkPerExecOutputFPath opts modNm fp (Just "html")

       ; when (isJust mbJs && ehcOptEmitJavaScript opts)
              (do { when (ehcOptVerbosity opts >= VerboseDebug)
                         (do { lift $ putStrLn $ "fpExec: " ++ fpathToStr fpExec
                             ; lift $ putStrLn $ show (ehcOptImportFileLocPath opts)
                             ; lift $ putStrLn $ "module dependencies:" ++ intercalate "," (jsModDeps (fromJust mbJs))
                             })

                  ; let fpDeps  = map fpathFromStr (jsModDeps (fromJust mbJs))
                  ; let searchPath = ehcOptImportFileLocPath opts

                  ; jsDepsFound <- jsDepsToFPaths searchPath fpDeps
                  
                  ; let someJsDepsNotFound = either (const True) (const False)

                  ; when (someJsDepsNotFound jsDepsFound) 
                         (do { let Left notFound = jsDepsFound
                             ; err $ "Could not find external js dependencies: " ++ intercalate "," (map fpathToStr notFound)
                             })

                  ; let Right jsDeps = jsDepsFound

                  -- ; lift $ putPPFPath fpM ("//" >#< modNm >-< ppMod) 1000
                  ; fpM <- cpOutputJavaScript False "" modNm
                  
                  ; case how of
                      FinalCompile_Exec
%%[[50
                        | ehcOptWholeProgOptimizationScope opts
                        -> do { cpJavaScript (fpathToStr fpExec) (rts ++ [fpathToStr fpM])
                              ; mkHtml fpHtml ((map fpathToStr jsDeps) ++ [fpathToStr fpExec])
                              }
%%]]
                        | otherwise
                        -> do { cpJavaScript (fpathToStr fpExec) [fpathToStr fpM]
                              ; mkHtml fpHtml $ ( map fpathToStr jsDeps )
                                               ++ rts 
                                               ++ [ fpathToStr (fpO m fp) | m <- othModNmL, let (_,_,_,fp) = crBaseInfo m cr ] 
                                               ++ [ fpathToStr fpExec ]
                              }
                        where rts = map (Cfg.mkInstalledRts opts Cfg.mkJavaScriptLibFilename Cfg.INST_LIB (Cfg.installVariant opts)) Cfg.libnamesRts
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




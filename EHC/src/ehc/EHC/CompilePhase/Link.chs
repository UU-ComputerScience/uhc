%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Linking

%%[(99 codegen) module {%{EH}EHC.CompilePhase.Link}
%%]

-- general imports
%%[(99 codegen) import({%{EH}EHC.Common})
%%]
%%[(99 codegen) import({%{EH}EHC.CompileUnit})
%%]
%%[(99 codegen) import({%{EH}EHC.CompileRun})
%%]

%%[(99 codegen) import(qualified {%{EH}Config} as Cfg)
%%]
%%[(99 codegen) import({%{EH}EHC.Environment})
%%]
%%[(99 codegen) import({%{EH}Base.Target})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: Linking into library for package
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 codegen) export(cpLinkO)
cpLinkO :: [HsName] -> String -> EHCompilePhase ()
cpLinkO modNmL pkgNm
  = do { cr <- get
       ; let (crsi,opts) = crBaseInfo' cr
             oFiles = [ fpathToStr o | m <- modNmL, o <- ecuGenCodeFiles $ crCU m cr ]
             libFile= mkOutputFPath opts l l (fpathSuff l)
                    where l = mkFPath $ Cfg.mkLibFilename "" pkgNm
             linkO  = map mkShellCmd $ Cfg.mkShellCmdLibtool (fpathToStr libFile) oFiles
       ; when (ehcOptVerbosity opts >= VerboseALot)
              (do { lift $ mapM_ putStrLn linkO
                  })
       ; cpSeq [ cpSystem c | c <- linkO ]
       }
%%]

cpCompileWithGCC :: GCC_CompileHow -> [HsName] -> HsName -> EHCompilePhase ()
cpCompileWithGCC how othModNmL modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 fpC    = mkOutputFPath opts modNm fp "c"
                 fpO m f= mkOutputFPath opts m f "o"
                 fpExec = maybe (mkOutputFPath opts modNm fp "") (\s -> mkOutputFPath opts modNm fp s) Cfg.mbSuffixExec
                 variant= ehcenvVariant (ehcOptEnvironment opts)
                 (fpTarg,targOpt,linkOpts,linkLibOpt,dotOFilesOpt)
                        = case how of
                            GCC_CompileExec -> ( fpExec
                                               , [ Cfg.gccOpts, "-o", fpathToStr fpExec ]
                                               , Cfg.ehcGccOptsStatic
                                               , map (\l -> Cfg.mkInstallFilePrefix opts Cfg.LIB variant ++ "lib" ++ l ++ ".a") Cfg.libnamesGccPerVariant
                                                 ++ map (\l -> Cfg.mkInstallFilePrefix opts Cfg.LIB_SHARED variant ++ "lib" ++ l ++ ".a") Cfg.libnamesGcc
                                                 ++ map ("-l" ++) Cfg.libnamesGccEhcExtraExternalLibs
                                               , if   ehcOptFullProgAnalysis opts
                                                 then [ ]
                                                 else [ fpathToStr $ fpO m fp | m <- othModNmL, let (_,_,_,fp) = crBaseInfo m cr ]
                                               )
                            GCC_CompileOnly -> (o, [ Cfg.gccOpts, "-c", "-o", fpathToStr o ], Cfg.ehcGccOptsStatic, [], [])
                                            where o = fpO modNm fp
         ;  when (targetIsC (ehcOptTarget opts))
                 (do { let compileC
                             = mkShellCmd
                               $ (  [ Cfg.shellCmdGcc ]
                                 ++ [ "-I" ++ Cfg.mkInstallFilePrefix opts Cfg.INCLUDE variant ]
                                 ++ [ "-I" ++ Cfg.mkInstallFilePrefix opts Cfg.INCLUDE_SHARED variant ]
                                 ++ linkOpts
                                 ++ targOpt
                                 ++ dotOFilesOpt
                                 ++ [ fpathToStr fpC ]
                                 ++ [ Cfg.mkInstallFilePrefix opts Cfg.INCLUDE variant ++ "mainSil.c"
                                    | ehcOptTarget opts == Target_FullProgAnal_Grin_C
                                    ]
                                 ++ linkLibOpt
                                 )
                     ; when (ehcOptVerbosity opts >= VerboseALot)
                            (do { cpMsg' modNm VerboseALot "GCC" Nothing fpTarg
                                ; lift $ putStrLn compileC
                                })
                     ; cpSystem compileC
                     })
         }

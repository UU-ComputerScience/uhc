%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

C + CPP compilation

%%[8 module {%{EH}EHC.CompilePhase.CompileC}
%%]

-- general imports
%%[8 import({%{EH}EHC.Common})
%%]
%%[8 import({%{EH}EHC.CompileUnit})
%%]
%%[8 import({%{EH}EHC.CompileRun})
%%]

%%[8 import(qualified {%{EH}Config} as Cfg)
%%]
%%[8 import({%{EH}EHC.Environment})
%%]
%%[(8 codegen) import({%{EH}Base.Target})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: C compilation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(GCC_CompileHow(..))
data GCC_CompileHow
  = GCC_CompileOnly
  | GCC_CompileExec
%%]

%%[(8 codegen) export(cpCompileWithGCC)
cpCompileWithGCC :: GCC_CompileHow -> [HsName] -> HsName -> EHCompilePhase ()
cpCompileWithGCC how othModNmL modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 fpC    = fpathSetSuff "c" fp
                 fpO fp = fpathSetSuff "o" fp
                 fpExec = maybe (fpathRemoveSuff fp) (\s -> fpathSetSuff s fp) Cfg.mbSuffixExec
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
                                                 else [ fpathToStr $ fpO fp | m <- othModNmL, let (_,_,_,fp) = crBaseInfo m cr ]
                                               )
                            GCC_CompileOnly -> (o, [ Cfg.gccOpts, "-c", "-o", fpathToStr o ], Cfg.ehcGccOptsStatic, [], [])
                                            where o = fpO fp
         ;  when (targetIsC (ehcOptTarget opts))
                 (do { let compileC
                             = concat $ intersperse " "
                               $ (  [ Cfg.shellCmdGcc ]
                                 ++ [ "-I" ++ Cfg.mkInstallFilePrefix opts Cfg.INCLUDE variant ]
                                 ++ [ "-I" ++ Cfg.mkInstallFilePrefix opts Cfg.INCLUDE_SHARED variant ]
                                 ++ linkOpts
                                 ++ targOpt
                                 ++ dotOFilesOpt
                                 ++ [ fpathToStr fpC ]
%%[[(8 codegen grin)
                                 ++ [ Cfg.mkInstallFilePrefix opts Cfg.INCLUDE variant ++ "mainSil.c"
                                    | ehcOptTarget opts == Target_FullProgAnal_Grin_C
                                    ]
%%]]
                                 ++ linkLibOpt
                                 )
                     ; when (ehcOptVerbosity opts >= VerboseALot)
                            (do { cpMsg' modNm VerboseALot "GCC" Nothing fpTarg
                                ; lift $ putStrLn compileC
                                })
                     ; cpSystem compileC
                     })
         }
%%]

%%[99 export(cpPreprocessWithCPP)
cpPreprocessWithCPP :: HsName -> EHCompilePhase ()
cpPreprocessWithCPP modNm
  = do { cr <- get
       ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
              fpCPP = fpathSetSuff (maybe "" (\s -> s ++ "-") (fpathMbSuff fp) ++ "cpp") fp
       ; when (  ehcOptCPP opts
              || modNm == hsnModIntlPrelude		-- 20080211, AD: builtin hack to preprocess EHC.Prelude with cpp, for now, to avoid implementation of pragmas
              )
              (do { let preCPP
                          = concat $ intersperse " "
                            $ (  [ Cfg.shellCmdCpp ]
                              ++ [ "-traditional-cpp", "-fno-show-column", "-P", "-D__EHC__" ]
%%[[(99 codegen grin)
                              ++ (if ehcOptFullProgAnalysis opts then ["-D__FULL_PROGRAM_ANALYSIS__"] else [])
%%]]
                              ++ [ fpathToStr fp, fpathToStr fpCPP ]
                              )
                  ; when (ehcOptVerbosity opts >= VerboseALot)
                         (do { cpMsg modNm VerboseALot "CPP"
                             ; lift $ putStrLn preCPP
                             })
                  ; when (crModCanCompile modNm cr)
                         (cpSystem preCPP)
                  ; cpUpdCU modNm (ecuStoreFilePath fpCPP)
                  })
       }
%%]




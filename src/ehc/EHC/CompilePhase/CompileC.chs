%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

C + CPP compilation

%%[8 module {%{EH}EHC.CompilePhase.CompileC}
%%]

-- general imports
%%[8 import(Data.Char)
%%]
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

%%[(8 codegen)
gccDefs :: EHCOpts -> [String]
gccDefs opts 
  = map (\d -> "-D__" ++ d ++ "__")
    $  [ "EHC", "EHC_TARGET_" ++ (map toUpper $ show $ ehcOptTarget opts) ]
%%[[(99 codegen grin)
    ++ (if ehcOptFullProgAnalysis opts then ["EHC_FULL_PROGRAM_ANALYSIS"] else [])
%%]]
%%]

%%[(8 codegen) export(cpCompileWithGCC)
cpCompileWithGCC :: FinalCompileHow -> [HsName] -> HsName -> EHCompilePhase ()
cpCompileWithGCC how othModNmL modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 fpC    = mkOutputFPath opts modNm fp "c"
                 fpO m f= mkOutputFPath opts m f "o"
                 fpExec = maybe (mkOutputFPath opts modNm fp "") (\s -> mkOutputFPath opts modNm fp s) Cfg.mbSuffixExec
                 variant= ehcenvVariant (ehcOptEnvironment opts)
                 (fpTarg,targOpt,linkOpts,linkLibOpt,dotOFilesOpt,genOFiles)
                        = case how of
                            FinalCompile_Exec
                              -> ( fpExec
                                 , [ Cfg.gccOpts, "-o", fpathToStr fpExec ]
                                 , Cfg.ehcGccOptsStatic
                                 , map (\l -> Cfg.mkInstallFilePrefix opts Cfg.LIB variant "" ++ "lib" ++ l ++ ".a")
                                       ((if ehcOptFullProgAnalysis opts then [] else pkgNmL) ++ Cfg.libnamesGccPerVariant)
                                   ++ map (\l -> Cfg.mkInstallFilePrefix opts Cfg.LIB_SHARED variant "" ++ "lib" ++ l ++ ".a") Cfg.libnamesGcc
                                   ++ map ("-l" ++) Cfg.libnamesGccEhcExtraExternalLibs
                                 , if   ehcOptFullProgAnalysis opts
                                   then [ ]
                                   else [ fpathToStr $ fpO m fp | m <- othModNmL2, let (_,_,_,fp) = crBaseInfo m cr ]
                                 , []
                                 )
                            FinalCompile_Module
                              -> (o, [ Cfg.gccOpts, "-c", "-o", fpathToStr o ], Cfg.ehcGccOptsStatic, [], [], [o])
                              where o = fpO modNm fp
%%[[8
                 pkgNmL     = [] :: [String]
                 othModNmL2 = othModNmL
%%][99
                 (pkgNmL,othModNmL2) = crPartitionIntoPkgAndOthers cr othModNmL
%%]]
         ;  when (targetIsC (ehcOptTarget opts))
                 (do { let compileC
                             = mkShellCmd
                                 (  [ Cfg.shellCmdGcc ]
                                 ++ gccDefs opts
                                 ++ [ "-I" ++ Cfg.mkInstallFilePrefix opts Cfg.INCLUDE variant "" ]
                                 ++ [ "-I" ++ Cfg.mkInstallFilePrefix opts Cfg.INCLUDE_SHARED variant "" ]
                                 ++ linkOpts
                                 ++ targOpt
                                 ++ dotOFilesOpt
                                 ++ [ fpathToStr fpC ]
%%[[(8 codegen grin)
                                 ++ [ Cfg.mkInstallFilePrefix opts Cfg.INCLUDE variant "" ++ "mainSil.c"
                                    | ehcOptTarget opts == Target_FullProgAnal_Grin_C
                                    ]
%%]]
                                 ++ linkLibOpt
                                 )
                     ; when (ehcOptVerbosity opts >= VerboseALot)
                            (do { cpMsg' modNm VerboseALot "GCC" Nothing fpTarg
                                ; lift $ putStrLn compileC
                                })
                     ; when (ehcOptVerbosity opts >= VerboseDebug)
                            (do { lift $ putStrLn ("pkgs : " ++ show pkgNmL)
                                ; lift $ putStrLn ("other: " ++ show othModNmL2)
                                })
                     ; cpSystem compileC
%%[[99
                     ; cpUpdCU modNm (ecuStoreGenCodeFiles genOFiles)
%%]]
                     })
         }
%%]

%%[99 export(cpPreprocessWithCPP)
cpPreprocessWithCPP :: HsName -> EHCompilePhase ()
cpPreprocessWithCPP modNm
  = do { cr <- get
       ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
              fpCPP = fpathSetSuff {- mkOutputFPath opts modNm fp -} (maybe "" (\s -> s ++ "-") (fpathMbSuff fp) ++ "cpp") fp
       ; when (  ehcOptCPP opts
              || modNm == hsnModIntlPrelude      -- 20080211, AD: builtin hack to preprocess EHC.Prelude with cpp, for now, to avoid implementation of pragmas
              )
              (do { let inclDirs= [ mk kind dir | FileLoc kind dir <- ehcOptImportFileLocPath opts, not (null dir) ]
                                where mk (FileLocKind_Dir  ) d = d
                                      mk (FileLocKind_Pkg _) d = Cfg.mkPkgIncludeDir $ filePathMkPrefix d
                        defs    = [ "EHC", "TARGET_" ++ (map toUpper $ show $ ehcOptTarget opts) ]
%%[[(99 codegen grin)
                                  ++ (if ehcOptFullProgAnalysis opts then ["EHC_FULL_PROGRAM_ANALYSIS"] else [])
%%]]
                        preCPP  = mkShellCmd
                                    (  [ Cfg.shellCmdCpp ]
                                    ++ gccDefs opts
                                    ++ [ "-traditional-cpp", {- "-std=gnu99", -} "-fno-show-column", "-P" ]
%%[[(99 codegen)
                                    ++ [ "-I" ++ d | d <- inclDirs ]
%%]]
                                    ++ [ fpathToStr fp, fpathToStr fpCPP ]
                                    )
                  ; when (ehcOptVerbosity opts >= VerboseALot)
                         (do { cpMsg modNm VerboseALot "CPP"
                             ; lift $ putStrLn preCPP
                             })
                  ; when (crModCanCompile modNm cr)
                         (do { lift $ fpathEnsureExists fpCPP
                             ; cpSystem preCPP
%%[[99
                             ; cpRegisterFilesToRm [fpCPP]
%%]]
                             })
                  ; cpUpdCU modNm (ecuStoreFilePath fpCPP)
                  })
       }
%%]




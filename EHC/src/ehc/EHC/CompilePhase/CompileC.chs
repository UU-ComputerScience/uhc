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
gccDefs :: EHCOpts -> [String] -> [String]
gccDefs opts builds
  = map (\(d,mbval) -> "-D__UHC" ++ d ++ "__" ++ maybe "" ("=" ++) mbval)
      $  [ (""                                                    , Just (Cfg.verAsNumber Cfg.version))
         , ("_TARGET_" ++ (map toUpper $ show $ ehcOptTarget opts), Nothing                           )
         ]
      ++ map (\x -> ("_BUILDS_" ++ x, Nothing))
             builds
      ++ map (\x -> (x,Nothing))
             [ "_" ++ map (\c -> case c of {'.' -> '_'; c -> c}) (Cfg.verFull Cfg.version) ]
%%[[(99 codegen grin)
    --  ++ (if ehcOptFullProgAnalysis opts then ["_FULL_PROGRAM_ANALYSIS"] else [])
%%]]
%%]

%%[(99 codegen)
gccInclDirs :: EHCOpts -> [String]
gccInclDirs opts 
  = [ mk kind dir | FileLoc kind dir <- ehcOptImportFileLocPath opts, not (null dir) ]
  where mk (FileLocKind_Dir  ) d = d
        mk (FileLocKind_Pkg _) d = Cfg.mkPkgIncludeDir $ filePathMkPrefix d
        mk  FileLocKind_PkgDb  d = Cfg.mkPkgIncludeDir $ filePathMkPrefix d
%%]

%%[(8 codegen) export(cpCompileWithGCC)
cpCompileWithGCC :: FinalCompileHow -> [HsName] -> HsName -> EHCompilePhase ()
cpCompileWithGCC how othModNmL modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 fpC    = case ecuStateToKind $ ecuState ecu of
%%[[94
                            EHCUKind_C -> fp
%%]]
                            _          -> mkOutputFPath opts modNm fp "c"
%%[[8
                 fpO m f= mkOutputFPath opts m f "o"
%%][99
                 fpO m f= case ehcOptPkg opts of
                            Just _ -> mkOutputFPath opts (hsnMapQualified (const base) m) (fpathSetBase base f) "o"
                                   where base = hsnShow "_" "_" m
                            _      -> mkOutputFPath opts m f "o"
%%]]
                 fpExec = maybe (mkOutputFPath opts modNm fp "") (\s -> mkOutputFPath opts modNm fp s) Cfg.mbSuffixExec
                 variant= Cfg.installVariant opts
                 (fpTarg,targOpt,linkOpts,linkLibOpt,dotOFilesOpt,genOFiles)
                        = case how of
                            FinalCompile_Exec
                              -> ( fpExec
                                 ,    ( if ehcOptOptimise opts >= OptimiseALot then ["-O2"] else if ehcOptOptimise opts >= OptimiseNormal then ["-O1"] else [] )
                                   ++ [ Cfg.gccOpts, "-o", fpathToStr fpExec ]
                                 , Cfg.ehcGccOptsStatic
                                 ,
%%[[99
                                      map (mkl2 Cfg.INST_LIB_PKG2)
                                          (if ehcOptFullProgAnalysis opts then [] else pkgKeyL)
                                   ++
%%]]
                                      map (mkl Cfg.INST_LIB)
                                          Cfg.libnamesGccPerVariant
                                   ++ map (\l -> Cfg.mkInstallFilePrefix opts Cfg.INST_LIB_SHARED variant "" ++ Cfg.mkCLibFilename "" l) (Cfg.libnamesGcc opts)
                                   ++ map ("-l" ++) Cfg.libnamesGccEhcExtraExternalLibs
                                 , if   ehcOptFullProgAnalysis opts
                                   then [ ]
                                   else [ fpathToStr $ fpO m fp | m <- othModNmL2, let (_,_,_,fp) = crBaseInfo m cr ]
                                 , []
                                 )
                              where mkl  how l = Cfg.mkCLibFilename (Cfg.mkInstallFilePrefix opts how variant l) l
%%[[99
                                    mkl2 how l = Cfg.mkCLibFilename (Cfg.mkInstallFilePrefix opts how variant (showPkgKey l) ++ "/" ++
                                                                       mkInternalPkgFileBase l (Cfg.installVariant opts)
                                                                         (ehcOptTarget opts) (ehcOptTargetFlavor opts) ++ "/")
                                                                    (showPkgKey l)
%%]]
                            FinalCompile_Module
                              -> (o, [ Cfg.gccOpts, "-c", "-o", fpathToStr o ], Cfg.ehcGccOptsStatic, [], [], [o])
                              where o = fpO modNm fp
%%[[8
                 pkgKeyL    = [] :: [String]
                 othModNmL2 = othModNmL
%%][99
                 (pkgKeyL,othModNmL2) = crPartitionIntoPkgAndOthers cr othModNmL
%%]]
         ;  when (targetIsC (ehcOptTarget opts))
                 (do { let compileC
                             = mkShellCmd
                                 (  [ Cfg.shellCmdGcc ]
                                 ++ gccDefs opts ["O"]
                                 ++ [ "-I" ++ Cfg.mkInstallFilePrefix opts Cfg.INST_INCLUDE variant "" ]
                                 ++ [ "-I" ++ Cfg.mkInstallFilePrefix opts Cfg.INST_INCLUDE_SHARED variant "" ]
%%[[(99 codegen)
                                 ++ [ "-I" ++ d | d <- gccInclDirs opts ]
%%]]
                                 ++ linkOpts
                                 ++ targOpt
                                 ++ dotOFilesOpt
                                 ++ [ fpathToStr fpC ]
                                 ++ linkLibOpt
                                 )
                     ; when (ehcOptVerbosity opts >= VerboseALot)
                            (do { cpMsg' modNm VerboseALot "GCC" Nothing fpTarg
                                ; lift $ putStrLn compileC
                                })
                     ; when (ehcOptVerbosity opts >= VerboseDebug)
                            (do { lift $ putStrLn ("pkgs : " ++ show pkgKeyL)
                                ; lift $ putStrLn ("other: " ++ show othModNmL2)
                                })
                     ; cpSeq [ cpSystem compileC
%%[[99
                             , cpUpdCU modNm (ecuStoreGenCodeFiles genOFiles)
%%]]
                             ]
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
              || modNm == hsnModIntlBase      -- 20080211, AD: builtin hack to preprocess EHC.Prelude with cpp, for now, to avoid implementation of pragmas
              )
              (do { let defs    = [ "UHC", "TARGET_" ++ (map toUpper $ show $ ehcOptTarget opts) ]
%%[[(99 codegen grin)
                                  ++ (if ehcOptFullProgAnalysis opts then ["UHC_FULL_PROGRAM_ANALYSIS"] else [])
%%]]
                        preCPP  = mkShellCmd
                                    (  [ Cfg.shellCmdCpp ]
                                    ++ [ Cfg.cppOpts ] ++ gccDefs opts ["CPP"]
                                    ++ [ "-traditional-cpp", {- "-std=gnu99", -} "-fno-show-column", "-P" ]
%%[[(99 codegen)
                                    ++ [ "-I" ++ d | d <- gccInclDirs opts ]
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
                  -- ; cpUpdCU modNm (ecuStoreSrcFilePath fpCPP)
                  ; cpUpdCU modNm (ecuStoreCppFilePath fpCPP)
                  })
       }
%%]




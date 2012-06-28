module EH101.EHC.CompilePhase.CompileC
( cpCompileWithGCC
, cpPreprocessWithCPP )
where
import Data.Char
import Data.Maybe
import EH101.EHC.Common
import EH101.EHC.CompileUnit
import EH101.EHC.CompileRun
import qualified EH101.Config as Cfg
import EH101.EHC.Environment
import EH101.Base.Target
import EH101.Base.FileSearchLocation
import EH101.Base.PackageDatabase




{-# LINE 44 "src/ehc/EHC/CompilePhase/CompileC.chs" #-}
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

{-# LINE 57 "src/ehc/EHC/CompilePhase/CompileC.chs" #-}
gccInclDirs :: EHCOpts -> [PkgModulePartition] -> [String]
gccInclDirs opts pkgKeyDirL
  =            [ mki kind dir | FileLoc kind dir <- ehcOptImportFileLocPath opts, not (null dir) ]
  ++ catMaybes [ mkp p        | p                <- map tup123to12 pkgKeyDirL                                   ]
  where mki (FileLocKind_Dir    ) d = d
        mki (FileLocKind_Pkg _ _) d = Cfg.mkPkgIncludeDir $ filePathMkPrefix d
        mki  FileLocKind_PkgDb    d = Cfg.mkPkgIncludeDir $ filePathMkPrefix d
        -- mkp (_,d) = Just (Cfg.mkPkgIncludeDir $ filePathMkPrefix d)
        mkp (k,_) = fmap (Cfg.mkPkgIncludeDir . filePathMkPrefix . filelocDir . pkginfoLoc) $ pkgDbLookup k $ ehcOptPkgDb opts

{-# LINE 69 "src/ehc/EHC/CompilePhase/CompileC.chs" #-}
cpCompileWithGCC :: FinalCompileHow -> [HsName] -> HsName -> EHCompilePhase ()
cpCompileWithGCC how othModNmL modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 fpC    = case ecuStateToKind $ ecuState ecu of
                            EHCUKind_C -> fp
                            _          -> mkOutputFPath opts modNm fp "c"
                 fpO m f = mkPerModuleOutputFPath opts False m f "o"
                 fpExec = mkPerExecOutputFPath opts modNm fp Cfg.mbSuffixExec
                 variant= Cfg.installVariant opts
                 (fpTarg,targOpt,linkOpts,linkLibOpt,dotOFilesOpt,genOFiles)
                        = case how of
                            FinalCompile_Exec
                              -> ( fpExec
                                 ,    ( if ehcOptOptimizationLevel opts >= OptimizationLevel_Much then ["-O2"] else if ehcOptOptimizationLevel opts >= OptimizationLevel_Normal then ["-O1"] else [] )
                                   ++ [ Cfg.gccOpts, "-o", fpathToStr fpExec ]
                                 , Cfg.ehcGccOptsStatic
                                 ,
                                      map (mkl2 Cfg.INST_LIB_PKG2)
                                          (if ehcOptWholeProgOptimizationScope opts then [] else map tup123to12 pkgKeyDirL)
                                   ++
                                      map (mkl Cfg.INST_LIB)
                                          Cfg.libnamesRts
                                   ++ map (\l -> Cfg.mkInstallFilePrefix opts Cfg.INST_LIB_SHARED variant "" ++ Cfg.mkCLibFilename "" l) (Cfg.libnamesGcc opts)
                                   ++ map ("-l" ++) Cfg.libnamesGccEhcExtraExternalLibs
                                 ,
                                   if   ehcOptWholeProgOptimizationScope opts
                                   then [ ]
                                   else
                                        [ fpathToStr $ fpO m fp | m <- othModNmL2, let (_,_,_,fp) = crBaseInfo m cr ]
                                 , []
                                 )
                              where -- mkl  how l = Cfg.mkCLibFilename (Cfg.mkInstallFilePrefix opts how variant l) l
                                    mkl how l = Cfg.mkInstalledRts opts Cfg.mkCLibFilename how variant l
                                    mkl2 how (l,d)
                                               = Cfg.mkCLibFilename (d ++ "/")
                                                                    (showPkgKey l)
                                               {-
                                               = Cfg.mkCLibFilename (Cfg.mkInstallFilePrefix opts how variant (showPkgKey l) ++ "/" ++
                                                                       mkInternalPkgFileBase l (Cfg.installVariant opts)
                                                                         (ehcOptTarget opts) (ehcOptTargetFlavor opts) ++ "/")
                                                                    (showPkgKey l)
                                               -}
                            FinalCompile_Module
                              -> (o, [ Cfg.gccOpts, "-c", "-o", fpathToStr o ], Cfg.ehcGccOptsStatic, [], [], [o])
                              where o = fpO modNm fp
                 (pkgKeyDirL,othModNmL2) = crPartitionIntoPkgAndOthers cr othModNmL
                 pkgKeyL = map tup123to1 pkgKeyDirL
         ;  when (targetIsC (ehcOptTarget opts))
                 (do { let compileC
                             = mkShellCmd
                                 (  [ Cfg.shellCmdGcc ]
                                 ++ gccDefs opts ["O"]
                                 ++ [ "-I" ++ Cfg.mkInstallFilePrefix opts Cfg.INST_INCLUDE variant "" ]
                                 ++ [ "-I" ++ Cfg.mkInstallFilePrefix opts Cfg.INST_INCLUDE_SHARED variant "" ]
                                 ++ [ "-I" ++ d | d <- gccInclDirs opts pkgKeyDirL ]
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
                                ; lift $ putStrLn ("pkgdirs : " ++ show pkgKeyDirL)
                                ; lift $ putStrLn ("other: " ++ show othModNmL2)
                                })
                     ; cpSeq [ cpSystem compileC
                             , cpUpdCU modNm (ecuStoreGenCodeFiles genOFiles)
                             ]
                     })
         }

{-# LINE 167 "src/ehc/EHC/CompilePhase/CompileC.chs" #-}
cpPreprocessWithCPP :: [PkgModulePartition] -> HsName -> EHCompilePhase ()
cpPreprocessWithCPP pkgKeyDirL modNm
  = do { cr <- get
       ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
              fpCPP = fpathSetSuff {- mkOutputFPath opts modNm fp -} (maybe "" (\s -> s ++ "-") (fpathMbSuff fp) ++ "cpp") fp
       ; {- when (  ehcOptCPP opts
              || modNm == hsnModIntlBase      -- 20080211, AD: builtin hack to preprocess EHC.Prelude with cpp, for now, to avoid implementation of pragmas
              ) -}
              (do { let defs    = [ "UHC", "TARGET_" ++ (map toUpper $ show $ ehcOptTarget opts) ]
                                  ++ (if targetDoesHPTAnalysis (ehcOptTarget opts) then ["UHC_FULL_PROGRAM_ANALYSIS"] else [])
                        -- (pkgKeyL,_) = crPartitionIntoPkgAndOthers cr othModNmL
                        preCPP  = mkShellCmd
                                    (  [ Cfg.shellCmdCpp ]
                                    ++ [ Cfg.cppOpts ] ++ gccDefs opts ["CPP"]
                                    ++ [ "-traditional-cpp", {- "-std=gnu99", -} "-fno-show-column", "-P" ]
                                    ++ [ "-I" ++ d | d <- gccInclDirs opts pkgKeyDirL ]
                                    ++ [ fpathToStr fp, fpathToStr fpCPP ]
                                    )
                  ; when (ehcOptVerbosity opts >= VerboseALot)
                         (do { cpMsg modNm VerboseALot "CPP"
                             ; lift $ putStrLn ("pkg db: " ++ show (ehcOptPkgDb opts))
                             ; lift $ putStrLn ("pkg srch filter: " ++ (show $ ehcOptPackageSearchFilter opts))
                             ; lift $ putStrLn ("exposed pkgs: " ++ show (pkgExposedPackages $ ehcOptPkgDb opts))
                             ; lift $ putStrLn ("pkgKeyDirL: " ++ show pkgKeyDirL)
                             ; lift $ putStrLn preCPP
                             })
                  ; when (crModCanCompile modNm cr)
                         (do { lift $ fpathEnsureExists fpCPP
                             ; cpSystem preCPP
                             ; cpRegisterFilesToRm [fpCPP]
                             })
                  -- ; cpUpdCU modNm (ecuStoreSrcFilePath fpCPP)
                  ; cpUpdCU modNm (ecuStoreCppFilePath fpCPP)
                  })
       }


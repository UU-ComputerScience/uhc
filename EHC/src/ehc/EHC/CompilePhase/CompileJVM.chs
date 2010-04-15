%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

JVM compilation

%%[(8 codegen java) module {%{EH}EHC.CompilePhase.CompileJVM}
%%]

%%[(8 codegen java) import(System.Directory)
%%]

-- general imports
%%[(8 codegen java) import({%{EH}EHC.Common})
%%]
%%[(8 codegen java) import({%{EH}EHC.CompileUnit})
%%]
%%[(8 codegen java) import({%{EH}EHC.CompileRun})
%%]

%%[(8 codegen java) import(qualified {%{EH}Config} as Cfg)
%%]
%%[(8 codegen java) import({%{EH}EHC.Environment})
%%]
%%[(8 codegen java) import({%{EH}Base.Target})
%%]

%%[(8 codegen jazy) import({%{EH}Core.ToJazy})
%%]
%%[(8 codegen java) import({%{EH}Base.Bits},{%{EH}JVMClass.ToBinary})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: Jar linking
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen && (java jazy)) export(JarWhat(..))
data JarWhat
  = Jar_Create
%%]

%%[(8 codegen && (java jazy)) export(JarMk(..))
data JarMk
  = JarMk_Pkg  String
  | JarMk_Exec HsName FPath
%%]

%%[(8 codegen && (java jazy))
type JarManifest = AssocL String String
%%]

%%[(8 codegen && (java jazy)) export(cpJar)
cpJar :: Maybe String -> Maybe FPath -> JarWhat -> FPath -> [FPath] -> EHCompilePhase ()
cpJar relToDir mbManif what archive files
  = do { cr <- get
       ; let (_,opts) = crBaseInfo' cr
             veryVerbose = ehcOptVerbosity opts >= VerboseALot
       ; cwd <- lift getCurrentDirectory
       ; (archive',files',mbInsideDir)
           <- case relToDir of
                Just d -> do { lift $ setCurrentDirectory d 
                             ; return ( mka archive, map mkd files, Just d )
                             }
                       where mkd = fpathUnPrependDir d
                             mka a = if fpathIsAbsolute a then a else fpathPrependDir cwd a
                _      -> return (archive,files,Nothing)
       ; when veryVerbose (lift $ putStrLn ("Jar: " ++ maybe "" (\d -> "in dir " ++ d ++ ": ") mbInsideDir ++ show files'))
       ; cpSeq
           (  ( case what of
                  Jar_Create | not (null files')
                    -> [jarCreateOrUpdate veryVerbose archive' files' mbManif]
                  _ -> []
              )
           ++ [lift $ setCurrentDirectory cwd]
           )
       }
  where -- split files in groups, fed to jar group by group, necessary because is somewhere a limit on nr of shell args
        -- TBD: correct Manifest (does not reflect actual content when only part of .hs are compiled)
        jarCreateOrUpdate veryVerbose archive files mbManif
          = do { archiveExists <- lift $ doesFileExist archive'
               ; cpSeq
                 $ map oneJar
                     (firstJargs archiveExists filesfirst : map restJargs filesrest)
               }
          where (manif,manifOpt)
                  = maybe ([],"") (\m -> ([fpathToStr m],"m")) mbManif
                breakInto at l
                  = case splitAt at l of
                      ([],_ ) -> []
                      (l1,[]) -> [l1]
                      (l1,l2) -> l1 : breakInto at l2
                filess@(filesfirst:filesrest)
                  = breakInto 1000 $ map fpathToStr files
                firstJargs archiveExists files
                  = [ (if archiveExists then "u" else "c")  ++ "f" ++ verbOpt ++ manifOpt, archive' ] ++ manif ++ files
                restJargs files
                  = [ "uf" ++ verbOpt, archive' ] ++ files
                verbOpt
                  = if veryVerbose then "v" else ""
                archive'
                  = fpathToStr archive
                oneJar jargs
                  = cpSystemRaw Cfg.shellCmdJar jargs
                             -- where j = mkShellCmd $ [Cfg.shellCmdJar] ++ jargs
                                   
%%]

%%[(99 codegen jazy) export(cpLinkJar)
cpLinkJar :: Maybe FPath -> [HsName] -> JarMk -> EHCompilePhase ()
cpLinkJar mbManif modNmL jarMk
  = do { cr <- get
       ; let (crsi,opts) = crBaseInfo' cr
             codeFiles = [ o | m <- modNmL, o <- ecuGenCodeFiles $ crCU m cr ]
             (libFile,mbLibDir)
                       = (libf,libd')
                       where (libf,libd) = mkInOrOutputFPathDirFor OutputFor_Pkg opts l1 l2 "jar"
                             (l1,l2,libd')
                               = case jarMk of
                                   JarMk_Pkg  p    -> (fp, fp, fmap (\d -> d {- ++ "/" ++ p -}) libd)
                                                   where fp = mkFPath $ Cfg.mkJarFilename "" p
                                   JarMk_Exec m fp -> (mkFPath m,fp,libd)
       ; cpRegisterFilesToRm codeFiles
       ; cpJar mbLibDir mbManif Jar_Create libFile codeFiles
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: JVM compilation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen && (java jazy)) export(cpCompileJazyJVM)
cpCompileJazyJVM :: FinalCompileHow -> [HsName] -> HsName -> EHCompilePhase ()
cpCompileJazyJVM how othModNmL modNm
  = do { cr <- get
       ; let  (ecu,_,opts,fp) = crBaseInfo modNm cr
              mbClsL          = ecuMbJVMClassL ecu
%%[[8
              fpCl c          = mkOutputFPath opts c (fpathSetBase (show c) fp) "class"     -- TBD: correct names
%%][20
              fpCl c          = mkOutputFPath opts c (maybe mFp (\d -> fpathPrependDir d mFp) (fpathMbDir fp)) "class"
                              where mFp = mkFPath c
%%]]
       ; when (isJust mbClsL && targetIsJVM (ehcOptTarget opts))
              (do { let (mainNm,clss1) = fromJust mbClsL
                        clss2          = concatMap jvmclass2binary clss1
                        variant        = Cfg.installVariant opts
                  ; cpMsg modNm VerboseALot "Emit Jazy"
                  ; when (ehcOptVerbosity opts >= VerboseDebug)
                         (lift $ putStrLn (show modNm ++ " JVM classes: " ++ show (map fst clss2)))
                  ; fpModClL
                      <- lift $
                         mapM (\(m,b) -> do { let fpModCl = fpCl m
                                            ; fpathEnsureExists fpModCl
                                            ; when (ehcOptVerbosity opts >= VerboseDebug)
                                                   (putStrLn (show m ++ " class file: " ++ fpathToStr fpModCl))
                                            ; writeBinaryToFile (bytesToString b) fpModCl
                                            ; return fpModCl
                                            })
                              clss2
%%[[8
                  ; return ()
%%][99
                  ; cpUpdCU modNm $ ecuStoreGenCodeFiles fpModClL
                  ; let fpManifest = mkOutputFPath opts modNm fp "manifest"     
                  ; case how of
                      FinalCompile_Exec
                        -> do { lift $ putPPFPath fpManifest (vlist $ [ k >|< ":" >#< v | (k,v) <- manifest ]) 100
                              ; cpRegisterFilesToRm [fpManifest]
                              ; cpLinkJar (Just fpManifest) (modNm : othModNmL2) (JarMk_Exec modNm fp)
                              }
                        where (pkgKeyL,othModNmL2) = crPartitionIntoPkgAndOthers cr othModNmL
                              libJarL
                                =    map mkl1 (["jazy"])
                                  ++ map mkl2 pkgKeyL
                                where mkl1 l = Cfg.mkInstallFilePrefix opts Cfg.INST_LIB      variant ""
                                               ++ "lib" ++ l ++ ".jar"
                                      mkl2 l = Cfg.mkInstallFilePrefix opts Cfg.INST_LIB_PKG2 variant (showPkgKey l)
                                               ++ "/" ++ mkInternalPkgFileBase l (Cfg.installVariant opts) (ehcOptTarget opts) (ehcOptTargetFlavor opts)
                                               ++ "/" ++ "lib" ++ (showPkgKey l) ++ ".jar"
                              manifest 
                                 = [ ( "Manifest-Version", "1.0" )
                                   , ( "Main-Class", show mainNm )
                                   , ( "Class-Path", concat $ intersperse " " libJarL )
                                   ]
                      _ -> return ()
%%]]
                  }
              )
       }                 
%%]




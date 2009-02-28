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
             libFile= mkOutputFPathFor OutputFor_Pkg opts l l (fpathSuff l)
                    where l = mkFPath $ Cfg.mkCLibFilename "" pkgNm
             linkO  = map mkShellCmd $ Cfg.mkShellCmdLibtool (fpathToStr libFile) oFiles
       ; when (ehcOptVerbosity opts >= VerboseALot)
              (do { lift $ mapM_ putStrLn linkO
                  })
       ; cpSeq [ cpSystem c | c <- linkO ]
       }
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

LLVM compilation

%%[8 module {%{EH}EHC.CompilePhase.CompileLLVM}
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: LLVM compilation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen grin) export(cpCompileWithLLVM)
cpCompileWithLLVM :: HsName -> EHCompilePhase()
cpCompileWithLLVM modNm
  = do { cr <- get
       ; let  (_,_,opts,fp) = crBaseInfo modNm cr
              fpLL          = fpathSetSuff "ll" fp
              fpExec        = maybe (fpathRemoveSuff fp) (\s -> fpathSetSuff s fp) 
                                    Cfg.mbSuffixExec
              libs          = map (\lib -> "-l " ++ lib) $
                              [ Cfg.selectFileprefixInstall opts 
                                ++ "%%@{%{VARIANT}%%}/lib/prim.o"
                              , Cfg.selectFileprefixInstall opts 
                                ++ "%%@{%{VARIANT}%%}/lib/llvm-gc.o"
                              , Cfg.selectFileprefixInstall opts 
                                ++ "%%@{%{VARIANT}%%}/lib/timing.o"
                              , Cfg.selectFileprefixInstall opts
                                ++ "lib/libgc.a"
                              ]
              inputOpts     = [ fpathToStr fpLL ]
              outputOpts    = ["-o " ++ fpathToStr fpExec]
       ; when ( ehcOptEmitExecLLVM opts )
         (  do { let compileLL 
                       = concat $ intersperse " "
                         $  [ Cfg.shellCmdLLVM ]
                         ++ libs
                         ++ outputOpts
                         ++ inputOpts
               ; when (ehcOptVerbosity opts >= VerboseALot)
                 (  do { cpMsg' modNm VerboseALot "LLVM" Nothing fpExec
                       ; lift $ putStrLn compileLL
                       }
                 )
               ; cpSystem compileLL
               }
         ) 
       }                 
%%]


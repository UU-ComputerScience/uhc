%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module Main
%%]

%%[1 import(System.Console.GetOpt)
%%]
%%[1.fastseq import(qualified EH.Util.FastSeq as Seq)
%%]
%%[1 import(qualified {%{EH}Config} as Cfg)
%%]
%%[1 import({%{EH}EHC.Common})
%%]

%%[8 -1.fastseq
%%]

-- HS semantics
%%[1.HSSem import(qualified {%{EH}HS.MainAG} as HSSem)
%%]
-- EH semantics
%%[1.EHSem import(qualified {%{EH}EH.MainAG} as EHSem)
%%]

-- parsing later put in {%{EH}EHC.CompilePhase.Parsers}
%%[1.scannercommon import({%{EH}Scanner.Common})
%%]
%%[1.parsinglib import(UU.Parsing, UU.Parsing.Offside)
%%]
%%[1.parse.EHPrs.HSPrs import(qualified {%{EH}EH.Parser} as EHPrs, qualified {%{EH}HS.Parser} as HSPrs)
%%]

%%[8 -(1.scannercommon 1.parsinglib 1.parse.EHPrs.HSPrs 1.EHSem 1.HSSem)
%%]

-- compiler driver modules
%%[8 import({%{EH}EHC.CompileUnit},{%{EH}EHC.CompileRun})
%%]
%%[8 import({%{EH}EHC.InitialSetup})
%%]
%%[8 import({%{EH}EHC.CompilePhase.TopLevelPhases})
%%]
%%[20 import({%{EH}EHC.CompilePhase.Module})
%%]

-- general imports
%%[8 import(qualified Debug.Trace)
%%]
%%[8 import(qualified Data.Map as Map)
%%]

-- module
%%[20 import({%{EH}Module}(modBuiltin))
%%]

-- Misc
%%[(102 codegen) import({%{EH}Core.Trf.Strip})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main, compiling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.main
main :: IO ()
main
  =  do  {  args      <- getArgs
         ;  progName  <- getProgName
         ;  let  ehcOpts        = defaultEHCOpts
%%[[99
                                    { ehcProgName      = p
                                    , ehcOptUseInplace = fpathBase p == Cfg.verProg Cfg.version
                                    }
                                where p = mkFPath progName
%%]]
                 oo@(o,n,errs)  = getOpt Permute ehcCmdLineOpts args
                 opts           = foldl (flip ($)) ehcOpts o
         ;  if ehcOptHelp opts
%%[[1
            then  putStrLn (usageInfo ("version: " ++ Cfg.verInfo Cfg.version ++ ", aspects: " ++ ehcOptAspects opts
                                       ++ "\n\nUsage: " ++ progName ++ " [options] [file[.eh|.hs]]\n\noptions:"
                                      ) ehcCmdLineOpts)
%%][8
            then  do  {  putStrLn (usageInfo (  "version: " ++ Cfg.verInfo Cfg.version ++ ", aspects: " ++ ehcOptAspects opts
                                             ++ "\n\nUsage: " ++ progName
                                             ++ " [options] [file[.eh|.hs]]\n\noptions:"
                                             )
                                             ehcCmdLineOpts)
%%[[(8 codegen)
                      ;  putStrLn ("Transformations:\n" ++ (unlines . map (\(n,t) -> "  " ++ n ++ ": " ++ t) $ cmdLineTrfs))
%%]]
                      }
%%]]
            else  if ehcOptVersion opts
            then  putStrLn (Cfg.verInfo Cfg.version)
%%[[99
            else  if ehcOptShowNumVersion opts
            then  putStrLn (Cfg.verNumeric Cfg.version)
%%]]
            else  if null errs
                  then  doCompileRun (if null n then "" else head n) opts
                  else  putStr (head errs)
         }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Search path
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
type FileSuffMp = [(String,EHCompileUnitState)]

fileSuffMpHs :: FileSuffMp
fileSuffMpHs
  = [ ( "hs"  , ECUSHaskell HSStart )
%%[[99
    , ( "lhs" , ECUSHaskell LHSStart )
%%]]
    , ( "eh"  , ECUSEh EHStart )
%%[[(8 grin)
    , ( "grin", ECUSGrin )
%%]]
    ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Show sizes, mem usage
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(102 codegen)
showSizeCore :: Core.CModule -> String
showSizeCore x = fevShow "Core" x

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% XXX periments
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
infixr 2 >$>

(>$>) :: CompileRunError e p => CompilePhase n u i e () -> CompilePhase n u i e () -> CompilePhase n u i e ()
this >$> next = this >-> next

cpLift :: CompilePhase n u i e () -> CompilePhase n u i e ()
cpLift = id
%%]
infixr 2 >$>

(>$>) :: CompileRunError e p => (CompilePhase n u i e () -> CompilePhase n u i e ()) -> CompilePhase n u i e () -> CompilePhase n u i e ()
this >$> next = this next

cpLift :: CompilePhase n u i e () -> CompilePhase n u i e () -> CompilePhase n u i e ()
cpLift this next
  = do { _ <- this
       ; next
       }

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: compilation of module(s)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compiler driver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.doCompile
doCompileRun :: String -> EHCOpts -> IO ()
doCompileRun filename opts
  =  do  {  (fn,fh) <-  if null filename
                        then  return ("<stdin>",stdin)
                        else  do  {  h <- openFile filename ReadMode
                                  ;  return (filename,h)
                                  }
         ;  let isHS = isSuffixOf ".hs" fn
         ;  when
              (ehcStopAtPoint opts >= CompilePoint_Parse)
              (do { tokens <- offsideScanHandle (if isHS then hsScanOpts else ehScanOpts) fn fh
                  ; resd <-
                      if isHS
                      then do { let steps = parseOffside (HSPrs.pAGItf) tokens
                              ; (resd,_) <- evalStepsIO show steps
                              ; if ehcStopAtPoint opts >= CompilePoint_AnalHS
                                then do { let res   = HSSem.sem_AGItf resd
                                              wrRes = HSSem.wrap_AGItf res
                                                        (HSSem.Inh_AGItf
                                                          { HSSem.opts_Inh_AGItf = opts
                                                          })
                                        ; putStrLn (disp (ppErrL $ Seq.toList $ HSSem.errSq_Syn_AGItf $ wrRes) 1000 "")
                                        ; when (ehcOptShowHS opts)
                                               (putStrLn (disp (HSSem.pp_Syn_AGItf wrRes) 1000 ""))
                                        ; return (HSSem.eh_Syn_AGItf wrRes)
                                        }
                                else return undefined
                              }
                      else do { let steps = parseOffside (EHPrs.pAGItf) tokens
                              ; (resd,_) <- evalStepsIO show steps
                              ; return resd
                              }
                  ; when
                      (ehcStopAtPoint opts >= CompilePoint_AnalEH)
                      (do { let res   = EHSem.sem_AGItf resd
                                wrRes = EHSem.wrap_AGItf res (EHSem.Inh_AGItf {EHSem.opts_Inh_AGItf = opts})
                          ; when (ehcOptShowEH opts)
                                 (putStrLn (disp (EHSem.pp_Syn_AGItf wrRes) 70 ""))
                          ; when (ehcOptShowAst opts)
                                 (putStrLn (disp (EHSem.ppAST_Syn_AGItf wrRes) 1000 ""))
%%[[(1 hmtyinfer)
                          ; when (ehcOptShowTopTyPP opts)
                                 (putStr (disp (EHSem.topTyPP_Syn_AGItf wrRes) 1000 ""))
%%]]
%%[[7_2
                          ; when (not (null filename) && ehcOptUniqueness opts)
                                 (writeFile (filename ++ ".html") (EHSem.ppHTML_Syn_AGItf wrRes))
%%]]
                          })
                  })
         }
%%]

%%[8.doCompile -1.doCompile
doCompileRun :: String -> EHCOpts -> IO ()
doCompileRun fn opts
  = do { let fp             = mkTopLevelFPath "hs" fn
             topModNm       = mkHNm (fpathBase fp)
             searchPath     = mkInitSearchPath fp
                              ++ ehcOptSearchPath opts
%%[[101
                              ++ (if ehcOptUseInplace opts then [] else [Cfg.fileprefixInstall ++ "ehclib/ehcbase"])
%%]]
             opts2          = opts { ehcOptSearchPath = searchPath }
             initialState   = mkEmptyCompileRun
                                topModNm
                                (EHCompileRunStateInfo opts2 (initialHSSem opts2) (initialEHSem opts2 fp)
%%[[(8 codegen)
                                                       (initialCore2GrSem opts2)
%%]]
                                                       uidStart uidStart
%%[[20
                                                       (initialHISem opts2) (initialHSSemMod opts2) Map.empty Map.empty defaultOptim
%%]]
%%[[(20 codegen)
                                                       Map.empty
%%]]
                                )
%%[[8
             comp mbFp nm
               = do { mbFoundFp <- cpFindFileForFPath fileSuffMpHs searchPath (Just nm) mbFp
                    ; when (isJust mbFoundFp)
                           (cpEhcModuleCompile1 nm)
                    }
%%][20
             imp mbFp nm
               = do { mbFoundFp <- cpFindFileForFPath fileSuffMpHs searchPath (Just nm) mbFp
                    -- ; lift $ putStrLn $ show nm ++ ": " ++ show mbFp ++ ": " ++ show mbFoundFp
                    ; when (isJust mbFp)
                           (cpUpdCU nm (ecuSetIsTopMod True))
                    ; when (isJust mbFoundFp)
                           (cpEhcModuleCompile1 (Just HSOnlyImports) nm)
                    }
%%]]
       -- ; putStrLn $ show searchPath
%%[[8
       ; _ <- runStateT (cpSeq [ comp (Just fp) topModNm
                               ]) initialState
%%][20
       ; _ <- runStateT (cpSeq [ imp (Just fp) topModNm
                               , cpImportGather (imp Nothing) topModNm
                               , cpCheckMods' [modBuiltin]
                               , cpEhcFullProgCompileAllModules
                               ]) initialState
%%]]
       ; return ()
       }

%%]


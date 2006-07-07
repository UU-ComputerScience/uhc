%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module Main import(System, Data.List, Control.Monad, System.Console.GetOpt, IO, UU.Pretty, UU.Parsing, UU.Parsing.Offside, {%{EH}Base.Common}, {%{EH}Scanner.Common}, {%{EH}Base.Opts})
%%]

%%[1 import(qualified {%{EH}EH.Parser} as EHPrs, qualified {%{EH}EH.MainAG} as EHSem, qualified {%{EH}HS.Parser} as HSPrs, qualified {%{EH}HS.MainAG} as HSSem)
%%]

%%[8 import (EH.Util.CompileRun,{%{EH}Error},{%{EH}Error.Pretty},EH.Util.FPath,qualified Data.Map as Map,Data.Maybe,Data.List)
%%]

%%[8 import ({%{EH}Core.Java},{%{EH}Core.Grin},{%{EH}Core.Pretty})
%%]

%%[8 import ({%{EH}Core.Trf.RenUniq},{%{EH}Core.Trf.FullLazy},{%{EH}Core.Trf.InlineLetAlias},{%{EH}Core.Trf.LetUnrec},{%{EH}Core.Trf.LamLift},{%{EH}Core.Trf.ConstProp},{%{EH}Core.Trf.EtaRed})
%%]

%%[8 import ({%{EH}GrinCode.Pretty})
%%]

%%[8 import (qualified {%{GRIN}CompilerDriver} as GRINC, qualified {%{GRIN}GRINCCommon} as GRINCCommon)
%%]

%%[8 import (qualified {%{EH}HS} as HS)
%%]
%%[8 import (qualified {%{EH}EH} as EH)
%%]
%%[8 import (qualified {%{EH}Core} as Core)
%%]
%%[8 import (qualified {%{EH}GrinCode} as Grin)
%%]
%%[8 import (qualified {%{GRIN}CmmCode} as Cmm)
%%]
%%[8 import (Control.Monad.State)
%%]
%%[8 import (qualified EH.Util.ScanUtils as ScanUtils)
%%]
%%[8 import(qualified {%{EH}GrinCode.Parser} as GrinParser)
%%]



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Version of program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
versionSvn      = "$Id$"
versionMajor    = "0"
versionMinor    = "1"
versionQuality  = "alpha"
versionDist     = versionMajor ++ "." ++ versionMinor ++ versionQuality
versionProg     = "ehc"
versionInfo     = versionProg ++ versionDist ++ ", " ++ versionSvn
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main, compiling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.main
main :: IO ()
main
  =  do  {  args <- getArgs
         ;  let  oo@(o,n,errs)  = getOpt Permute ehcCmdLineOpts args
                 opts           = foldl (flip ($)) defaultEHCOpts o
         ;  if ehcOptHelp opts
%%]
%%[1.main.ehcOptHelp
            then  putStrLn (usageInfo ("version: " ++ versionInfo ++ "\n\nUsage: ehc [options] [file[.eh|.hs]]\n\noptions:") ehcCmdLineOpts)
%%]
%%[8.main.ehcOptHelp -1.main.ehcOptHelp
            then  do  {  putStrLn (usageInfo ("version: " ++ versionInfo ++ "\n\nUsage: ehc [options] [file[.eh|.hs]]\n\noptions:") ehcCmdLineOpts)
                      ;  putStrLn ("Transformations:\n" ++ (unlines . map (\(n,t) -> "  " ++ n ++ ": " ++ t) $ cmdLineTrfs))
                      }
%%]
%%[1.main.tl
            else  if ehcOptVersion opts
            then  putStrLn versionDist
            else  if null errs
                  then  doCompileRun (if null n then "" else head n) opts
                  else  putStr (head errs)
         }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Messages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]
mkParseErrInfoL :: (Eq s, Show s) => [Message s (Maybe Token)] -> ErrL
mkParseErrInfoL = map (\(Msg exp pos act) -> Err_Parse (show exp) (show act))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compilation unit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data EHCompileUnitState
  = ECUSUnknown | ECUSHaskell | ECUSEh | ECUSGrin | ECUSFail
  deriving (Show,Eq)

data EHCompileUnit
  = EHCompileUnit
      { ecuFilePath          :: FPath
      , ecuModNm             :: HsName
      , ecuMbSemHS           :: Maybe HSSem.Syn_AGItf
      , ecuMbSemEH           :: Maybe EHSem.Syn_AGItf
      , ecuMbHS              :: Maybe HS.AGItf
      , ecuMbEH              :: Maybe EH.AGItf
      , ecuMbCore            :: Maybe Core.CModule
      , ecuMbGrin            :: Maybe Grin.GrModule
      , ecuState             :: EHCompileUnitState
      }

type EcuUpdater a = a -> EHCompileUnit -> EHCompileUnit

ecuStoreHS :: EcuUpdater HS.AGItf
ecuStoreHS x ecu = ecu { ecuMbHS = Just x }

ecuStoreEH :: EcuUpdater EH.AGItf
ecuStoreEH x ecu = ecu { ecuMbEH = Just x }

ecuStoreCore :: EcuUpdater Core.CModule
ecuStoreCore x ecu = ecu { ecuMbCore = Just x }

ecuStoreGrin :: EcuUpdater Grin.GrModule
ecuStoreGrin x ecu = ecu { ecuMbGrin = Just x }



emptyECU :: EHCompileUnit
emptyECU
  = EHCompileUnit
      { ecuFilePath          = emptyFPath
      , ecuModNm             = hsnUnknown
      , ecuMbSemHS           = Nothing
      , ecuMbSemEH           = Nothing
      , ecuMbHS              = Nothing
      , ecuMbEH              = Nothing
      , ecuMbCore            = Nothing
      , ecuMbGrin            = Nothing
      , ecuState             = ECUSUnknown
      }

instance CompileUnitState EHCompileUnitState where
  cusDefault    = ECUSEh
  cusUnk        = ECUSUnknown
  cusIsUnk      = (==ECUSUnknown)
  cusIsImpKnown = const True

instance CompileUnit EHCompileUnit HsName EHCompileUnitState where
  cuDefault         = emptyECU
  cuFPath           = ecuFilePath
  cuKey             = ecuModNm
  cuState           = ecuState
  cuUpdFPath fp u   = u {ecuFilePath = fp}
  cuUpdState st u   = u {ecuState = st}
  cuUpdKey   nm u   = u {ecuModNm = nm}
  cuImports _       = []

instance CompileRunError Err () where
  crePPErrL                 = ppErrL
  creMkNotFoundErrL _ fp sp = [Err_FileNotFound fp sp]
  creAreFatal               = const True

instance CompileModName HsName where
  mkCMNm = HNm
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile run combinators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data EHCompileRunStateInfo
  = EHCompileRunStateInfo
      { crsiOpts        :: EHCOpts
      , crsiNextUID     :: UID
      , crsiHereUID     :: UID
      }

instance CompileRunStateInfo EHCompileRunStateInfo HsName () where
  crsiImportPosOfCUKey n i = ()

type EHCompileRun     = CompileRun   HsName EHCompileUnit EHCompileRunStateInfo Err
type EHCompilePhase a = CompilePhase HsName EHCompileUnit EHCompileRunStateInfo Err a

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Search path
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
type FileSuffMp = Map.Map String EHCompileUnitState

fileSuffMpHs :: FileSuffMp
fileSuffMpHs = Map.fromList [ ( "hs", ECUSHaskell ), ( "eh", ECUSEh ), ( "grin", ECUSGrin ) ]

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%[8

cpParseOffside :: HSPrs.HSParser a -> ScanUtils.ScanOpts -> EcuUpdater a -> String -> HsName -> EHCompilePhase ()
cpParseOffside parse scanOpts store description modNm
 = do { cr <- get
      ; (fn,fh) <- lift $ openFPath (ecuFilePath (crCU modNm cr)) ReadMode
      ; tokens  <- lift $ offsideScanHandle scanOpts fn fh
      ; let steps = parseOffside parse tokens
      ; cpUpdCU modNm (store (fst (evalSteps steps)))
      ; cpSetLimitErrsWhen 5 description (map mkPPErr (getMsgs steps))
      }

cpParseHs :: HsName -> EHCompilePhase ()
cpParseHs = cpParseOffside HSPrs.pAGItf hsScanOpts ecuStoreHS "Parse (Haskell syntax) of module"

cpParseEH :: HsName -> EHCompilePhase ()
cpParseEH = cpParseOffside EHPrs.pAGItf ehScanOpts ecuStoreEH "Parse (EH syntax) of module"

cpParseGrin :: HsName -> EHCompilePhase ()
cpParseGrin modNm
 = do { cr <- get
      ; (fn,fh) <- lift $ openFPath (ecuFilePath (crCU modNm cr)) ReadMode
      ; tokens  <- lift $ scanHandle GrinParser.scanOpts fn fh
      ; gr      <- lift $ parseIO GrinParser.pModule tokens
      ; cpUpdCU modNm (ecuStoreGrin gr)
      }




foldEH :: FPath -> UID -> EHCOpts -> EH.AGItf -> EHSem.Syn_AGItf
foldEH fp uid opts eh
 = EHSem.wrap_AGItf (EHSem.sem_AGItf eh)
                    (EHSem.Inh_AGItf { EHSem.baseName_Inh_AGItf = fpathBase fp
                                     , EHSem.gUniq_Inh_AGItf    = uid
                                     , EHSem.opts_Inh_AGItf     = opts
                                     })

foldHs :: EHCOpts -> HS.AGItf -> HSSem.Syn_AGItf
foldHs opts hs
 = HSSem.wrap_AGItf (HSSem.sem_AGItf hs)
                    (HSSem.Inh_AGItf { HSSem.opts_Inh_AGItf     = opts
                                     })


cpTranslateHs2EH :: HsName -> EHCompilePhase ()
cpTranslateHs2EH modNm
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
                 opts   = crsiOpts crsi
                 mbHs   = ecuMbHS ecu
                 hsSem  = foldHs opts (fromJust mbHs)
                 eh     = HSSem.eh_Syn_AGItf hsSem
         ;  when (isJust mbHs)
                 (cpUpdCU modNm (ecuStoreEH eh))
         }

cpTranslateEH2Core :: HsName -> EHCompilePhase ()
cpTranslateEH2Core modNm
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
                 opts   = crsiOpts crsi
                 mbEH   = ecuMbEH ecu
                 fp     = ecuFilePath ecu
                 ehSem  = foldEH fp (crsiHereUID crsi) opts (fromJust mbEH)
                 core   = EHSem.cmodule_Syn_AGItf ehSem
                 errs   = EHSem.allErrL_Syn_AGItf ehSem
         ;  when (isJust mbEH)
                 (do { cpUpdCU modNm (ecuStoreCore core)
                     ; cpSetLimitErrsWhen 5 "Type checking" errs
                     ; when (ehcOptEmitEH opts)
                            (lift $ putPPFile (fpathToStr (fpathSetSuff "eh2" fp)) (EHSem.pp_Syn_AGItf ehSem) 1000)
                     ; when (ehcOptShowEH opts)
                            (lift $ putWidthPPLn 120 (EHSem.pp_Syn_AGItf ehSem))
                     ; when (ehcOptShowAst opts)
                            (lift $ putPPLn (EHSem.ppAST_Syn_AGItf ehSem))
                     }
                 )
         }

cpTranslateCore2Grin :: HsName -> EHCompilePhase ()
cpTranslateCore2Grin modNm
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
                 opts   = crsiOpts crsi
                 fp     = ecuFilePath ecu
                 mbCore = ecuMbCore ecu
                 cMod   = fromJust mbCore
                 [u1]   = mkNewLevUIDL 1 . snd . mkNewLevUID . crsiHereUID $ crsi
                 grin   = cmodGrin u1 cMod
         ;  when (isJust mbCore && (ehcOptEmitGrin opts || ehcOptEmitCmm opts || ehcOptEmitLlc opts))
                 (cpUpdCU modNm (ecuStoreGrin grin))
         }

cpTranslateGrin :: HsName -> EHCompilePhase ()
cpTranslateGrin modNm
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
                 opts   = crsiOpts crsi
                 fp     = ecuFilePath ecu
                 mbGrin = ecuMbGrin ecu
                 grin   = fromJust mbGrin
         ;  when (isJust mbGrin && (ehcOptEmitCmm opts || ehcOptEmitLlc opts))
                 (lift $ GRINC.doCompileGrin (Right (fp,grin)) opts)
         }


cpCore1Trf :: HsName -> String -> EHCompilePhase ()
cpCore1Trf modNm trfNm
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
                 mbCore = ecuMbCore ecu
                 core   = fromJust mbCore
                 [u1]   = mkNewLevUIDL 1 . snd . mkNewLevUID . crsiHereUID $ crsi
                 core2  = ( case trfNm of
                              "CER"     -> cmodTrfEtaRed
                              "CCP"     -> cmodTrfConstProp
                              "CRU"     -> cmodTrfRenUniq
                              "CLU"     -> cmodTrfLetUnrec
                              "CILA"    -> cmodTrfInlineLetAlias
                              "CFL"     -> cmodTrfFullLazy u1
                              "CLL"     -> cmodTrfLamLift
                              _         -> id
                          ) core
         ;  lift (putCompileMsg VerboseALot (ehcOptVerbosity . crsiOpts $ crsi) "Transforming" (lookup trfNm cmdLineTrfs) modNm (ecuFilePath ecu))
         ;  cpUpdCU modNm (ecuStoreCore core2)
         }

cpTransformCore :: HsName -> [String] -> EHCompilePhase ()
cpTransformCore modNm trfNmL
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
                 opts   = crsiOpts crsi
                 tr     = ehcOptTrf opts
                 ps     = ( intersperse cpStepUID 
                          . map (cpCore1Trf modNm)
                          . filter (maybe True id . trfOptOverrides tr)
                          ) trfNmL
         ;  cpSeq ps
         }


cpProcessHs :: HsName -> EHCompilePhase ()
cpProcessHs modNm 
  = cpSeq [ cpTranslateHs2EH modNm
          , cpProcessEH modNm
          ]

cpProcessEH :: HsName -> EHCompilePhase ()
cpProcessEH modNm 
  = cpSeq [ cpTranslateEH2Core modNm
          , cpProcessCore modNm
          ]

cpProcessCore :: HsName -> EHCompilePhase ()
cpProcessCore modNm 
  = cpSeq [ cpStepUID
          , cpTransformCore modNm ["CER", "CCP", "CRU", "CLU", "CILA", "CFL", "CLL", "CFL", "CLU"]
          , cpOutputCore modNm
          , cpTranslateCore2Grin modNm
          , cpProcessGrin modNm
          ]
          
cpProcessGrin :: HsName -> EHCompilePhase ()
cpProcessGrin modNm 
  = cpSeq [ cpOutputGrin modNm
          , cpTranslateGrin modNm
          ]





cpOutputCore :: HsName -> EHCompilePhase ()
cpOutputCore modNm
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
                 fp     = ecuFilePath ecu
                 mbCore = ecuMbCore ecu
                 opts   = crsiOpts crsi
                 cMod   = fromJust mbCore
                 (jBase,jPP) = cmodJavaSrc cMod
                 jFP = fpathSetBase jBase fp                 
         ;  when (ehcOptEmitCore opts) 
                 (lift (putPPFile (fpathToStr (fpathSetSuff "core" fp)) (ppCModule cMod) 120))
         ;  when (ehcOptEmitJava opts)
                 (lift (putPPFile (fpathToStr (fpathSetSuff "java" jFP)) jPP 120))
         }

cpOutputGrin :: HsName -> EHCompilePhase ()
cpOutputGrin modNm
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
                 fp     = ecuFilePath ecu
                 mbGrin = ecuMbGrin ecu
                 opts   = crsiOpts crsi
                 grin   = fromJust mbGrin
                 grinPP = ppGrModule grin
         ;  when (ehcOptEmitGrin opts)
                 (lift $ putPPFile (fpathToStr (fpathSetSuff "grin2" fp)) grinPP 1000)
         ;  when (ehcOptShowGrin opts)
                 (lift $ putPPLn grinPP)
         }



%%]




%%[8
cpStepUID :: EHCompilePhase ()
cpStepUID
  = do{ cr <- get
      ; let (n,h) = mkNewLevUID (crsiNextUID crsi)
            crsi = crStateInfo cr
      ; put (cr {crStateInfo = crsi {crsiNextUID = n, crsiHereUID = h}})
      }
%%]

%%[8
cpCompileCU :: HsName -> EHCompilePhase ()
cpCompileCU modNm
  = do { cr <- get
       ; let ecu   = crCU modNm cr
             opts  = crsiOpts (crStateInfo cr)
             -- msg m = lift (putCompileMsg VerboseNormal (ehcOptVerbosity opts) m Nothing modNm (ecuFilePath ecu))
             msg m = lift (putCompileMsg VerboseNormal (ehcOptVerbosity opts) (m ++ " (" ++ show (ecuState ecu) ++ "/" ++ fpathSuff (ecuFilePath ecu) ++ ")") Nothing modNm (ecuFilePath ecu))
       ; case ecuState ecu of
           ECUSHaskell
             -> do { msg "Compiling Haskell"
                   ; cpSeq [cpParseHs modNm, cpProcessHs modNm]
                   }
           ECUSEh
             -> do { msg "Compiling EH"
                   ; cpSeq [cpParseEH modNm, cpProcessEH modNm]
                   }
           ECUSGrin
             -> do { msg "Compiling Grin"
                   ; cpSeq [cpParseGrin modNm, cpProcessGrin modNm]
                   }
           _ -> do { msg "Skipping"
                   }
       }

cpCompileOrderedCUs :: EHCompilePhase ()
cpCompileOrderedCUs
 = do { modNmLL <- gets crCompileOrder
      ; cpSeq (map (cpCompileCU . head) modNmLL)
      }

%%]

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
         ;  tokens <- offsideScanHandle (if isHS then hsScanOpts else ehScanOpts) fn fh
         ;  resd <-
              if isHS
              then do { let steps = parseOffside (HSPrs.pAGItf) tokens
                      ; (resd,_) <- evalStepsIO show steps
                      ; let res   = HSSem.sem_AGItf resd
                            wrRes = HSSem.wrap_AGItf res (HSSem.Inh_AGItf {HSSem.opts_Inh_AGItf = opts})
                      ; return (HSSem.eh_Syn_AGItf wrRes)
                      }
              else do { let steps = parseOffside (EHPrs.pAGItf) tokens
                      ; (resd,_) <- evalStepsIO show steps
                      ; return resd
                      }
         ;  let res   = EHSem.sem_AGItf resd
                wrRes = EHSem.wrap_AGItf res (EHSem.Inh_AGItf {EHSem.opts_Inh_AGItf = opts})
         ;  when (ehcOptShowEH opts)
                 (putStrLn (disp (EHSem.pp_Syn_AGItf wrRes) 70 ""))
         ;  when (ehcOptShowAst opts)
                 (putStrLn (disp (EHSem.ppAST_Syn_AGItf wrRes) 1000 ""))
         ;  when (ehcOptShowTopTyPP opts)
                 (putStr (disp (EHSem.topTyPP_Syn_AGItf wrRes) 1000 ""))
         }
%%]

%%[8.doCompile -1.doCompile

doCompileRun :: String -> EHCOpts -> IO ()
doCompileRun fn opts
  = do { let fp             = mkTopLevelFPath "hs" fn
             topModNm       = HNm (fpathBase fp)
             searchPath     = ehcOptSearchPath opts ++ mkInitSearchPath fp
             opts2          = opts { ehcOptSearchPath = searchPath }
             initialState   = mkEmptyCompileRun topModNm (EHCompileRunStateInfo opts2 uidStart uidStart)
             aSetup         = do { mbFp <- cpFindFileForFPath fileSuffMpHs searchPath (Just topModNm) (Just fp)
                                 ; when (isJust mbFp)
                                        (cpSetCompileOrder [[topModNm]])
                                 }
       ; _ <- runStateT (cpSeq [ aSetup, cpCompileOrderedCUs ]) initialState
       ; return ()
       }

%%]

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module Main import(System, Data.List, Control.Monad, System.Console.GetOpt, IO, UU.Pretty,EH.Util.PPUtils,{%{EH}Error.Pretty}, UU.Parsing, UU.Parsing.Offside, {%{EH}Base.Common}, {%{EH}Scanner.Common}, {%{EH}Base.Opts})
%%]

%%[1 import(qualified {%{EH}EH.Parser} as EHPrs, qualified {%{EH}EH.MainAG} as EHSem, qualified {%{EH}HS.Parser} as HSPrs, qualified {%{EH}HS.MainAG} as HSSem)
%%]

%%[8 import (EH.Util.CompileRun,{%{EH}Error},{%{EH}Gam},{%{EH}Module},EH.Util.FPath,qualified Data.Map as Map,Data.Maybe,Data.List)
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
%%% Compilation group
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data EHCompileGroup
  = EHCompileGroup
      { ecgNm                :: HsName
      , ecgModL              :: [HsName]
      }

emptyECG :: EHCompileGroup
emptyECG
  = EHCompileGroup
      { ecgNm                = hsnUnknown
      , ecgModL              = []
      }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compilation unit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data HaskellState
  = HSStart | HSOnlyImports | HSAllSem
  deriving (Show,Eq)

data EHCompileUnitState
  = ECUSUnknown | ECUSHaskell HaskellState | ECUSEh | ECUSGrin | ECUSFail
  deriving (Show,Eq)

data EHCompileUnit
  = EHCompileUnit
      { ecuFilePath          :: FPath
      , ecuGrpNm             :: HsName
      , ecuModNm             :: HsName
      , ecuImpNmL            :: [HsName]
      , ecuMbHSSem           :: Maybe HSSem.Syn_AGItf
      , ecuMbEHSem           :: Maybe EHSem.Syn_AGItf
      , ecuMbHS              :: Maybe HS.AGItf
      , ecuMbEH              :: Maybe EH.AGItf
      , ecuMbCore            :: Maybe Core.CModule
      , ecuMbGrin            :: Maybe Grin.GrModule
      , ecuState             :: EHCompileUnitState
      , ecuMod               :: Mod
      }

type EcuUpdater a = a -> EHCompileUnit -> EHCompileUnit

ecuStoreHS :: EcuUpdater HS.AGItf
ecuStoreHS x ecu = ecu { ecuMbHS = Just x }

ecuStoreEH :: EcuUpdater EH.AGItf
ecuStoreEH x ecu = ecu { ecuMbEH = Just x }

ecuStoreHSSem :: EcuUpdater HSSem.Syn_AGItf
ecuStoreHSSem x ecu = ecu { ecuMbHSSem = Just x }

ecuStoreEHSem :: EcuUpdater EHSem.Syn_AGItf
ecuStoreEHSem x ecu = ecu { ecuMbEHSem = Just x }

ecuStoreCore :: EcuUpdater Core.CModule
ecuStoreCore x ecu = ecu { ecuMbCore = Just x }

ecuStoreGrin :: EcuUpdater Grin.GrModule
ecuStoreGrin x ecu = ecu { ecuMbGrin = Just x }

ecuStoreMod :: EcuUpdater Mod
ecuStoreMod x ecu = ecu { ecuMod = x, ecuImpNmL = map mimpSource $ modImpL $ x }

ecuStoreState :: EcuUpdater EHCompileUnitState
ecuStoreState x ecu = ecu { ecuState = x }

emptyECU :: EHCompileUnit
emptyECU
  = EHCompileUnit
      { ecuFilePath          = emptyFPath
      , ecuGrpNm			 = hsnUnknown
      , ecuModNm             = hsnUnknown
      , ecuImpNmL            = []
      , ecuMbHSSem           = Nothing
      , ecuMbEHSem           = Nothing
      , ecuMbHS              = Nothing
      , ecuMbEH              = Nothing
      , ecuMbCore            = Nothing
      , ecuMbGrin            = Nothing
      , ecuState             = ECUSUnknown
      , ecuMod               = emptyMod
      }

instance CompileUnitState EHCompileUnitState where
  cusDefault      = ECUSEh
  cusUnk          = ECUSUnknown
  cusIsUnk        = (==ECUSUnknown)
  cusIsImpKnown s = case s of
                      ECUSHaskell HSOnlyImports -> True
                      ECUSHaskell HSAllSem      -> True
                      _                         -> False

instance CompileUnit EHCompileUnit HsName EHCompileUnitState where
  cuDefault         = emptyECU
  cuFPath           = ecuFilePath
  cuKey             = ecuModNm
  cuState           = ecuState
  cuUpdFPath fp u   = u {ecuFilePath = fp}
  cuUpdState st u   = u {ecuState = st}
  cuUpdKey   nm u   = u {ecuModNm = nm}
  cuImports         = ecuImpNmL

instance CompileRunError Err () where
  crePPErrL                 = ppErrL
  creMkNotFoundErrL _ fp sp = [Err_FileNotFound fp sp]
  creAreFatal               = const True

instance CompileModName HsName where
  mkCMNm = HNm

instance Show EHCompileUnit where
  show _ = "EHCompileUnit"

instance PP EHCompileUnit where
  pp ecu = ecuModNm ecu >|< ":" >#< ppCommas (ecuImpNmL ecu) >|< "," >#< show (ecuState ecu)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile run combinators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data EHCompileRunStateInfo
  = EHCompileRunStateInfo
      { crsiOpts        :: EHCOpts
      , crsiHSInh       :: HSSem.Inh_AGItf
      , crsiEHInh       :: EHSem.Inh_AGItf
      , crsiNextUID     :: UID
      , crsiHereUID     :: UID
      , crsiGrpMp       :: Map.Map HsName EHCompileGroup
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
fileSuffMpHs = Map.fromList [ ( "hs", ECUSHaskell HSStart ), ( "eh", ECUSEh ), ( "grin", ECUSGrin ) ]

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

foldEH :: EHSem.Inh_AGItf -> FPath -> UID -> EHCOpts -> EH.AGItf -> EHSem.Syn_AGItf
foldEH inh fp uid opts eh
 = EHSem.wrap_AGItf (EHSem.sem_AGItf eh)
                    (inh { EHSem.baseName_Inh_AGItf = fpathBase fp
                         , EHSem.gUniq_Inh_AGItf    = uid
                         , EHSem.opts_Inh_AGItf     = opts
                         })

foldHs :: HSSem.Inh_AGItf -> HsName -> UID -> EHCOpts -> HS.AGItf -> HSSem.Syn_AGItf
foldHs inh modNm uid opts hs
 = HSSem.wrap_AGItf (HSSem.sem_AGItf hs)
                    (inh { HSSem.opts_Inh_AGItf     = opts
                         , HSSem.gUniq_Inh_AGItf    = uid
                         , HSSem.moduleNm_Inh_AGItf = modNm
                         })


cpFoldEH :: HsName -> EHCompilePhase ()
cpFoldEH modNm
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
                 mbEH   = ecuMbEH ecu
                 ehSem  = foldEH (crsiEHInh crsi) (ecuFilePath ecu) (crsiHereUID crsi) (crsiOpts crsi) (fromJust mbEH)
         ;  when (isJust mbEH)
                 (cpUpdCU modNm (ecuStoreEHSem ehSem))
         }

cpFoldHs :: HsName -> EHCompilePhase ()
cpFoldHs modNm
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
                 mbHS   = ecuMbHS ecu
                 hsSem  = foldHs (crsiHSInh crsi) modNm (crsiHereUID crsi) (crsiOpts crsi) (fromJust mbHS)
         ;  when (isJust mbHS)
                 (cpUpdCU modNm (ecuStoreHSSem hsSem))
         }

cpGetHsImports :: HsName -> EHCompilePhase ()
cpGetHsImports modNm
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
                 mbHsSem= ecuMbHSSem ecu
                 hsSem  = fromJust mbHsSem
                 mod    = HSSem.mod_Syn_AGItf hsSem
         ;  when (isJust mbHsSem)
                 (cpUpdCU modNm (ecuStoreMod mod))
         }

cpTranslateHs2EH :: HsName -> EHCompilePhase ()
cpTranslateHs2EH modNm
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
                 opts   = crsiOpts crsi
                 fp     = ecuFilePath ecu
                 mbHsSem= ecuMbHSSem ecu
                 hsSem  = fromJust mbHsSem
                 eh     = HSSem.eh_Syn_AGItf hsSem
                 errs   = HSSem.errL_Syn_AGItf hsSem
         ;  when (isJust mbHsSem)
                 (do { cpUpdCU modNm (ecuStoreEH eh)
                     ; cpSetLimitErrsWhen 5 "Dependency/name analysis" errs
                     ; when (ehcOptEmitHS opts)
                            (lift $ putPPFile (fpathToStr (fpathSetSuff "hs2" fp)) (HSSem.pp_Syn_AGItf hsSem) 1000)
                     ; when (ehcOptShowHS opts)
                            (lift $ putWidthPPLn 120 (HSSem.pp_Syn_AGItf hsSem))
                     })
         }

cpTranslateEH2Core :: HsName -> EHCompilePhase ()
cpTranslateEH2Core modNm
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
                 opts   = crsiOpts crsi
                 mbEHSem= ecuMbEHSem ecu
                 fp     = ecuFilePath ecu
                 ehSem  = fromJust mbEHSem
                 core   = EHSem.cmodule_Syn_AGItf ehSem
                 errs   = EHSem.allErrL_Syn_AGItf ehSem
         ;  when (isJust mbEHSem)
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
                 u1     = uidChild . crsiHereUID $ crsi
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
                 u1     = uidChild . crsiHereUID $ crsi
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
  = cpSeq [ cpStepUID
          , cpFoldEH modNm
          , cpTranslateEH2Core modNm
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
cpCompileCU :: Maybe HaskellState -> HsName -> EHCompilePhase ()
cpCompileCU targHSState modNm
  = do { cr <- get
       ; let ecu   = crCU modNm cr
             opts  = crsiOpts (crStateInfo cr)
             msg m = lift (putCompileMsg VerboseNormal (ehcOptVerbosity opts) m Nothing modNm (ecuFilePath ecu))
             -- msg m = lift (putCompileMsg VerboseNormal (ehcOptVerbosity opts) (m ++ " (" ++ show (ecuState ecu) ++ "/" ++ fpathSuff (ecuFilePath ecu) ++ ")") Nothing modNm (ecuFilePath ecu))
       ; case (ecuState ecu,targHSState) of
           (ECUSHaskell HSStart,Just HSOnlyImports)
             -> do { cpSeq [cpParseHs modNm, cpStepUID, cpFoldHs modNm, cpGetHsImports modNm]
                   ; cpUpdCU modNm (ecuStoreState (ECUSHaskell HSOnlyImports))
                   }
           (ECUSHaskell HSOnlyImports,Just HSOnlyImports)
             -> return ()
           (ECUSHaskell HSOnlyImports,_)
             -> do { msg "Compiling Haskell"
                   ; cpProcessHs modNm
                   ; cpUpdCU modNm (ecuStoreState (ECUSHaskell HSAllSem))
                   }
           (ECUSHaskell HSStart,_)
             -> do { msg "Compiling Haskell"
                   ; cpSeq [cpParseHs modNm, cpStepUID, cpFoldHs modNm, cpProcessHs modNm]
                   ; cpUpdCU modNm (ecuStoreState (ECUSHaskell HSAllSem))
                   }
           (ECUSEh,_)
             -> do { msg "Compiling EH"
                   ; cpSeq [cpParseEH modNm, cpProcessEH modNm]
                   }
           (ECUSGrin,_)
             -> do { msg "Compiling Grin"
                   ; cpSeq [cpParseGrin modNm, cpProcessGrin modNm]
                   }
           _ -> do { msg "Skipping"
                   }
       }
%%]

%%[8
%%]
crCompileCG :: Maybe HaskellState -> [HsName] -> EHCompileRun -> IO EHCompileRun
crCompileCG targHSState modNmL cr
  = do { let grpNm = HNm $ concat $ intersperse "-" $ map show $ modNmL
             crsi  = crStateInfo cr
             cr2   = cr {crStateInfo = crsi {crsiGrpMp = Map.insert grpNm (emptyECG {ecgNm = grpNm, ecgModL = modNmL}) (crsiGrpMp crsi)}}
             crSetNm = crSeq $ map (\n -> crUpdCU n (\ecu -> return (ecu {ecuGrpNm = grpNm}))) modNmL
       ; crSetNm cr2
       }

%%[8
cpCompileOrderedCUs :: EHCompilePhase ()
cpCompileOrderedCUs
 = do { modNmLL <- gets crCompileOrder
      ; cpSeq (map (cpCompileCU Nothing . head) modNmLL)
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
                            wrRes = HSSem.wrap_AGItf res
                                      (HSSem.Inh_AGItf
                                        { HSSem.opts_Inh_AGItf = opts
                                        -- , HSSem.idGam_Inh_AGItf = emptyGam
                                        })
                      ; putStrLn (disp (ppErrL $ HSSem.errL_Syn_AGItf $ wrRes) 1000 "")
                      ; when (ehcOptShowHS opts)
                             (putStrLn (disp (HSSem.pp_Syn_AGItf wrRes) 1000 ""))
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
             topModNm       = mkHNm (fpathBase fp)
             searchPath     = ehcOptSearchPath opts ++ mkInitSearchPath fp
             opts2          = opts { ehcOptSearchPath = searchPath }
             hsInh          = HSSem.Inh_AGItf { HSSem.opts_Inh_AGItf     = opts2
                                              , HSSem.idGam_Inh_AGItf    = emptyGam
                                              , HSSem.gUniq_Inh_AGItf    = uidStart
                                              , HSSem.moduleNm_Inh_AGItf = hsnUnknown
                                              }
             ehInh          = EHSem.Inh_AGItf { EHSem.baseName_Inh_AGItf = fpathBase fp
                                              , EHSem.gUniq_Inh_AGItf    = uidStart
                                              , EHSem.opts_Inh_AGItf     = opts2
                                              }
             initialState   = mkEmptyCompileRun topModNm (EHCompileRunStateInfo opts2 hsInh ehInh uidStart uidStart Map.empty)
             imp mbFp nm
               = do { mbFoundFp <- cpFindFileForFPath fileSuffMpHs searchPath (Just nm) mbFp
                    ; when (isJust mbFoundFp)
                           (cpCompileCU (Just HSOnlyImports) nm)
                    }
       ; _ <- runStateT (cpSeq [ imp (Just fp) topModNm
                               , cpImportGather (imp Nothing) topModNm
                               , cpCompileOrderedCUs
                               ]) initialState
       ; return ()
       }

%%]


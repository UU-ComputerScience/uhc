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
  = ECUSUnknown | ECUSHaskell | ECUSEh | ECUSFail
  deriving (Show,Eq)

data EHCompileUnit
  = EHCompileUnit
      { ecuFilePath          :: FPath
      , ecuModNm             :: HsName
      , ecuMbSemHS           :: Maybe HSSem.Syn_AGItf
      , ecuMbSemEH           :: Maybe EHSem.Syn_AGItf
      , ecuState             :: EHCompileUnitState
      }

emptyECU :: EHCompileUnit
emptyECU
  = EHCompileUnit
      { ecuFilePath          = emptyFPath
      , ecuModNm             = hsnUnknown
      , ecuMbSemHS           = Nothing
      , ecuMbSemEH           = Nothing
      , ecuState             = ECUSUnknown
      }

instance CompileUnitState EHCompileUnitState where
  cusDefault	= ECUSEh
  cusUnk   		= ECUSUnknown
  cusIsUnk 		= (==ECUSUnknown)
  cusIsImpKnown	= const True

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
      , crsiHSInh       :: HSSem.Inh_AGItf
      , crsiEHInh       :: EHSem.Inh_AGItf
      , crsiNextUID     :: UID
      , crsiHereUID     :: UID
      }

instance CompileRunStateInfo EHCompileRunStateInfo HsName () where
  crsiImportPosOfCUKey n i = ()

type EHCompileRun = CompileRun HsName EHCompileUnit EHCompileRunStateInfo Err

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Search path
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
type FileSuffMp = Map.Map String EHCompileUnitState

fileSuffMpHs :: FileSuffMp
fileSuffMpHs = Map.fromList [ ( "hs", ECUSHaskell ), ( "eh", ECUSEh ) ]

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Open a file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
ehOpenFile :: FPath -> IO (String,Handle)
ehOpenFile fp
  = do { let fNm = fpathToStr fp
       ; r <- if fpathIsEmpty fp
              then  return ("<stdin>",stdin)
              else  do  {  h <- openFile fNm ReadMode
                        ;  return (fNm,h)
                        }
       ; return r
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
crCompileCUParseHS :: HsName -> EHCompileRun -> IO EHCompileRun
crCompileCUParseHS modNm cr
  = do {  let ecu    = crCU modNm cr
              crsi   = crStateInfo cr
              opts   = crsiOpts crsi
              fp     = ecuFilePath ecu
       ;  (fn,fh) <-  ehOpenFile fp
       ;  tokens <- offsideScanHandle hsScanOpts fn fh
       ;  let steps = parseOffside (HSPrs.pAGItf) tokens
              (resd,_) = evalSteps steps
              res = HSSem.sem_AGItf resd
              hsSem = HSSem.wrap_AGItf res
                       ((crsiHSInh crsi) {HSSem.opts_Inh_AGItf = opts})
              -- errL = mkParseErrInfoL (getMsgs steps)
       ;  cr' <- crUpdCU modNm (\ecu -> return (ecu {ecuMbSemHS = Just hsSem})) cr
       -- ;  crSetInfos "Parse (Haskell syntax) of module" True errL cr'
       ;  crSetLimitErrsWhen 5 "Parse (Haskell syntax) of module" (map mkPPErr (getMsgs steps)) cr'
       }
%%]

%%[8
crCompileCUParseEH :: HsName -> EHCompileRun -> IO EHCompileRun
crCompileCUParseEH modNm cr
  = do {  let ecu    = crCU modNm cr
              crsi   = crStateInfo cr
              opts   = crsiOpts crsi
              fp     = ecuFilePath ecu
       ;  (fn,fh) <-  ehOpenFile fp
       ;  tokens <- offsideScanHandle ehScanOpts fn fh
       ;  let steps = parseOffside (EHPrs.pAGItf) tokens
              (resd,_) = evalSteps steps
              res = EHSem.sem_AGItf resd
              ehSem = EHSem.wrap_AGItf res
                        ((crsiEHInh crsi) {EHSem.gUniq_Inh_AGItf = crsiHereUID crsi, EHSem.baseName_Inh_AGItf = fpathBase fp})
              -- errL = mkParseErrInfoL (getMsgs steps)
       ;  cr' <- crUpdCU modNm (\ecu -> return (ecu {ecuMbSemEH = Just ehSem})) cr
       -- ;  crSetInfos "Parse (EH syntax) of module" True errL cr'
       ;  crSetLimitErrsWhen 5 "Parse (EH syntax) of module" (map mkPPErr (getMsgs steps)) cr'
       }
%%]

%%[8
crCompileCUCheckHS :: HsName -> EHCompileRun -> IO EHCompileRun
crCompileCUCheckHS modNm cr
  = do {  let ecu    = crCU modNm cr
              crsi   = crStateInfo cr
              hsSem  = fromJust (ecuMbSemHS ecu)
              ehSem  = EHSem.wrap_AGItf (EHSem.sem_AGItf (HSSem.eh_Syn_AGItf hsSem))
                        ((crsiEHInh crsi) {EHSem.gUniq_Inh_AGItf = crsiHereUID crsi, EHSem.baseName_Inh_AGItf = fpathBase (ecuFilePath ecu)})
       ;  cr' <- crUpdCU modNm (\ecu -> return (ecu {ecuMbSemEH = Just ehSem})) cr
       ;  return (cr' {crState = CRSErrInfoL "Dependency/name analysis" False ([])})
       }
%%]

%%[8
crCompileCUCheckEH :: HsName -> EHCompileRun -> IO EHCompileRun
crCompileCUCheckEH modNm cr
  = do {  let ehSem  = fromJust (ecuMbSemEH (crCU modNm cr))
       ;  case ehcOptDumpPP (crsiOpts (crStateInfo cr)) of
               Just "pp"   ->  putWidthPPLn 120 (EHSem.pp_Syn_AGItf ehSem)
               Just "ast"  ->  putPPLn (EHSem.ppAST_Syn_AGItf ehSem)
               _           ->  return ()
       ;  return (cr {crState = CRSErrInfoL "Type checking" False (EHSem.allErrL_Syn_AGItf ehSem)})
       }
%%]

%%[8
crCore1Trf :: HsName -> String -> EHCompileRun -> IO EHCompileRun
crCore1Trf modNm trfNm cr
  =  do  {  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
                 synAG  = fromJust (ecuMbSemEH ecu)
                 [u1]   = mkNewLevUIDL 1 . snd . mkNewLevUID . crsiHereUID $ crsi
                 synAG' = synAG {EHSem.cmodule_Syn_AGItf
                                    = ( case trfNm of
                                          "CER"     -> cmodTrfEtaRed
                                          "CCP"     -> cmodTrfConstProp
                                          "CRU"     -> cmodTrfRenUniq
                                          "CLU"     -> cmodTrfLetUnrec
                                          "CILA"    -> cmodTrfInlineLetAlias
                                          "CFL"     -> cmodTrfFullLazy u1
                                          "CLL"     -> cmodTrfLamLift
                                          _         -> id
                                      )
                                    . EHSem.cmodule_Syn_AGItf
                                    $ synAG}
         ;  putCompileMsg VerboseALot (ehcOptVerbosity . crsiOpts $ crsi) "Transforming" (lookup trfNm cmdLineTrfs) modNm (ecuFilePath ecu)
         ;  crUpdCU modNm (\ecu -> return (ecu {ecuMbSemEH = Just synAG'})) cr
         }
%%]

%%[8
crCoreTrf :: HsName -> [String] -> EHCompileRun -> IO EHCompileRun
crCoreTrf modNm trfNmL cr
  = trf cr
  where trf  =  crSeq . intersperse crStepUID . map (crCore1Trf modNm)
             .  filter (maybe True id . trfOptOverrides (ehcOptTrf $ crsiOpts $ crStateInfo $ cr))
             $  trfNmL
%%]

%%[8
crOutputCore :: HsName -> EHCompileRun -> IO EHCompileRun
crOutputCore modNm cr
  =  do  {  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
                 ehSem  = fromJust (ecuMbSemEH ecu)
                 fp     = ecuFilePath ecu
                 opts   = crsiOpts crsi
                 cMod   = EHSem.cmodule_Syn_AGItf ehSem
                 [u1]   = mkNewLevUIDL 1 . snd . mkNewLevUID . crsiHereUID $ crsi
                 codePP = ppCModule cMod 
         ;  when (ehcOptCore opts) (putPPFile (fpathToStr (fpathSetSuff "core" fp)) codePP 120)
         ;  when (ehcOptCoreJava opts)
                 (do  {  let (jBase,jPP) = cmodJavaSrc cMod
                             jFP = fpathSetBase jBase fp
                      ;  putPPFile (fpathToStr (fpathSetSuff "java" jFP)) jPP 120
                      })
         ;  let  grin = cmodGrin u1 cMod
                 grinPP = ppGrModule (Just []) grin
         ;  when (ehcOptCoreGrin opts)
                 (do  {  putPPFile (fpathToStr (fpathSetSuff "grin" fp)) grinPP 1000
                      ;  when (ehcOptCoreCmm opts)
                              (GRINC.doCompileGrin
                                 (Right (fp,grin))
                                 (GRINCCommon.defaultGRINCOpts
                                   { GRINCCommon.grincOptVerbosity = ehcOptVerbosity opts
                                   , GRINCCommon.grincOptGenTrace = ehcOptGenTrace opts
                                   , GRINCCommon.grincOptTimeCompile = ehcOptTimeCompile opts
                                   , GRINCCommon.grincOptDumpTrfGrin = ehcOptDumpTrfGrin opts
                                   , GRINCCommon.grincOptDumpCallGraph = ehcOptDumpCallGraph opts
                                   }))
                      })
         ;  case ehcOptDumpPP (crsiOpts crsi) of
              Just "grin"  ->  putPPLn grinPP
              _            ->  return ()
         ;  return cr
         }
%%]

%%[8
crStepUID :: EHCompileRun -> IO EHCompileRun
crStepUID cr
  = let (n,h) = mkNewLevUID (crsiNextUID crsi)
        crsi = crStateInfo cr
     in return (cr {crStateInfo = crsi {crsiNextUID = n, crsiHereUID = h}})
%%]

%%[8
crCompileCU :: HsName -> EHCompileRun -> IO EHCompileRun
crCompileCU modNm cr
  = do { let ecu   = crCU modNm cr
             opts  = crsiOpts (crStateInfo cr)
             msg m = putCompileMsg VerboseNormal (ehcOptVerbosity opts) m Nothing modNm (ecuFilePath ecu)
             -- msg m = putCompileMsg VerboseNormal (ehcOptVerbosity opts) (m ++ " (" ++ show (ecuState ecu) ++ "/" ++ fpathSuff (ecuFilePath ecu) ++ ")") Nothing modNm (ecuFilePath ecu)
       ; case ecuState ecu of
           ECUSHaskell
             -> do { msg "Compiling Haskell"
                   ; crSeq [crHSPrs,crHSSem,crEHSem] cr
                   }
           ECUSEh
             -> do { msg "Compiling EH"
                   ; crSeq [crEHPrs,crEHSem] cr
                   }
           _ -> do { msg "Skipping"
                   ; return cr
                   }
       }
  where crHSPrs
          = crSeq [ crStepUID, crCompileCUParseHS modNm
                  ]
        crHSSem
          = crSeq [            crCompileCUCheckHS modNm
                  ]
        crEHPrs
          = crSeq [ crStepUID, crCompileCUParseEH modNm
                  ]
        crEHSem
          = crSeq [            crCompileCUCheckEH modNm
                  , crStepUID, crCoreTrf modNm ["CER", "CCP", "CRU", "CLU", "CILA", "CFL", "CLL", "CFL", "CLU"]
                  , crStepUID, crOutputCore modNm
                  ]

crCompileOrderedCUs :: [[HsName]] -> EHCompileRun -> IO EHCompileRun
crCompileOrderedCUs modNmLL = crSeq (map (crCompileCU . head) modNmLL)
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
         ;  case ehcOptDumpPP opts of
              Just "pp"   ->  putStrLn (disp (EHSem.pp_Syn_AGItf wrRes) 70 "")
              Just "ast"  ->  putStrLn (disp (EHSem.ppAST_Syn_AGItf wrRes) 1000 "")
              _           ->  return ()
         ;  when (ehcOptShowTopTyPP opts)
                 (putStr (disp (EHSem.topTyPP_Syn_AGItf wrRes) 1000 ""))
         }
%%]

%%[8.doCompile -1.doCompile
doCompileRun :: String -> EHCOpts -> IO ()
doCompileRun fn opts
  = do { let fp             = mkTopLevelFPath "hs" fn
             topModNm       = HNm (fpathBase fp)
             searchPath     = mkInitSearchPath fp
             opts'          = opts { ehcOptSearchPath = ehcOptSearchPath opts ++ searchPath }
             hsInh          = HSSem.Inh_AGItf {HSSem.opts_Inh_AGItf = opts'}
             ehInh          = EHSem.Inh_AGItf {EHSem.baseName_Inh_AGItf = fpathBase fp, EHSem.gUniq_Inh_AGItf = uidStart, EHSem.opts_Inh_AGItf = opts'}
             aSetup cr      = crHandle1
                                   (\(cr,_) -> return (cr {crCompileOrder = [[topModNm]]}))
                                   (crSetFail . fst) (crState . fst)
                                   (crFindFileForFPath fileSuffMpHs (ehcOptSearchPath opts') (Just topModNm) (Just fp) cr)
             aCompile cr    = crCompileOrderedCUs (crCompileOrder cr) cr
       ; cr <- crSeq [ aSetup, aCompile ]
                 (mkEmptyCompileRun topModNm (EHCompileRunStateInfo opts' hsInh ehInh uidStart uidStart))
       ; return ()
       }
%%]


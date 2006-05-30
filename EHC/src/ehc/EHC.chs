%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module Main import(System, System.Console.GetOpt, IO, UU.Pretty, UU.Parsing, UU.Parsing.Offside, {%{EH}Base.Common}, {%{EH}Base.ScannerCommon}, {%{EH}Base.Opts})
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
            then  putStrLn (usageInfo ("version: " ++ versionInfo ++ "\n\nUsage: ehc [options] [file]\n\noptions:") ehcCmdLineOpts)
%%]
%%[8.main.ehcOptHelp -1.main.ehcOptHelp
            then  do  {  putStrLn (usageInfo ("version: " ++ versionInfo ++ "\n\nUsage: ehc [options] [file]\n\noptions:") ehcCmdLineOpts)
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
mkParseErrInfoL :: (Eq s, Show s) => [Message s (Maybe Token)] -> ErrL
mkParseErrInfoL = map (\(Msg exp pos act) -> Err_Parse (show exp) (show act))
%%]

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
      , ecuMbOut             :: Maybe EHSem.Syn_AGItf
      , ecuState             :: EHCompileUnitState
      }

emptyECU :: EHCompileUnit
emptyECU
  = EHCompileUnit
      { ecuFilePath          = emptyFPath
      , ecuModNm             = hsnUnknown
      , ecuMbOut             = Nothing
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
      , crsiP1In        :: EHSem.Inh_AGItf
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
%%% Compile actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
crCompileCUParseHS :: HsName -> EHCompileRun -> IO EHCompileRun
crCompileCUParseHS modNm cr
  = do {  let ecu    = crCU modNm cr
              crsi   = crStateInfo cr
              opts   = crsiOpts crsi
              fp     = ecuFilePath ecu
              fNm    = fpathToStr fp
              p1ib   = (crsiP1In crsi) {EHSem.gUniq_Inh_AGItf = crsiHereUID crsi, EHSem.opts_Inh_AGItf = opts, EHSem.baseName_Inh_AGItf = fpathBase fp}
       ;  (fn,fh) <-  if fpathIsEmpty fp
                      then  return ("<stdin>",stdin)
                      else  do  {  h <- openFile fNm ReadMode
                                ;  return (fNm,h)
                                }
       ;  tokens <- offsideScanHandle fn fh
       ;  let steps = parseOffside (EHPrs.pAGItf) tokens
              (resd,_) = evalSteps steps
              res = EHSem.sem_AGItf resd
              p1ob = EHSem.wrap_AGItf res p1ib
              errL = mkParseErrInfoL (getMsgs steps)
       ;  cr' <- crUpdCU modNm (\ecu -> return (ecu {ecuMbOut = Just p1ob})) cr
       ;  crSetInfos "Parse of module" True errL cr'
       }
%%]

%%[8
crCompileCUPass1HS :: HsName -> EHCompileRun -> IO EHCompileRun
crCompileCUPass1HS modNm cr
  = do { let p1ob   = fromJust (ecuMbOut (crCU modNm cr))
       ; case ehcOptDumpPP (crsiOpts (crStateInfo cr)) of
              Just "pp"   ->  putWidthPPLn 120 (EHSem.pp_Syn_AGItf p1ob)
              Just "ast"  ->  putPPLn (EHSem.ppAST_Syn_AGItf p1ob)
              _           ->  return ()
       ; return (cr {crState = CRSErrInfoL "Type checking" False (EHSem.allErrL_Syn_AGItf p1ob)})
       }
%%]

%%[8
crCore1Trf :: HsName -> String -> EHCompileRun -> IO EHCompileRun
crCore1Trf modNm trfNm cr
  =  do  {  let  ecu    = crCU modNm cr
                 crsi   = crStateInfo cr
                 synAG  = fromJust (ecuMbOut ecu)
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
         ;  crUpdCU modNm (\ecu -> return (ecu {ecuMbOut = Just synAG'})) cr
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
                 p1ob   = fromJust (ecuMbOut ecu)
                 fp     = ecuFilePath ecu
                 opts   = crsiOpts crsi
                 cMod   = EHSem.cmodule_Syn_AGItf p1ob
                 [u1]   = mkNewLevUIDL 1 . snd . mkNewLevUID . crsiHereUID $ crsi
                 codePP = ppCModule cMod 
         ;  if ehcOptCore opts
            then  putPPFile (fpathToStr (fpathSetSuff "core" fp)) codePP 120
            else  return ()
         ;  if ehcOptCoreJava opts
            then  do  {  let (jBase,jPP) = cmodJavaSrc cMod
                             jFP = fpathSetBase jBase fp
                      ;  putPPFile (fpathToStr (fpathSetSuff "java" jFP)) jPP 120
                      }
            else  return ()
         ;  let  grin = cmodGrin u1 cMod
                 grinPP = ppGrModule (Just []) grin
         ;  if ehcOptCoreGrin opts
            then  do  {  putPPFile (fpathToStr (fpathSetSuff "grin" fp)) grinPP 1000
                      ;  GRINC.doCompileGrin
                           (Right (fp,grin))
                           (GRINCCommon.defaultGRINCOpts
                             { GRINCCommon.grincOptVerbosity = ehcOptVerbosity opts
                             , GRINCCommon.grincOptGenTrace = ehcOptGenTrace opts
                             , GRINCCommon.grincOptTimeCompile = ehcOptTimeCompile opts
                             , GRINCCommon.grincOptDumpTrfGrin = ehcOptDumpTrfGrin opts
                             , GRINCCommon.grincOptDumpCallGraph = ehcOptDumpCallGraph opts
                             })
                      }
            else  return ()
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
           ECUSEh
             -> do { msg "Compiling"
                   ; crSeq
                       [ crStepUID, crCompileCUParseHS modNm
                                  , crCompileCUPass1HS modNm
                       , crStepUID, crCoreTrf modNm ["CER", "CCP", "CRU", "CLU", "CILA", "CFL", "CLL", "CFL", "CLU"]
                       , crStepUID, crOutputCore modNm
                       ] cr
                   }
           _ -> do { msg "Skipping"
                   ; return cr
                   }
       }

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
         ;  tokens <- offsideScanHandle fn fh
         ;  let steps = parseOffside (EHPrs.pAGItf) tokens
         ;  (resd,_) <- evalStepsIO show steps
         ;  let res   = EHSem.sem_AGItf resd
                wrRes = EHSem.wrap_AGItf res (EHSem.Inh_AGItf {EHSem.opts_Inh_AGItf = opts})
         ;  case ehcOptDumpPP opts of
              Just "pp"   ->  putStrLn (disp (EHSem.pp_Syn_AGItf wrRes) 70 "")
              Just "ast"  ->  putStrLn (disp (EHSem.ppAST_Syn_AGItf wrRes) 1000 "")
              _           ->  return ()
         ;  if ehcOptShowTopTyPP opts
            then  putStr (disp (EHSem.topTyPP_Syn_AGItf wrRes) 1000 "")
            else  return ()
         }
%%]

%%[8.doCompile -1.doCompile
doCompileRun :: String -> EHCOpts -> IO ()
doCompileRun fn opts
  = do { let fp             = mkTopLevelFPath "eh" fn
             topModNm       = HNm (fpathBase fp)
             searchPath     = mkInitSearchPath fp
             opts'          = opts { ehcOptSearchPath = ehcOptSearchPath opts ++ searchPath }
             p1ib           = EHSem.Inh_AGItf {EHSem.baseName_Inh_AGItf = fpathBase fp, EHSem.gUniq_Inh_AGItf = uidStart, EHSem.opts_Inh_AGItf = opts'}
             aSetup cr      = crHandle1
                                   (\(cr,_) -> return (cr {crCompileOrder = [[topModNm]]}))
                                   (crSetFail . fst) (crState . fst)
                                   (crFindFileForFPath fileSuffMpHs (ehcOptSearchPath opts') (Just topModNm) (Just fp) cr)
             aCompile cr    = crCompileOrderedCUs (crCompileOrder cr) cr
       ; cr <- crSeq [ aSetup, aCompile ]
                 (mkEmptyCompileRun topModNm (EHCompileRunStateInfo opts' p1ib uidStart uidStart))
       ; return ()
       }
%%]


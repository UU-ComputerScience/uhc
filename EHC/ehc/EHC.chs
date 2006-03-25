% $Id$

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module Main import(System, System.Console.GetOpt, IO, UU.Pretty, UU.Parsing, UU.Parsing.Offside, EHCommon, EHScannerCommon, EHOpts)
%%]

%%[1 import(qualified EHParser as EHPrs, qualified EHMainAG as EHSem, qualified HSParser as HSPrs, qualified HSMainAG as HSSem)
%%]

%%[8 import (CompileRun,EHError,EHErrorPretty,FPath,qualified Data.Map as Map,Data.Maybe,Data.List)
%%]

%%[8 import (EHCoreJava,EHCoreGrin,EHCorePretty)
%%]

%%[8 import (EHCoreTrfRenUniq,EHCoreTrfFullLazy,EHCoreTrfInlineLetAlias,EHCoreTrfLetUnrec,EHCoreTrfLamLift,EHCoreTrfConstProp)
%%]

%%[8 import (GrinCodePretty)
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
                 opts           = foldr ($) defaultEHCOpts o
         ;  if ehcoptHelp opts
%%]
%%[1.main.ehcoptHelp
            then  putStrLn (usageInfo ("version: " ++ versionInfo ++ "\n\nUsage: ehc [options] [file]\n\noptions:") ehcCmdLineOpts)
%%]
%%[8.main.ehcoptHelp -1.main.ehcoptHelp
            then  do  {  putStrLn (usageInfo ("version: " ++ versionInfo ++ "\n\nUsage: ehc [options] [file]\n\noptions:") ehcCmdLineOpts)
                      ;  putStrLn ("Transformations:\n" ++ (unlines . map (\(n,t) -> "  " ++ n ++ ": " ++ t) $ cmdLineTrfs))
                      }
%%]
%%[1.main.tl
            else  if ehcoptVersion opts
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
{-
data CompileRunState
  = CRSOk
  | CRSFail
  | CRSFailErrL ErrL
  | CRSErrInfoL String Bool ErrL -- [(ErrorCateg,Error)]
-}

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

{-
data CompileRun
  = CompileRun
      { crCUCache       :: Map.Map HsName CompileUnit
      , crCompileOrder  :: [[HsName]]
      , crOpts          :: EHCOpts
      , crState         :: CompileRunState
      , crP1In          :: EHSem.Inh_AGItf
      , crNextUID       :: UID
      , crHereUID       :: UID
      }

crHandle1 :: (a -> IO b) -> (a -> b) -> (a -> CompileRunState) -> IO a -> IO b
crHandle1 action err errs it
  = do { v <- it
       ; case errs v of
           CRSFailErrL es
             -> do { putPPLn (ppErrL es)
                   ; return (err v)
                   }
           CRSErrInfoL about doPrint is
             -> do { if null is then return () else putPPLn (about >#< "found errors" >-< e)
                   ; if not (null is) then return (err v) else action v
                   }
             where e = if doPrint then ppErrL is else empty
           CRSFail
             -> return (err v)
           CRSOk
             -> action v
       }

crSetFail :: CompileRun -> CompileRun
crSetFail cr = cr {crState = CRSFail}

crSetOk :: CompileRun -> CompileRun
crSetOk cr = cr {crState = CRSOk}

crSetErrs :: ErrL -> CompileRun -> CompileRun
crSetErrs es cr
  = case es of
      [] -> cr
      _  -> cr {crState = CRSFailErrL es}

crSetInfos :: String -> Bool -> ErrL -> CompileRun -> CompileRun
crSetInfos msg dp is cr
  = case is of
      [] -> cr
      _  -> cr {crState = CRSErrInfoL msg dp is}

crMbCU :: HsName -> CompileRun -> Maybe CompileUnit
crMbCU modNm cr = Map.lookup modNm (crCUCache cr)

crCU :: HsName -> CompileRun -> CompileUnit
crCU modNm = fromJust . crMbCU modNm

crCUState :: HsName -> EHCompileRun -> EHCompileUnitState
crCUState modNm cr = maybe ECUSUnknown ecuState (crMbCU modNm cr)

crCUFPath :: HsName -> EHCompileRun -> FPath
crCUFPath modNm cr = maybe emptyFPath ecuFilePath (crMbCU modNm cr)

crUpdCU :: HsName -> (CompileUnit -> IO CompileUnit) -> CompileRun -> IO CompileRun
crUpdCU modNm upd cr
  = do { cu <- maybe (upd emptyECU) upd (crMbCU modNm cr)
       ; return (cr {crCUCache = Map.insert modNm cu (crCUCache cr)})
       }

crSeq :: [CompileRun -> IO CompileRun] -> CompileRun -> IO CompileRun
crSeq []      cr = return cr
crSeq (a:as)  cr = crHandle1 (\cr -> crSeq as (crSetOk cr)) crSetFail crState (a cr)
-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Search path
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
type FileSuffMp = Map.Map String EHCompileUnitState

fileSuffMpHs :: FileSuffMp
fileSuffMpHs = Map.fromList [ ( "hs", ECUSHaskell ), ( "eh", ECUSEh ) ]

{-
fileSuffLookup :: String -> FileSuffMp -> EHCompileUnitState
fileSuffLookup s m = maybe ECUSUnknown id $ lookup s m

pathsSearchForReadableFile :: [String] -> FileSuffMp -> FPath -> IO (Maybe (FPath,EHCompileUnitState))
pathsSearchForReadableFile paths suffs fp
  = let select f l
          = do { finds <- mapM f l
               ; return (listToMaybe . catMaybes $ finds)
               }
        tryToOpen mbSuff fp
          = do { let (cus,fp') = maybe (ECUSEh,fp) (\(suff,cus) -> (cus,fpathSetSuff suff fp)) mbSuff
               ; fExists <- doesFileExist (fpathToStr fp')
               ; if fExists
                  then return (Just (fp',cus))
                  else return Nothing
               }
        tryToOpenWithSuffs suffs fp
          = case suffs of
              [] -> tryToOpen Nothing fp
              _  -> select (\(s,f) -> tryToOpen (Just s) f) (zip suffs (repeat fp))
        tryToOpenInDir dir
          = select (tryToOpenWithSuffs suffs) [fpathSetDir dir fp,fpathPrependDir dir fp]
     in select tryToOpenInDir paths
-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
{-
crFindFPath :: Maybe HsName -> FileSuffMp -> FPath -> EHCompileRun -> IO (EHCompileRun,Maybe FPath)
crFindFPath mbModNm suffs fp cr
  = case maybe ECUSUnknown (flip crCUState cr) mbModNm of
      ECUSUnknown
        -> do { let opts = crsiOpts (crStateInfo cr)
              ; fpMaybe <- searchPathForReadableFile (ehcoptSearchPath opts) (map fst suffs) fp
              ; case fpMaybe of
                  Nothing
                    -> return (crSetErrs [Err_FileNotFound (fpathToStr fp) (ehcoptSearchPath opts)] cr,Nothing)
                  Just ff
                    -> do { cr' <- case mbModNm of
                                     Just modNm -> crUpdCU modNm (\ecu -> return (ecu {ecuFilePath = ff, ecuState = ecus})) cr
                                     Nothing    -> return cr
                          ; return (cr',Just ff)
                          }
                    where ecus = fileSuffLookup (fpathSuff ff) suffs
              }
      _ -> return (cr,maybe Nothing (\nm -> Just (crCUFPath nm cr)) mbModNm)

crFindTopLevelModule :: FPath -> EHCompileRun -> IO (EHCompileRun,Maybe FPath)
crFindTopLevelModule
  = crFindFPath Nothing []
-}
%%]

%%[8
{-
crFindModule :: HsName -> FileSuffMp -> EHCompileRun -> IO (EHCompileRun,Maybe FPath)
crFindModule modNm suffs = crFindFPath (Just modNm) suffs (mkFPath (show modNm))
-}
%%]

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
              (res,_) = evalSteps steps
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
       ; case ehcoptDumpPP (crsiOpts (crStateInfo cr)) of
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
         ;  putCompileMsg VerboseALot (ehcoptVerbosity . crsiOpts $ crsi) "Transforming" (lookup trfNm cmdLineTrfs) modNm (ecuFilePath ecu)
         ;  crUpdCU modNm (\ecu -> return (ecu {ecuMbOut = Just synAG'})) cr
         }
%%]

%%[8
crCoreTrf :: HsName -> [String] -> EHCompileRun -> IO EHCompileRun
crCoreTrf modNm trfNmL cr
  = trf cr
  where trf  =  crSeq . intersperse crStepUID . map (crCore1Trf modNm)
             .  filter (maybe True id . trfOptOverrides (ehcoptTrf $ crsiOpts $ crStateInfo $ cr))
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
         ;  if ehcoptCore opts
            then  putPPFile (fpathToStr (fpathSetSuff "core" fp)) codePP 120
            else  return ()
         ;  if ehcoptCoreJava opts
            then  do  {  let (jBase,jPP) = cmodJavaSrc cMod
                             jFP = fpathSetBase jBase fp
                      ;  putPPFile (fpathToStr (fpathSetSuff "java" jFP)) jPP 120
                      }
            else  return ()
         ;  let  grin = cmodGrin u1 cMod
                 grinPP = ppGrModule (Just []) grin
         ;  if ehcoptCoreGrin opts
            then  do  {  putPPFile (fpathToStr (fpathSetSuff "grin" fp)) grinPP 1000
                      }
            else  return ()
         ;  case ehcoptDumpPP (crsiOpts crsi) of
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
             msg m = putCompileMsg VerboseNormal (ehcoptVerbosity opts) m Nothing modNm (ecuFilePath ecu)
             -- msg m = putCompileMsg VerboseNormal (ehcoptVerbosity opts) (m ++ " (" ++ show (ecuState ecu) ++ "/" ++ fpathSuff (ecuFilePath ecu) ++ ")") Nothing modNm (ecuFilePath ecu)
       ; case ecuState ecu of
           ECUSEh
             -> do { msg "Compiling"
                   ; crSeq
                       [ crStepUID, crCompileCUParseHS modNm
                                  , crCompileCUPass1HS modNm
                       , crStepUID, crCoreTrf modNm ["CCP", "CRU", "CLU", "CILA", "CFL", "CLL", "CFL", "CLU"]
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
         ;  (res,_) <- evalStepsIO show steps
         ;  let wrRes = EHSem.wrap_AGItf res (EHSem.Inh_AGItf {EHSem.opts_Inh_AGItf = opts})
         ;  case ehcoptDumpPP opts of
              Just "pp"   ->  putStrLn (disp (EHSem.pp_Syn_AGItf wrRes) 70 "")
              Just "ast"  ->  putStrLn (disp (EHSem.ppAST_Syn_AGItf wrRes) 1000 "")
              _           ->  return ()
         ;  if ehcoptShowTopTyPP opts
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
             opts'          = opts { ehcoptSearchPath = ehcoptSearchPath opts ++ searchPath }
             p1ib           = EHSem.Inh_AGItf {EHSem.baseName_Inh_AGItf = fpathBase fp, EHSem.gUniq_Inh_AGItf = uidStart, EHSem.opts_Inh_AGItf = opts'}
             aSetup cr      = crHandle1
                                   (\(cr,_) -> return (cr {crCompileOrder = [[topModNm]]}))
                                   (crSetFail . fst) (crState . fst)
                                   (crFindFileForFPath fileSuffMpHs (ehcoptSearchPath opts') (Just topModNm) (Just fp) cr)
             aCompile cr    = crCompileOrderedCUs (crCompileOrder cr) cr
       ; cr <- crSeq [ aSetup, aCompile ]
                 (mkEmptyCompileRun topModNm (EHCompileRunStateInfo opts' p1ib uidStart uidStart))
       ; return ()
       }
%%]


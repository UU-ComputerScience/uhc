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

%%[8 import (EHError,EHErrorPretty,FPath,qualified Data.Map as Map,Data.Maybe,Data.List,Directory)
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
data CompileUnitState
  = CUSUnknown | CUSHaskell | CUEH | CUSFail
  deriving (Show,Eq)

data CompileUnit
  = CompileUnit
      { cuFilePath          :: FPath
      , cuModNm             :: HsName
      , cuMbOut             :: Maybe EHSem.Syn_AGItf
      , cuState             :: CompileUnitState
      }

emptyCU :: CompileUnit
emptyCU
  = CompileUnit
      { cuFilePath          = emptyFPath
      , cuModNm             = hsnUnknown
      , cuMbOut             = Nothing
      , cuState             = CUSUnknown
      }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile run combinators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data CompileRunState
  = CRSOk
  | CRSFail
  | CRSFailErrL ErrL
  | CRSErrInfoL String Bool ErrL -- [(ErrorCateg,Error)]

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

crCUState :: HsName -> CompileRun -> CompileUnitState
crCUState modNm cr = maybe CUSUnknown cuState (crMbCU modNm cr)

crCUFPath :: HsName -> CompileRun -> FPath
crCUFPath modNm cr = maybe emptyFPath cuFilePath (crMbCU modNm cr)

crUpdCU :: HsName -> (CompileUnit -> IO CompileUnit) -> CompileRun -> IO CompileRun
crUpdCU modNm upd cr
  = do { cu <- maybe (upd emptyCU) upd (crMbCU modNm cr)
       ; return (cr {crCUCache = Map.insert modNm cu (crCUCache cr)})
       }

crSeq :: [CompileRun -> IO CompileRun] -> CompileRun -> IO CompileRun
crSeq []      cr = return cr
crSeq (a:as)  cr = crHandle1 (\cr -> crSeq as (crSetOk cr)) crSetFail crState (a cr)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Search path
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
type FileSuffMp = AssocL String CompileUnitState

fileSuffMpHs :: FileSuffMp
fileSuffMpHs = [ ( "hs", CUSHaskell ), ( "eh", CUEH ) ]

pathsSearchForReadableFile :: [String] -> FileSuffMp -> FPath -> IO (Maybe (FPath,CompileUnitState))
pathsSearchForReadableFile paths suffs fp
  = let select f l
          = do { finds <- mapM f l
               ; return (listToMaybe . catMaybes $ finds)
               }
        tryToOpen mbSuff fp
          = do { let (cus,fp') = maybe (CUEH,fp) (\(suff,cus) -> (cus,fpathSetSuff suff fp)) mbSuff
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
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
crFindFPath :: Maybe HsName -> FileSuffMp -> FPath -> CompileRun -> IO (CompileRun,Maybe FPath)
crFindFPath mbModNm suffs fp cr
  = case maybe CUSUnknown (flip crCUState cr) mbModNm of
      CUSUnknown
        -> do { let opts = crOpts cr
              ; fpMaybe <- pathsSearchForReadableFile (ehcoptSearchPath opts) suffs fp
              ; case fpMaybe of
                  Nothing
                    -> return (crSetErrs [Err_FileNotFound (fpathToStr fp) (ehcoptSearchPath opts)] cr,Nothing)
                  Just (ff,cus)
                    -> do { cr' <- case mbModNm of
                                     Just modNm -> crUpdCU modNm (\cu -> return (cu {cuFilePath = ff, cuState = cus})) cr
                                     Nothing    -> return cr
                          ; return (cr',Just ff)
                          }
              }
      _ -> return (cr,maybe Nothing (\nm -> Just (crCUFPath nm cr)) mbModNm)

crFindTopLevelModule :: FPath -> CompileRun -> IO (CompileRun,Maybe FPath)
crFindTopLevelModule
  = crFindFPath Nothing []
%%]

%%[8
crFindModule :: HsName -> FileSuffMp -> CompileRun -> IO (CompileRun,Maybe FPath)
crFindModule modNm suffs = crFindFPath (Just modNm) suffs (mkFPath (show modNm))
%%]

%%[8
crCompileCUParseHS :: HsName -> CompileRun -> IO CompileRun
crCompileCUParseHS modNm cr
  = do {  let cu     = crCU modNm cr
              opts   = crOpts cr
              fp     = cuFilePath cu
              fNm    = fpathToStr fp
              p1ib   = (crP1In cr) {EHSem.gUniq_Inh_AGItf = crHereUID cr, EHSem.opts_Inh_AGItf = opts, EHSem.baseName_Inh_AGItf = fpathBase fp}
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
       ;  cr' <- crUpdCU modNm (\cu -> return (cu {cuMbOut = Just p1ob})) cr
       ;  return (crSetInfos "Parse of module" True errL cr')
       }
%%]

%%[8
crCompileCUPass1HS :: HsName -> CompileRun -> IO CompileRun
crCompileCUPass1HS modNm cr
  = do { let p1ob   = fromJust (cuMbOut (crCU modNm cr))
       ; case ehcoptDumpPP (crOpts cr) of
              Just "pp"   ->  putWidthPPLn 120 (EHSem.pp_Syn_AGItf p1ob)
              Just "ast"  ->  putPPLn (EHSem.ppAST_Syn_AGItf p1ob)
              _           ->  return ()
       ; return (cr {crState = CRSErrInfoL "Type checking" False (EHSem.allErrL_Syn_AGItf p1ob)})
       }
%%]

%%[8
crCore1Trf :: HsName -> String -> CompileRun -> IO CompileRun
crCore1Trf modNm trfNm cr
  =  do  {  let  cu     = crCU modNm cr
                 synAG  = fromJust (cuMbOut cu)
                 [u1]   = mkNewLevUIDL 1 . snd . mkNewLevUID . crHereUID $ cr
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
         ;  putCompileMsg VerboseALot (ehcoptVerbosity . crOpts $ cr) "Transforming" (lookup trfNm cmdLineTrfs) modNm (cuFilePath cu)
         ;  crUpdCU modNm (\cu -> return (cu {cuMbOut = Just synAG'})) cr
         }
%%]

%%[8
crCoreTrf :: HsName -> [String] -> CompileRun -> IO CompileRun
crCoreTrf modNm trfNmL cr
  = trf cr
  where trf  =  crSeq . intersperse crStepUID . map (crCore1Trf modNm)
             .  filter (maybe True id . trfOptOverrides (ehcoptTrf . crOpts $ cr))
             $  trfNmL
%%]

%%[8
crOutputCore :: HsName -> CompileRun -> IO CompileRun
crOutputCore modNm cr
  =  do  {  let  cu     = crCU modNm cr
                 p1ob   = fromJust (cuMbOut cu)
                 fp     = cuFilePath cu
                 opts   = crOpts cr
                 cMod   = EHSem.cmodule_Syn_AGItf p1ob
                 [u1]   = mkNewLevUIDL 1 . snd . mkNewLevUID . crHereUID $ cr
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
         ;  case ehcoptDumpPP (crOpts cr) of
              Just "grin"  ->  putPPLn grinPP
              _            ->  return ()
         ;  return cr
         }
%%]

%%[8
crStepUID :: CompileRun -> IO CompileRun
crStepUID cr
  = let (n,h) = mkNewLevUID (crNextUID cr)
     in return (cr {crNextUID = n, crHereUID = h})
%%]

%%[8
crCompileCU :: HsName -> CompileRun -> IO CompileRun
crCompileCU modNm cr
  = do { let cu    = crCU modNm cr
             opts  = crOpts cr
             msg m = putCompileMsg VerboseNormal (ehcoptVerbosity opts) m Nothing modNm (cuFilePath cu)
       ; case cuState cu of
           CUEH
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

crCompileOrderedCUs :: [[HsName]] -> CompileRun -> IO CompileRun
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
             opts'          = opts { ehcoptSearchPath = searchPath ++ ehcoptSearchPath opts }
             p1ib           = EHSem.Inh_AGItf {EHSem.baseName_Inh_AGItf = fpathBase fp, EHSem.gUniq_Inh_AGItf = uidStart, EHSem.opts_Inh_AGItf = opts'}
             aSetup cr      = crHandle1
                                   (\(cr,_) -> return (cr {crCompileOrder = [[topModNm]]}))
                                   (crSetFail . fst) (crState . fst)
                                   (crFindFPath (Just topModNm) [] fp cr)
             aCompile cr    = crCompileOrderedCUs (crCompileOrder cr) cr
       ; cr <- crSeq [ aSetup, aCompile ]
                 (CompileRun
                    { crCUCache = Map.empty, crCompileOrder = []
                    , crOpts = opts', crP1In = p1ib
                    , crState = CRSOk, crNextUID = uidStart, crHereUID = uidStart
                    })
       ; return ()
       }
%%]


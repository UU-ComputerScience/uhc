% $Id$

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module Main import(System, GetOpt, IO, UU.Pretty, UU.Parsing, UU.Parsing.Offside, EHCommon, EHOpts,EHParser, EHMainAG)
%%]

%%[8 import (EHScanner,EHError,EHErrorPretty,FPath,FiniteMap,Maybe,List,Directory)
%%]

%%[8 import (EHCoreJava,EHCoreGrin,EHCorePretty)
%%]

%%[8 import (EHCoreTrfRenUniq,EHCoreTrfFullLazy,EHCoreTrfInlineLetAlias,EHCoreTrfLetUnrec,EHCoreTrfLamLift,EHCoreTrfConstProp)
%%]

%%[8 import (GrinCodePretty)
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
            then  putStrLn (usageInfo "Usage: ehc [options] [file]\n\noptions:" ehcCmdLineOpts)
            else  if null errs
                  then  doCompileRun (if null n then "" else head n) opts
                  else  putStr (head errs)
         }
%%]

%%[8.main -1.main
main :: IO ()
main
  =  do  {  args <- getArgs
         ;  let  oo@(o,n,errs)  = getOpt Permute ehcCmdLineOpts args
                 opts           = foldr ($) defaultEHCOpts o
         ;  if ehcoptHelp opts
            then  do  {  putStrLn (usageInfo "Usage ehc [options] [file]\n\noptions:" ehcCmdLineOpts)
                      ;  putStrLn ("Transformations:\n" ++ (unlines . map (\(n,t) -> "  " ++ n ++ ": " ++ t) $ cmdLineTrfs))
                      }
            else  if null errs
                  then  doCompileRun (if null n then "" else head n) opts
                  else  putStr (head errs)
         }
%%]

%%[doCompileA.1
  =  do  {  (fn,fh) <-  if null filename
                        then  return ("<stdin>",stdin)
                        else  do  {  h <- openFile filename ReadMode
                                  ;  return (filename,h)
                                  }
         ;  tokens <- offsideScanHandle fn fh
         ;  let steps = parseOffside (pAGItf) tokens
         ;  (res,_) <- evalStepsIO show steps
%%]

%%[doCompileB.1
         ;  let wrRes = wrap_AGItf res (Inh_AGItf {opts_Inh_AGItf = opts})
%%]

%%[doCompileB.8
         ;  let fp = mkFPath fn
                wrRes = wrap_AGItf res (Inh_AGItf {gUniq_Inh_AGItf = uidStart, opts_Inh_AGItf = opts, baseName_Inh_AGItf = fpathBase fp})
%%]

%%[doCompileC.1
         ;  case ehcoptDumpPP opts of
              Just "pp"   ->  putStrLn (disp (pp_Syn_AGItf wrRes) 70 "")
              Just "ast"  ->  putStrLn (disp (ppAST_Syn_AGItf wrRes) 1000 "")
              _           ->  return ()
%%]

%%[doCompileC.8
         ;  let codePP = ppCModule (cmodule_Syn_AGItf wrRes)
         ;  case ehcoptDumpPP opts of
              Just "pp"    ->  putStrLn (disp (pp_Syn_AGItf wrRes) 70 "")
              Just "ast"   ->  putStrLn (disp (ppAST_Syn_AGItf wrRes) 1000 "")
              Just "code"  ->  putStrLn (disp codePP 120 "")
              _            ->  return ()
%%]

%%[doCompileD.1
         ;  if ehcoptShowTopTyPP opts
            then  putStr (disp (topTyPP_Syn_AGItf wrRes) 1000 "")
            else  return ()
%%]

%%[1.doCompile
doCompileRun :: String -> EHCOpts -> IO ()
doCompileRun filename opts
%%@doCompileA.1
%%@doCompileB.1
%%@doCompileC.1
%%@doCompileD.1
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
      , cuMbOut             :: Maybe Syn_AGItf
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
      { crCUCache       :: FiniteMap HsName CompileUnit
      , crCompileOrder  :: [[HsName]]
      , crOpts          :: EHCOpts
      , crState         :: CompileRunState
      , crP1In          :: Inh_AGItf
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
crMbCU modNm cr = lookupFM (crCUCache cr) modNm

crCU :: HsName -> CompileRun -> CompileUnit
crCU modNm = fromJust . crMbCU modNm

crCUState :: HsName -> CompileRun -> CompileUnitState
crCUState modNm cr = maybe CUSUnknown cuState (crMbCU modNm cr)

crCUFPath :: HsName -> CompileRun -> FPath
crCUFPath modNm cr = maybe emptyFPath cuFilePath (crMbCU modNm cr)

crUpdCU :: HsName -> (CompileUnit -> IO CompileUnit) -> CompileRun -> IO CompileRun
crUpdCU modNm upd cr
  = do { cu <- maybe (upd emptyCU) upd (crMbCU modNm cr)
       ; return (cr {crCUCache = addToFM (crCUCache cr) modNm cu})
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
              p1ib   = (crP1In cr) {gUniq_Inh_AGItf = crHereUID cr, opts_Inh_AGItf = opts, baseName_Inh_AGItf = fpathBase fp}
       ;  (fn,fh) <-  if fpathIsEmpty fp
                      then  return ("<stdin>",stdin)
                      else  do  {  h <- openFile fNm ReadMode
                                ;  return (fNm,h)
                                }
       ;  tokens <- offsideScanHandle fn fh
       ;  let steps = parseOffside (pAGItf) tokens
              (res,_) = evalSteps steps
              p1ob = wrap_AGItf res p1ib
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
              Just "pp"   ->  putWidthPPLn 120 (pp_Syn_AGItf p1ob)
              Just "ast"  ->  putPPLn (ppAST_Syn_AGItf p1ob)
              _           ->  return ()
       ; return (cr {crState = CRSErrInfoL "Type checking" False (allErrL_Syn_AGItf p1ob)})
       }
%%]

%%[8
crCore1Trf :: HsName -> String -> CompileRun -> IO CompileRun
crCore1Trf modNm trfNm cr
  =  do  {  let  cu     = crCU modNm cr
                 synAG  = fromJust (cuMbOut cu)
                 [u1]   = mkNewLevUIDL 1 . snd . mkNewLevUID . crHereUID $ cr
                 synAG' = synAG {cmodule_Syn_AGItf
                                    = ( case trfNm of
                                          "CCP"     -> cmodTrfConstProp
                                          "CRU"     -> cmodTrfRenUniq
                                          "CLU"     -> cmodTrfLetUnrec
                                          "CILA"    -> cmodTrfInlineLetAlias
                                          "CFL"     -> cmodTrfFullLazy u1
                                          "CLL"     -> cmodTrfLamLift
                                          _         -> id
                                      )
                                    . cmodule_Syn_AGItf
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
                 cMod   = cmodule_Syn_AGItf p1ob
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

%%[8.doCompile -1.doCompile
doCompileRun :: String -> EHCOpts -> IO ()
doCompileRun fn opts
  = do { let fp             = mkTopLevelFPath "eh" fn
             topModNm       = HNm (fpathBase fp)
             searchPath     = mkInitSearchPath fp
             opts'          = opts { ehcoptSearchPath = searchPath ++ ehcoptSearchPath opts }
             p1ib           = Inh_AGItf {baseName_Inh_AGItf = fpathBase fp, gUniq_Inh_AGItf = uidStart, opts_Inh_AGItf = opts'}
             aSetup cr      = crHandle1
                                   (\(cr,_) -> return (cr {crCompileOrder = [[topModNm]]}))
                                   (crSetFail . fst) (crState . fst)
                                   (crFindFPath (Just topModNm) [] fp cr)
             aCompile cr    = crCompileOrderedCUs (crCompileOrder cr) cr
       ; cr <- crSeq [ aSetup, aCompile ]
                 (CompileRun
                    { crCUCache = emptyFM, crCompileOrder = []
                    , crOpts = opts', crP1In = p1ib
                    , crState = CRSOk, crNextUID = uidStart, crHereUID = uidStart
                    })
       ; return ()
       }
%%]


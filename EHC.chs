% $Id: EHC.lag 199 2004-05-12 19:11:13Z andres $

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module Main import(System, GetOpt, IO, UU.Pretty, UU.Parsing, UU.Parsing.Offside, EHCommon, EHParser, EHMainAG)
%%]

%%[8 import (EHScanner,EHError,EHErrorPretty,FPath,FiniteMap,Maybe,Directory)
%%]

%%[8 import (EHCoreJava,EHCoreGrin,EHCorePretty)
%%]

%%[8 import (EHCoreTrfRenUniq,EHCoreTrfFullLazy,EHCoreTrfLetUnrec)
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
         ;  let  oo@(o,n,errs)  = getOpt Permute cmdLineOpts args
                 opts           = foldr ($) defaultEHCOpts o
         ;  if ehcoptHelp opts
            then  putStrLn (usageInfo "Usage ehc [options] [file]\n\noptions:" cmdLineOpts)
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

%%[88.doCompile -1.doCompile
doCompileRun :: String -> EHCOpts -> IO ()
doCompileRun filename opts
%%@doCompileA.1
%%@doCompileB.8
%%@doCompileC.8
%%@doCompileD.1
         ;  if ehcoptCore opts
            then  do  {  writeFile (fpathToStr (fpathSetSuff "core" fp))
                            (disp codePP 120 "")
                      }
            else  return ()
         ;  if ehcoptCoreJava opts
            then  do  {  let (jBase,jPP) = cmodJavaSrc (cmodule_Syn_AGItf wrRes)
                             jFP = fpathSetBase jBase fp
                      ;  writeFile (fpathToStr (fpathSetSuff "java" jFP))
                            (disp jPP 120 "")
                      }
            else  return ()
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

crFindModule :: HsName -> FileSuffMp -> CompileRun -> IO (CompileRun,Maybe FPath)
crFindModule modNm suffs = crFindFPath (Just modNm) suffs (mkFPath (show modNm))

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

crCompileCUPass1HS :: HsName -> CompileRun -> IO CompileRun
crCompileCUPass1HS modNm cr
  = do { let p1ob   = fromJust (cuMbOut (crCU modNm cr))
       ; case ehcoptDumpPP (crOpts cr) of
              Just "pp"   ->  putPPLn (pp_Syn_AGItf p1ob)
              Just "ast"  ->  putPPLn (ppAST_Syn_AGItf p1ob)
              _           ->  return ()
       ; return (cr {crState = CRSErrInfoL "Type checking" False (allErrL_Syn_AGItf p1ob)})
       }

crTrfCore :: HsName -> CompileRun -> IO CompileRun
crTrfCore modNm cr
  =  do  {  let  cu     = crCU modNm cr
                 synAG  = fromJust (cuMbOut cu)
                 [u1,u2] = mkNewLevUIDL 2 . snd . mkNewLevUID . crHereUID $ cr
                 synAG' = synAG {cmodule_Syn_AGItf
                                    = cmodTrfFullLazy u2
                                    . cmodTrfLetUnrec
                                    . cmodTrfRenUniq u1
                                    . cmodule_Syn_AGItf
                                    $ synAG}
         ;  crUpdCU modNm (\cu -> return (cu {cuMbOut = Just synAG'})) cr
         }

crOutputCore :: HsName -> CompileRun -> IO CompileRun
crOutputCore modNm cr
  =  do  {  let  cu     = crCU modNm cr
                 p1ob   = fromJust (cuMbOut cu)
                 fp     = cuFilePath cu
                 opts   = crOpts cr
                 cMod   = cmodule_Syn_AGItf p1ob
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
         ;  return cr
         }

crStepUID :: CompileRun -> IO CompileRun
crStepUID cr
  = let (n,h) = mkNewLevUID (crNextUID cr)
     in return (cr {crNextUID = n, crHereUID = h})

crCompileCU :: HsName -> CompileRun -> IO CompileRun
crCompileCU modNm cr
  = do { let cu    = crCU modNm cr
             opts  = crOpts cr
             msg m = putCompileMsg opts m modNm (cuFilePath cu)
       ; case cuState cu of
           CUEH
             -> do { msg "Compiling"
                   ; crSeq
                       [ crStepUID, crCompileCUParseHS modNm
                                  , crCompileCUPass1HS modNm
                       , crStepUID, crTrfCore modNm
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
mkTopLevelFPath :: String -> FPath
mkTopLevelFPath fn
  = let fpNoSuff = mkFPath fn
     in maybe (fpathSetSuff "eh" fpNoSuff) (const fpNoSuff) . fpathMbSuff $ fpNoSuff

mkSearchPath :: FPath -> [String]
mkSearchPath fp = maybe [] (:[]) (fpathMbDir fp) ++ [""]

doCompileRun :: String -> EHCOpts -> IO ()
doCompileRun fn opts
  = do { let fp             = mkTopLevelFPath fn
             topModNm       = HNm (fpathBase fp)
             searchPath     = mkSearchPath fp
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


-------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------

%%[1 hs module(Main)
%%]

%%[1 hs import (System, IO, Control.Monad.State, qualified Data.Map as Map)
%%]

%%[1 hs import (System.Console.GetOpt, EH.Util.Pretty, EH.Util.Utils( panicJust ))
%%]

%%[1 hs import (EH.Util.ParseUtils, EH.Util.ParseErrPrettyPrint, EH.Util.CompileRun)
%%]

%%[1 hs import (Version, Err, Gam( emptyGam ), Common, Opts)
%%]

%%[1 hs import (qualified AbsSyn.AbsSyn1 as AS1)
%%]

%%[1 hs import (qualified Main1AG as M1)
%%]

%%[1 hs import (qualified Main2AG as M2)
%%]

%%[1 hs import (AS1.Imports, TrfAS2.GenARule, TrfAS2.GenLaTeX, KeywParser)
%%]

%%[1 hs import (Parser, Expr.Expr, FmGam)
%%]


%%[1 hs

-------------------------------------------------------------------------
-- Compile run state
-------------------------------------------------------------------------

data RCompileUnitState
  = RCUSUnknown | RCUSRuler | RCUSFail
  deriving (Show,Eq)

data RCompileUnit
  = RCompileUnit
      { rcuFilePath          :: FPath
      , rcuModNm             :: Nm
      , rcuMbOut             :: Maybe AS1.AGItf
      , rcuImpNmL            :: [Nm]
      , rcuState             :: RCompileUnitState
      }

emptyRCU :: RCompileUnit
emptyRCU
  = RCompileUnit
      { rcuFilePath          = emptyFPath
      , rcuModNm             = nmUnk
      , rcuMbOut             = Nothing
      , rcuImpNmL            = []
      , rcuState             = RCUSUnknown
      }

rcuStoreMbOut x rcu = rcu {rcuMbOut = x}
rcuStoreImpNmL x rcu = rcu {rcuImpNmL = x}

data RCompileRunStateInfo
  = RCompileRunStateInfo
      { crsiOpts        :: Opts
      , crsiImpPosMp    :: ImpModMp
      }

instance FileLocatable RCompileUnit String where
  fileLocation _ = "unknown"
  noFileLocation = "unknown"

instance CompileUnitState RCompileUnitState where
  cusDefault        = RCUSRuler
  cusUnk            = RCUSUnknown
  cusIsUnk          = (==RCUSUnknown)
  cusIsImpKnown s   = s /= RCUSUnknown

instance CompileUnit RCompileUnit Nm String RCompileUnitState where
  cuDefault         = emptyRCU
  cuFPath           = rcuFilePath
  cuLocation        = fileLocation
  cuKey             = rcuModNm
  cuState           = rcuState
  cuUpdFPath fp u   = u {rcuFilePath = fp}
  cuUpdLocation _ u = u
  cuUpdState st u   = u {rcuState = st}
  cuUpdKey   nm u   = u {rcuModNm = nm}
  cuImports         = rcuImpNmL

instance CompileRunError Err SPos where
  crePPErrL                 = ppErrPPL
  creMkNotFoundErrL p fp sp = [Err_FileNotFound p fp sp]
  creAreFatal               = errLIsFatal

instance CompileRunStateInfo RCompileRunStateInfo Nm SPos where
  crsiImportPosOfCUKey n i = Map.findWithDefault emptySPos n (crsiImpPosMp i)

instance CompileModName Nm where
  mkCMNm = Nm

type RCompileRun     = CompileRun   Nm RCompileUnit RCompileRunStateInfo Err
type RCompilePhase a = CompilePhase Nm RCompileUnit RCompileRunStateInfo Err a

-------------------------------------------------------------------------
-- Pretty printing
-------------------------------------------------------------------------

instance PP RCompileUnitState where
  pp = pp . show

instance Show RCompileUnit where
  show _ = "RCU"

instance PP RCompileUnit where
  pp u = "RCU:" >#< pp (show $ rcuFilePath $ u) >#< ": state " >#< pp (rcuState u) >#< ": impL " >#< pp (show $ rcuImpNmL u)

-------------------------------------------------------------------------
-- File suffix
-------------------------------------------------------------------------

type FileSuffMp = [(String,RCompileUnitState)]

fileSuffMp :: FileSuffMp
fileSuffMp = [ ( "rul", RCUSRuler ), ( "", RCUSRuler ), ( "*", RCUSRuler ) ]

-------------------------------------------------------------------------
-- Compile run actions
-------------------------------------------------------------------------

cpParseCU :: Nm -> RCompilePhase ()
cpParseCU modNm
  = do { cr <- get
       ; let cu     = crCU modNm cr
             fp     = cuFPath cu
             fNm    = fpathToStr fp
       ; (fn,fb,fh)
             <- if fpathIsEmpty fp
                then return ("<stdin>","<stdin>",stdin)
                else do { let fn = fpathToStr fp
                        ; h <- lift $ openFile fn ReadMode
                        ; return (fn,fpathToStr (fpathRemoveSuff fp),h)
                        }
       -- ; cpPP "crParseCU"
       ; tokens <- lift $ mkHScan fn fh
       ; let (pres,perrs) = parseToResMsgs pAGItf tokens
       ; if null perrs
         then do { let impMp = as1Imports pres
                       info = crStateInfo cr
                 ; cpUpdCU modNm (rcuStoreMbOut (Just pres) . rcuStoreImpNmL (Map.keys impMp))
                 ; modify (\cr -> (cr {crStateInfo = info {crsiImpPosMp = impMp `Map.union` crsiImpPosMp info}}))
                 }
         else cpSetLimitErrs 5 "" (map mkPPErr perrs)
       }

cpFindAndParseCU :: Maybe FPath -> Nm -> RCompilePhase ()
cpFindAndParseCU mbFp modNm
  =  do { cr <- get
        ; let opts = crsiOpts (crStateInfo cr)
              cpFind mn mbFp
                = do { _ <-  cpFindFileForFPath fileSuffMp (optSearchPath opts) (Just mn) mbFp ; return ()}
        ; cpSeq [cpFind modNm mbFp, cpParseCU modNm]
        }

cpFlattenAndCompileAllCU :: RCompilePhase ()
cpFlattenAndCompileAllCU
 = do { cr <- get
      -- ; lift $ hPutStrLn stderr (show $ crCompileOrder cr)
      ; let opts = crsiOpts (crStateInfo cr)
            isAS2 = fmAS2Fm (optGenFM opts) /= optGenFM opts
            parseRes = as1JoinAGItfs [ panicJust ("crFlattenAndCompileAllCU: " ++ show n) $ rcuMbOut $ crCU n $ cr | ns <- crCompileOrder cr, n <- ns ]
            sem1Res
              = M1.wrap_AGItf (M1.sem_AGItf parseRes)
                         (M1.Inh_AGItf
                            { M1.opts_Inh_AGItf = opts {optGenFM = fmAS2Fm (optGenFM opts)}
                            , M1.fmGam_Inh_AGItf = fmGamFromList' FmFmtCmd [ (Nm n,Expr_Var (Nm v)) | (n,v) <- optDefs opts ]
                            })
            hPutBld f h b = if f then hPutPPFile h b 2000 else return ()
            putBld  f   b = hPutBld f stdout b
            cpPutBld f b = lift $ putBld f b
            cpPutDbg = cpPutBld (optDebug opts) (M1.pp_Syn_AGItf sem1Res)
            cpMk1
              = do { let t1 = M1.as2_Syn_AGItf sem1Res
                         ((t2,_,t2errL),doPrint)
                           = case optGenFM opts of
                               FmTeX -> bld as2LaTeX
                               FmAG  -> bld as2ARule
                               FmHS  -> ((t1,empty,[]),True)
                               _ | optGenExpl opts -> ((t1,empty,[]),True)
                                 | otherwise            -> ((t1,empty,[]),False)
                           where bld f = (f opts (M1.dtInvGam_Syn_AGItf sem1Res) (M1.scGam_Syn_AGItf sem1Res) (M1.fmGam_Syn_AGItf sem1Res) (M1.rwGam_Syn_AGItf sem1Res) t1,True)
                   ; cpSeq [cpSetErrs t2errL, cpPutBld doPrint (M2.ppAS2 opts (M1.fmGam_Syn_AGItf sem1Res) t2)]
                   }
{-
            cpMk2
              = cpSeq [ cpPutBld True (M1.mkPP_Syn_AGItf sem1Res (optGenFM opts))
                      , cpPutBld (optGenExpl opts) (M1.scExplPP_Syn_AGItf sem1Res)
                      ]
            cpMk3 f
              = do { let t1 = M1.as2_Syn_AGItf sem1Res
                         (t2,t2ppDbg,t2errL)
                           = case f of
                               FmTeX -> as2LaTeX opts (M1.scGam_Syn_AGItf sem1Res) (M1.fmGam_Syn_AGItf sem1Res) (M1.rwGam_Syn_AGItf sem1Res) t1
                               FmAG  -> as2ARule opts (M1.scGam_Syn_AGItf sem1Res) (M1.fmGam_Syn_AGItf sem1Res) (M1.rwGam_Syn_AGItf sem1Res) t1
                   ; cpSeq [ cpSetErrs t2errL
                           , cpPutBld True t2ppDbg
                           , cpPutBld True (M2.ppAS2 opts t2)
                           , cpPutBld True (M1.mkPP_Syn_AGItf sem1Res f)
                           ]
                   }
-}
      ; cpSeq [ cpPutDbg
              , cpSetErrs (M1.errL_Syn_AGItf sem1Res)
              , cpMk1
{-
              , if optGenV2 opts && not isAS2
                then cpMk1
                else if not isAS2
                then cpMk2
                else case optGenFM opts of
                       FmAS2 f -> cpMk3 f
                       _       -> liftCR id
-}
              ]
      }

compileTopLevel :: FPath -> Opts -> IO ()
compileTopLevel fp opts
  = do { let topModNm       = Nm (fpathBase fp)
             opts'          = opts { optSearchPath = mkInitSearchPath fp ++ optSearchPath opts }
             cr             = mkEmptyCompileRun topModNm (RCompileRunStateInfo opts' Map.empty)
       ; _ <- runStateT (cpSeq [ cpFindAndParseCU (Just fp) topModNm
                               -- , crPP "crCompileTopLevel 1"
                               , cpImportGather (cpFindAndParseCU Nothing) topModNm
                               -- , crPP "crCompileTopLevel 2"
                               , cpFlattenAndCompileAllCU
                               ])
                         cr
       ; return ()
       }

-------------------------------------------------------------------------
-- main
-------------------------------------------------------------------------

main :: IO ()
main
  = do { args <- getArgs
       ; let oo@(o,n,errs)  = getOpt Permute cmdLineOpts args
             opts           = foldr ($) defaultOpts o
       ; if optHelp opts
         then putStrLn (usageInfo ("version: " ++ versionInfo ++ "\n\nUsage ruler [options] [file]\n\noptions:") cmdLineOpts)
         else if optVersion opts || optSvnVersion opts
         then do { let s =  (if optVersion    opts                       then versionDist else "")
                         ++ (if optVersion    opts && optSvnVersion opts then ", "        else "")
                         ++ (if optSvnVersion opts                       then versionSvn  else "")
                 ; putStr s
                 }
         else if null errs
              -- then  doCompile (if null n then emptyFPath else mkFPath (head n)) opts
              then  compileTopLevel (if null n then emptyFPath else mkFPath (head n)) opts
              else  do hPutStr stderr (head errs)
                       exitFailure
       }

{-
doCompile :: FPath -> Opts -> IO ()
doCompile fp opts
  = do { (fn,fb,fh)
             <- if fpathIsEmpty fp
                then return ("<stdin>","<stdin>",stdin)
                else do { let fn = fpathToStr fp
                        ; h <- openFile fn ReadMode
                        ; return (fn,fpathToStr (fpathRemoveSuff fp),h)
                        }
       ; tokens <- mkOffScan fn fh
       ; let (pres,perrs) = parseOffsideToResMsgs pAGItf tokens
             (showErrs,omitErrs) = splitAt 5 perrs
       ; putErr' (if null omitErrs then return () else hPutStrLn stderr "... and more parsing errors") (map mkPPErr showErrs)
       ; let res = M1.wrap_AGItf (M1.sem_AGItf pres)
                     (M1.Inh_AGItf
                        { M1.opts_Inh_AGItf = opts {optGenFM = fmAS2Fm (optGenFM opts)}
                        })
             putDbg = putBld (optDebug opts) (M1.pp_Syn_AGItf res)
             errL = M1.errL_Syn_AGItf res
       ; putDbg
       ; putErr errL
       ; let isAS2 = fmAS2Fm (optGenFM opts) /= optGenFM opts
       ; if optGenV2 opts && not isAS2
         then do { let t1 = M1.as2_Syn_AGItf res
                       ((t2,_,t2errL),doPrint)
                         = case optGenFM opts of
                             FmTeX -> bld as2LaTeX
                             FmAG  -> bld as2ARule
                             _     -> ((t1,empty,[]),False)
                         where bld f = (f opts (M1.scGam_Syn_AGItf res) (M1.fmGam_Syn_AGItf res) (M1.rwGam_Syn_AGItf res) t1,True)
                 ; putErr t2errL
                 ; putBld doPrint (M2.ppAS2 opts t2)
                 }
         else if not isAS2
         then do { putBld True (M1.mkPP_Syn_AGItf res (optGenFM opts))
                 ; putBld (optGenExpl opts) (M1.scExplPP_Syn_AGItf res)
                 }
         else case optGenFM opts of
                FmAS2 f
                    -> do { putErr t2errL
                          ; putBld True t2ppDbg
                          ; putBld True (M2.ppAS2 opts t2)
                          ; putBld True (M1.mkPP_Syn_AGItf res f)
                          }
                    where t1 = M1.as2_Syn_AGItf res
                          (t2,t2ppDbg,t2errL)
                            = case f of
                                FmTeX -> as2LaTeX opts (M1.scGam_Syn_AGItf res) (M1.fmGam_Syn_AGItf res) (M1.rwGam_Syn_AGItf res) t1
                                FmAG  -> as2ARule opts (M1.scGam_Syn_AGItf res) (M1.fmGam_Syn_AGItf res) (M1.rwGam_Syn_AGItf res) t1
                _   -> return ()
       }
  where hPutBld f h b = if f then hPutPPFile h b 2000 else return ()
        putBld  f   b = hPutBld f stdout b
        -- putErr' :: IO () -> [Err] -> IO ()
        putErr' m e   = if null e
                        then return ()
                        else do { hPutBld True stderr (ppErrPPL e)
                                ; m
                                ; if errLIsFatal e then exitFailure else return ()
                                }
        -- putErr :: [Err] -> IO ()
        putErr        = putErr' (return ())
-}

%%]

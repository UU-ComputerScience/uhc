-------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------

module Main where

import System
-- import System.Exit
import IO
import qualified Data.Map as Map
import System.Console.GetOpt
import Utils( panicJust )
import ParseUtils
import ParseErrPrettyPrint
import CompileRun
import Version
import Err
import Common
import Opts
import qualified RulerAbsSyn1 as AS1
import qualified Main1AG as M1
import qualified Main2AG as M2
import RulerAS1Imports
import TrfAS2GenARule
import TrfAS2GenLaTeX
import KeywParser
import RulerParser

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

data RCompileRunStateInfo
  = RCompileRunStateInfo
      { crsiOpts        :: Opts
      , crsiImpPosMp    :: ImpModMp
      }

instance CompileUnitState RCompileUnitState where
  cusDefault		= RCUSRuler
  cusUnk            = RCUSUnknown
  cusIsUnk          = (==RCUSUnknown)
  cusIsImpKnown s   = s /= RCUSUnknown

instance CompileUnit RCompileUnit Nm RCompileUnitState where
  cuDefault         = emptyRCU
  cuFPath           = rcuFilePath
  cuKey             = rcuModNm
  cuState           = rcuState
  cuUpdFPath fp u   = u {rcuFilePath = fp}
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

type RCompileRun = CompileRun Nm RCompileUnit RCompileRunStateInfo Err

-------------------------------------------------------------------------
-- Pretty printing
-------------------------------------------------------------------------

instance PP RCompileUnitState where
  pp = pp . show

instance Show RCompileUnit where
  show _ = "RCU"

instance PP RCompileUnit where
  pp u = "RCU:" >#< pp (show $ rcuFilePath $ u) >#< ":" >#< pp (rcuState u)

-------------------------------------------------------------------------
-- File suffix
-------------------------------------------------------------------------

type FileSuffMp = Map.Map String RCompileUnitState

fileSuffMp :: FileSuffMp
fileSuffMp = Map.fromList [ ( "rul", RCUSRuler ), ( "", RCUSRuler ), ( "*", RCUSRuler ) ]

-------------------------------------------------------------------------
-- Compile run actions
-------------------------------------------------------------------------

crParseCU :: Nm -> RCompileRun -> IO RCompileRun
crParseCU modNm cr
  = do { let cu     = crCU modNm cr
             fp     = cuFPath cu
             fNm    = fpathToStr fp
       ; (fn,fb,fh)
             <- if fpathIsEmpty fp
                then return ("<stdin>","<stdin>",stdin)
                else do { let fn = fpathToStr fp
                        ; h <- openFile fn ReadMode
                        ; return (fn,fpathToStr (fpathRemoveSuff fp),h)
                        }
       -- ; crPP "crParseCU" cr
       ; tokens <- mkHScan fn fh
       ; let (pres,perrs) = parseToResMsgs pAGItf tokens
             (showErrs,omitErrs) = splitAt 5 perrs
       ; if null perrs
         then do { let impMp = as1Imports pres
                       info = crStateInfo cr
                 ; crUpdCU modNm (\cu -> return (cu {rcuMbOut = Just pres, rcuImpNmL = Map.keys impMp}))
                           (cr {crStateInfo = info {crsiImpPosMp = impMp `Map.union` crsiImpPosMp info}})
                 }
         else crSetLimitErrs 5 (map mkPPErr perrs) cr
       }
{-
crParseCU :: Nm -> RCompileRun -> IO RCompileRun
crParseCU modNm cr
  = do { let cu     = crCU modNm cr
             fp     = cuFPath cu
             fNm    = fpathToStr fp
       ; (fn,fb,fh)
             <- if fpathIsEmpty fp
                then return ("<stdin>","<stdin>",stdin)
                else do { let fn = fpathToStr fp
                        ; h <- openFile fn ReadMode
                        ; return (fn,fpathToStr (fpathRemoveSuff fp),h)
                        }
       ; tokens <- mkOffScan fn fh
       ; let (pres,perrs) = parseOffsideToResMsgs pAGItf tokens
             (showErrs,omitErrs) = splitAt 5 perrs
       ; if null perrs
         then do { let impMp = as1Imports pres
                       info = crStateInfo cr
                 ; crUpdCU modNm (\cu -> return (cu {rcuMbOut = Just pres, rcuImpNmL = Map.keys impMp}))
                           (cr {crStateInfo = info {crsiImpPosMp = impMp `Map.union` crsiImpPosMp info}})
                 }
         else crSetLimitErrs 5 (map mkPPErr perrs) cr
       }
-}

crFindAndParseCU :: Maybe FPath -> Nm -> RCompileRun -> IO RCompileRun
crFindAndParseCU mbFp modNm cr
  = crSeq [crFind modNm mbFp, crParseCU modNm] cr
  where opts = crsiOpts (crStateInfo cr)
        crFind mn mbFp cr
          = do { (cr',_) <- crFindFileForFPath fileSuffMp (optSearchPath opts) (Just mn) mbFp cr
               ; return cr'
               }

crFlattenAndCompileAllCU :: RCompileRun -> IO RCompileRun
crFlattenAndCompileAllCU cr
  = crSeq
      [ crPutDbg
      , crSetErrs (M1.errL_Syn_AGItf sem1Res)
      , crMk1
{-
      , if optGenV2 opts && not isAS2
        then crMk1
        else if not isAS2
        then crMk2
        else case optGenFM opts of
               FmAS2 f -> crMk3 f
               _       -> liftCR id
-}
      ]
      cr
  where opts = crsiOpts (crStateInfo cr)
        isAS2 = fmAS2Fm (optGenFM opts) /= optGenFM opts
        parseRes = as1JoinAGItfs [ panicJust ("crFlattenAndCompileAllCU: " ++ show n) $ rcuMbOut $ crCU n $ cr | ns <- crCompileOrder cr, n <- ns ]
        sem1Res
          = M1.wrap_AGItf (M1.sem_AGItf parseRes)
                     (M1.Inh_AGItf
                        { M1.opts_Inh_AGItf = opts {optGenFM = fmAS2Fm (optGenFM opts)}
                        })
        hPutBld f h b = if f then hPutPPFile h b 2000 else return ()
        putBld  f   b = hPutBld f stdout b
        crPutBld f b cr = do { putBld f b ; return cr }
        crPutDbg = crPutBld (optDebug opts) (M1.pp_Syn_AGItf sem1Res)
        crMk1 cr
          = do { let t1 = M1.as2_Syn_AGItf sem1Res
                     ((t2,_,t2errL),doPrint)
                       = case optGenFM opts of
                           FmTeX -> bld as2LaTeX
                           FmAG  -> bld as2ARule
                           _     -> ((t1,empty,[]),False)
                       where bld f = (f opts (M1.scGam_Syn_AGItf sem1Res) (M1.fmGam_Syn_AGItf sem1Res) (M1.rwGam_Syn_AGItf sem1Res) t1,True)
               ; crSeq [crSetErrs t2errL, crPutBld doPrint (M2.ppAS2 opts t2)] cr
               }
{-
        crMk2
          = crSeq [ crPutBld True (M1.mkPP_Syn_AGItf sem1Res (optGenFM opts))
                  , crPutBld (optGenExpl opts) (M1.scExplPP_Syn_AGItf sem1Res)
                  ]
        crMk3 f cr
          = do { let t1 = M1.as2_Syn_AGItf sem1Res
                     (t2,t2ppDbg,t2errL)
                       = case f of
                           FmTeX -> as2LaTeX opts (M1.scGam_Syn_AGItf sem1Res) (M1.fmGam_Syn_AGItf sem1Res) (M1.rwGam_Syn_AGItf sem1Res) t1
                           FmAG  -> as2ARule opts (M1.scGam_Syn_AGItf sem1Res) (M1.fmGam_Syn_AGItf sem1Res) (M1.rwGam_Syn_AGItf sem1Res) t1
               ; crSeq [ crSetErrs t2errL
                       , crPutBld True t2ppDbg
                       , crPutBld True (M2.ppAS2 opts t2)
                       , crPutBld True (M1.mkPP_Syn_AGItf sem1Res f)
                       ]
                       cr
               }
-}

crCompileTopLevel :: FPath -> Opts -> IO ()
crCompileTopLevel fp opts
  = do { let topModNm       = Nm (fpathBase fp)
             opts'          = opts { optSearchPath = mkInitSearchPath fp ++ optSearchPath opts }
             cr             = mkEmptyCompileRun topModNm (RCompileRunStateInfo opts' Map.empty)
       ; cr' <-
           crSeq [ crFindAndParseCU (Just fp) topModNm
                 -- , crPP "crCompileTopLevel 1"
                 , crImportGather (crFindAndParseCU Nothing) topModNm
                 -- , crPP "crCompileTopLevel 2"
                 , crFlattenAndCompileAllCU
                 ]
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
         else if optVersion opts
         then putStrLn versionDist
         else if null errs
              -- then  doCompile (if null n then emptyFPath else mkFPath (head n)) opts
              then  crCompileTopLevel (if null n then emptyFPath else mkFPath (head n)) opts
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

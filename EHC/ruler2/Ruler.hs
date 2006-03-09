-------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------

module Main where

import System
import System.Exit
import IO
import System.Console.GetOpt
import ParseUtils
import ParseErrPrettyPrint
import Version
import Err
import Common
import Opts
import qualified Main1AG as M1
import qualified Main2AG as M2
import TrfAS2GenARule
import TrfAS2GenLaTeX
import KeywParser
import RulerParser

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
              then  doCompile (if null n then emptyFPath else mkFPath (head n)) opts
              else  do hPutStr stderr (head errs)
                       exitFailure
       }

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


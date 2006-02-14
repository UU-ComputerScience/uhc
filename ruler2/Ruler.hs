-------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------

module Main where

-- import qualified Data.Set as Set
-- import qualified Data.Map as Map
import System
import System.Exit
import IO
import System.Console.GetOpt
import ParseUtils
import ParseErrPrettyPrint
import Version
import Common
import Opts
import qualified Main1AG as M1
import qualified Main2AG as M2
import TrfAS2GenARule
import KeywParser
import RulerParser
-- import CDoc
-- import CDocSubst
-- import Data.List

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
       ; tokens <- mkHScan fn fh
       ; let (pres,perrs) = parseToResMsgs pAGItf tokens
       ; if null perrs
         then do { let res = M1.wrap_AGItf pres
                               (M1.Inh_AGItf
                                  { M1.opts_Inh_AGItf = opts {optGenFM = fmAS2Fm (optGenFM opts)}
                                  })
                 ; let putDbg = putBld (optDebug opts) (M1.pp_Syn_AGItf res)
                       errL = M1.errL_Syn_AGItf res
                 ; if null errL
                   then do { putDbg
                           ; case optGenFM opts of
                               FmAS2 f        -> do { putBld True (M2.ppAS2 t1)
                                                    ; putBld True (M2.ppAS2 t2)
                                                    ; putBld True (M1.mkPP_Syn_AGItf res f)
                                                    }
                                              where t1 = M1.as2_Syn_AGItf res
                                                    t2 = as2ARule opts (M1.scGam_Syn_AGItf res) (M1.fmGam_Syn_AGItf res) t1
                               o | o /= FmAll -> putBld True (M1.mkPP_Syn_AGItf res (optGenFM opts))
                               _              -> return ()
                           ; putBld (optGenExpl opts) (M1.scExplPP_Syn_AGItf res)
                           }
                   else do { hPutBld True stderr (ppErrPPL errL)
                           ; putDbg
                           ; exitFailure
                           }
                 }
         else do { let (showErrs,omitErrs) = splitAt 5 perrs
                 ; hPutBld True stderr (ppErrPPL showErrs)
                 ; if null omitErrs
                   then return ()
                   else hPutStrLn stderr "... and more parsing errors"
                 }
       }
  where hPutBld f h b = if f then hPutPPFile h b 2000 else return ()
        putBld  f   b = hPutBld f stdout b


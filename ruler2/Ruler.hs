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
import Common
import MainAG
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
         then do { let res = wrap_AGItf pres
                               (Inh_AGItf
                                  { opts_Inh_AGItf = opts
                                  })
                 ; let putDbg = putBld (optDebug opts) (pp_Syn_AGItf res)
                       errL = errL_Syn_AGItf res
                 ; if null errL
                   then do { putDbg
                           ; putBld (optGenFM opts /= FmAll) (mkPP_Syn_AGItf res (optGenFM opts))
                           ; putBld (optGenExpl opts) (scExplPP_Syn_AGItf res)
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


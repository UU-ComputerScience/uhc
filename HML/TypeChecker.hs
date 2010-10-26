module Main where

import System.Console.GetOpt
import qualified EH.Util.FastSeq as Seq
import qualified EH8.Config as Cfg
import EH8.EHC.Common
import qualified EH8.HS.MainAG as HSSem
import qualified EH8.EH.MainAG as EHSem
import EH8.Scanner.Common
import UU.Parsing
import UU.Parsing.Offside
import qualified EH8.EH.Parser as EHPrs
import qualified EH8.HS.Parser as HSPrs
import EH8.EHC.InitialSetup
import EH8.EH

import qualified HML as HML
import qualified Data.Map as M

main :: IO ()
main
  =  do  {  args      <- getArgs
         ;  progName  <- getProgName
         ;  let  opts1          = defaultEHCOpts
                 oo@(o,n,errs)  = getOpt Permute ehcCmdLineOpts args
                 opts2          = foldl (flip ($)) opts1 o
         ;  let opts3 = opts2
         ;  case ehcOptImmQuit opts3 of
              Just immq     -> handleImmQuitOption immq opts3
              _ | null errs ->
                               doCompileRun (if null n then "" else head n) opts3
                | otherwise -> do { putStr (head errs)
                                  ; exitFailure
                                  }
         }

handleImmQuitOption :: ImmediateQuitOption -> EHCOpts -> IO ()
handleImmQuitOption immq opts
  = case immq of
      ImmediateQuitOption_Help
        -> do {
                progName  <- getProgName
              ; putStrLn (usageInfo (  "version: " ++ Cfg.verInfo Cfg.version ++ " (HML Build), aspects: " ++ ehcOptAspects opts
                                    ++ "\n\nUsage: " ++ progName ++ " [options] [file[.eh|.hs] ...]\n\noptions:"
                                    )
                                    ehcCmdLineOpts)
              }
        where
      ImmediateQuitOption_Version
        -> putStrLn (Cfg.verInfo Cfg.version)
      ImmediateQuitOption_Meta_Variant
        -> putStrLn Cfg.ehcDefaultVariant
      ImmediateQuitOption_Meta_Targets
        -> putStr ""
      ImmediateQuitOption_Meta_TargetDefault
        -> putStr "no-target"

doCompileRun :: String -> EHCOpts -> IO ()
doCompileRun filename opts
  =  do  {  (fn,fh) <-  if null filename
                        then  return ("<stdin>",stdin)
                        else  do  {  h <- openFile filename ReadMode
                                  ;  return (filename,h)
                                  }
         ;  let isHS = isSuffixOf ".hs" fn
         ;  when
              (ehcStopAtPoint opts >= CompilePoint_Parse)
              (do { tokens <- offsideScanHandle (if isHS then hsScanOpts else ehScanOpts) fn fh
                  ; (AGItf_AGItf resd) <-
                      if isHS
                      then do { let steps = parseOffside (HSPrs.pAGItf) tokens
                              ; (resd,_) <- evalStepsIO show steps
                              ; if ehcStopAtPoint opts >= CompilePoint_AnalHS
                                then do { let res   = HSSem.sem_AGItf resd
                                              wrRes = HSSem.wrap_AGItf res (initialHSSem opts)
                                        ; putStrLn (disp (ppErrL $ Seq.toList $ HSSem.errSq_Syn_AGItf $ wrRes) 1000 "")
                                        ; when (ehcOptShowHS opts)
                                               (putStrLn (disp (HSSem.pp_Syn_AGItf wrRes) 1000 ""))
                                        ; return (HSSem.eh_Syn_AGItf wrRes)
                                        }
                                else return undefined
                              }
                      else do { let steps = parseOffside (EHPrs.pAGItf) tokens
                              ; (resd,_) <- evalStepsIO show steps
                              ; return resd
                              }
                  ; bind_res <- HML.typeCheck resd
                  ; visualize bind_res 
                  })
         }
         
visualize :: ([TyScheme], Env, Prefix, Int) -> IO ()
visualize (result, env, prefix, freevarcount) 
  = do let hml_sem = EHSem.sem_HMLItf $ HMLItf_HMLItf result env (map HML.clean env) prefix freevarcount
           hml     = EHSem.wrap_HMLItf hml_sem (EHSem.Inh_HMLItf {})
       putStrLn (disp (EHSem.ppHML_Syn_HMLItf hml) 1000 "")
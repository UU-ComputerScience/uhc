module TestRunner where

import EH8.EH hiding (Env)
import EH8.EH as Eh
import qualified HML as HML
import TypeChecker as Ty hiding (main, doCompileRun)

import System.Console.GetOpt
import qualified EH.Util.FastSeq as Seq
import qualified EH8.Config as Cfg
import EH8.EHC.Common hiding (pp, try)
import qualified EH8.HS.MainAG as HSSem
import qualified EH8.EH.MainAG as EHSem
import EH8.Scanner.Common
import UU.Parsing
import UU.Parsing.Offside
import qualified EH8.EH.Parser as EHPrs
import qualified EH8.HS.Parser as HSPrs
import EH8.EHC.InitialSetup

import Utils

import Data.IORef

import TestDriver
import TestMangler

import Control.Exception

type ERun = ([TyScheme], Eh.Env, Prefix, Int)

testAll :: IO ()
testAll 
 = do t <- parseTest "TestMod.hs"
      let env = prepareEnvs t
      runTests env runInterpreter printResult
      
      
runInterpreter :: String -> String -> IO Result
runInterpreter cmd msg 
 = do writeBuff msg cmd
      let x = evaluate (performCheck name)
      res <- try x :: IO (Either SomeException (IO ERun))
      case res of
        Right a -> return $ OK Nothing
        Left  e -> return FAILED

printResult :: Int -> Counter -> Command -> Result -> IO ()
printResult width counter cmd res
 = do let sig     = if null (typeSigs cmd) then command cmd  else (concat $ lines $ sigValue (head $ typeSigs cmd))
          left    = sig ++ " ... "
          correct = res == result cmd
          right   = "[" ++ (if correct then "RIGHT" else "WRONG") ++ "]"
          pad     = replicate (width - length left - length right) ' '
      modifyIORef counter (+1)
      putStrLn $ left ++ pad ++ right

name :: String
name = "Scratch.hs"

writeBuff :: String -> String -> IO ()
writeBuff str cmd
  = do let content = unlines [str, "main = " ++ cmd]
       writeFile name content

performCheck :: String -> IO ERun
performCheck file
  =  do  {  args      <- getArgs
         ;  progName  <- getProgName
         ;  let  opts1          = defaultEHCOpts
                 oo@(o,n,errs)  = getOpt Permute ehcCmdLineOpts args
                 opts2          = foldl (flip ($)) opts1 o
         ;  let opts3 = opts2
         ;  case ehcOptImmQuit opts3 of
              Just immq     -> handleImmQuitOption immq opts3 >> return ([],undefined, undefined, undefined) 
              _ | null errs -> doCompileRun file opts3
                | otherwise -> do { putStr (head errs)
                                  ; exitFailure
                                  }
         }
         
doCompileRun :: String -> EHCOpts -> IO ERun
doCompileRun filename opts
  =  do  {  (fn,fh) <-  if null filename
                        then  return ("<stdin>",stdin)
                        else  do  {  h <- openFile filename ReadMode
                                  ;  return (filename,h)
                                  }
         ;  let isHS = isSuffixOf ".hs" fn
         ;    (do { tokens <- offsideScanHandle (if isHS then hsScanOpts else ehScanOpts) fn fh
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
                  ; HML.typeCheck resd
                  })
         }
         
visualize :: ERun -> IO ()
visualize (result, env, prefix, freevarcount) 
  = do let hml_sem = EHSem.sem_HMLItf $ HMLItf_HMLItf (map Utils.clean result ++ result) env (map Utils.clean env) prefix freevarcount
           hml     = EHSem.wrap_HMLItf hml_sem (EHSem.Inh_HMLItf {})
       putStrLn (disp (EHSem.ppHML_Syn_HMLItf hml) 1000 "")
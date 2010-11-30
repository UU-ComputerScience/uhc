module Terminal where

import EH8.EH
import qualified HML as HML
import TypeChecker as Ty hiding (main)

import System.Console.GetOpt
import qualified EH.Util.FastSeq as Seq
import qualified EH8.Config as Cfg
import EH8.EHC.Common hiding (pp)
import qualified EH8.HS.MainAG as HSSem
import qualified EH8.EH.MainAG as EHSem
import EH8.Scanner.Common
import UU.Parsing
import UU.Parsing.Offside
import qualified EH8.EH.Parser as EHPrs
import qualified EH8.HS.Parser as HSPrs
import EH8.EHC.InitialSetup
import EH8.EH

main :: IO ()
main 
  = do interpreter_loop

name :: String
name = "Scratch.hs"

interpreter_loop :: IO ()
interpreter_loop
  = do putStr "> "
       str <- getLine
       case str of
        ":q" -> return ()
        _    -> do writeBuff str
                   performCheck name

writeBuff :: String -> IO ()
writeBuff str
  = do prelude <- readFile "TestLude.hs"
       let content = unlines [prelude, "main = " ++ str]
       writeFile name content

performCheck :: String -> IO ()
performCheck file
  =  do  {  args      <- getArgs
         ;  progName  <- getProgName
         ;  let  opts1          = defaultEHCOpts
                 oo@(o,n,errs)  = getOpt Permute ehcCmdLineOpts args
                 opts2          = foldl (flip ($)) opts1 o
         ;  let opts3 = opts2
         ;  case ehcOptImmQuit opts3 of
              Just immq     -> handleImmQuitOption immq opts3
              _ | null errs -> do 
                                Ty.doCompileRun file opts3
                                interpreter_loop
                | otherwise -> do { putStr (head errs)
                                  ; exitFailure
                                  }
         }
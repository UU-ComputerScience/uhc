{-| Module      :  CompileUtils
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module CompileUtils
    ( module CompileUtils
    , Option(..)
    , splitFilePath, combinePathAndFile
    , when, unless
    , exitWith, ExitCode(..), getArgs
    , module ImportEnvironment
    , Module(..)
    ) where

import Args(Option(..))
import Messages(HasMessage)
import HeliumMessages(sortAndShowMessages)
import Monad(when, unless)
import Utils(splitFilePath, combinePathAndFile)
import System(exitWith, ExitCode(..), getArgs)
import Logger
import ImportEnvironment
import UHA_Syntax(Module(..))
import Data.Maybe
import Standard(searchPathMaybe)

type Phase err a = IO (Either [err] a)
type CompileOptions = ([Option], String, [String]) 

(===>) :: Phase err1 a -> (a -> Phase err2 b) -> Phase (Either err1 err2) b
p ===> f = 
   p >>= either (return . Left . map Left) 
                (\a -> f a >>= return . either (Left . map Right) Right)

doPhaseWithExit :: HasMessage err => Int -> ([err] -> String) -> CompileOptions -> Phase err a -> IO a
doPhaseWithExit nrOfMsgs code (options, fullName, doneModules) phase =
   do result <- phase
      case result of
         Left errs ->
            do unless (NoLogging `elem` options) $ 
                 sendLog (code errs) fullName doneModules options
               showErrorsAndExit errs nrOfMsgs
         Right a ->
            return a

sendLog :: String -> String -> [String] -> [Option] -> IO ()
sendLog code fullName modules options =
    logger code (Just (modules,fullName)) (DebugLogger `elem` options) (LogSpecial `elem` options)
    
enterNewPhase :: String -> [Option] -> IO ()
enterNewPhase phase options =
   when (Verbose `elem` options) $
      putStrLn (phase ++ "...")

showErrorsAndExit :: HasMessage a => [a] -> Int -> IO b
showErrorsAndExit errors maximumNumber = do
    let someErrors = take maximumNumber errors
    showMessages someErrors
    when (number > maximumNumber) $ 
        putStrLn "(...)\n"
    putStrLn ("Compilation failed with " ++ show number ++
                " error" ++ (if number == 1 then "" else "s"))
    exitWith (ExitFailure 1)
  where
    number = length errors

showMessages :: HasMessage a => [a] -> IO ()
showMessages =
    putStr . sortAndShowMessages  
    
checkExistence :: [String] -> String -> IO ()
checkExistence path name =
    do
        maybeLocation <- resolve path name
        when (isNothing maybeLocation) $ do
            putStr
                (  "Cannot find "
                ++ name
                ++ ".hs (or .lvm) in search path:\n"
                ++ unlines (map ("\t" ++) path)
                ++ "See the installation manual on setting the environment variable LVMPATH\n"
                )
            exitWith (ExitFailure 1)

resolve :: [String] -> String -> IO (Maybe String)
resolve path name = 
    do maybeFullName <- searchPathMaybe path ".hs" name
       case maybeFullName of
           Just fullName -> return (Just fullName)
           Nothing       -> searchPathMaybe path ".lvm" name

{-| Module      :  Logger
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Utils.Logger ( logger ) where

import Network
import Control.Concurrent
import Monad
import System
import Char
import List
import IO
import Helium.Compiler.Version

{-# NOTINLINE logger #-}

---------------------------------------------------
-- Global variables and settings

loggerHOSTNAME :: String
loggerHOSTNAME    = {- Bastiaan     -} -- "ikaria.cs.uu.nl" 
                    {- Jurriaan     -} -- "cox.cs.uu.nl" 
                    {- Test         -} -- "localhost"
                    {- StudentenNet -} "shell.students.cs.uu.nl" 
                    
loggerPORTNUMBER, loggerDELAY, loggerTRIES :: Int
loggerPORTNUMBER  = 5010
loggerDELAY       = 10000    -- in micro-seconds
loggerTRIES       = 2

loggerSEPARATOR, loggerTERMINATOR, loggerUSERNAME, loggerDEFAULTNAME :: String
loggerSEPARATOR      = "\NUL\NUL\n"
loggerTERMINATOR     = "\SOH\SOH\n"
loggerUSERNAME       = "USERNAME"
loggerDEFAULTNAME    = "unknown"

loggerADMINSEPARATOR, loggerESCAPECHAR :: Char
loggerADMINSEPARATOR = '|'
loggerESCAPECHAR     = '\\'

loggerESCAPABLES :: String
loggerESCAPABLES     = [loggerADMINSEPARATOR, loggerESCAPECHAR]

loggerENABLED :: Bool
loggerENABLED        = True

debug :: String -> Bool -> IO ()
debug s loggerDEBUGMODE = when loggerDEBUGMODE (putStrLn s)

------------------------------------------------------
-- Normalization/escaping functions

normalizeName :: String -> String
normalizeName name = let 
                       newname = map toLower (filter isAlphaNum name)
                     in   
                       if null newname then loggerDEFAULTNAME else newname

-- Escapes all characters from the list loggerESCAPABLES
escape :: String -> String
escape []     = []
escape (x:xs) = if (x `elem` loggerESCAPABLES) 
                then loggerESCAPECHAR : rest 
                else rest
                where 
                  rest = x:(escape xs)

-- Remove line breaks and escape special characters                
normalize :: String -> String
normalize xs = escape (filter ((/=) '\n') xs)

------------------------------------------------------
-- The function to send a message to a socket

logger :: String -> Maybe ([String],String) -> Bool -> Bool -> IO ()
logger logcode maybeSources loggerDEBUGMODE loggerLOGSPECIAL
    | not loggerENABLED || isInterpreterModule maybeSources = return ()
    | otherwise      = do
        username <- (getEnv loggerUSERNAME) `catch` (\_ -> return loggerDEFAULTNAME)
        optionString  <- getArgs
        sources  <- case maybeSources of 
            Nothing -> 
                return (loggerTERMINATOR)
            Just (imports,hsFile) -> 
                do let allHsFiles = hsFile:imports
                       allFiles   = allHsFiles ++ map toTypeFile allHsFiles
                   xs <- mapM (getContentOfFile loggerDEBUGMODE) allFiles
                   return (concat (loggerSEPARATOR:xs)++loggerTERMINATOR) 
                     `catch` (\_ -> return (loggerTERMINATOR) )
        {- putStr (normalizeName username ++ 
                       (loggerADMINSEPARATOR : normalize logcode) ++ 
                       (loggerADMINSEPARATOR : normalize version) ++
                       (loggerADMINSEPARATOR : normalize (unwords optionString)) ++ 
                       "\n" ++sources) -}      
        let specialLogcode = if loggerLOGSPECIAL then map toLower logcode else map toUpper logcode
        sendLogString (normalizeName username ++ 
                       (loggerADMINSEPARATOR : normalize specialLogcode) ++ 
                       (loggerADMINSEPARATOR : normalize version) ++
                       (loggerADMINSEPARATOR : normalize (unwords optionString)) ++ 
                       "\n" ++sources
                      ) loggerDEBUGMODE

toTypeFile :: String -> String
toTypeFile fullName = fullNameNoExt ++ ".type"
 where
   (path, baseName, _) = splitFilePath fullName
   fullNameNoExt       = combinePathAndFile path baseName     
                     
getContentOfFile :: Bool -> String -> IO String
getContentOfFile loggerDEBUGMODE name =    
   do program <- readFile name                                                        
      debug ("Logging file " ++ name) loggerDEBUGMODE
      return (  fileNameWithoutPath name
             ++ "\n" 
             ++ program
             ++ "\n"                
             ++ loggerSEPARATOR 
             )
 `catch`
    (\_ -> return "")
    
isInterpreterModule :: Maybe ([String],String) -> Bool
isInterpreterModule Nothing = False
isInterpreterModule (Just (_, hsFile)) = fileNameWithoutPath hsFile == "Interpreter.hs"

sendLogString :: String -> Bool -> IO ()
sendLogString message loggerDEBUGMODE = withSocketsDo (rec 0)
 where
    rec i = do --installHandler sigPIPE Ignore Nothing
             handle <- connectTo loggerHOSTNAME (PortNumber (fromIntegral loggerPORTNUMBER))
             hSetBuffering handle (BlockBuffering (Just 1024))
             sendToAndFlush handle message loggerDEBUGMODE
          `catch`       
              \exception -> 
                 if i+1 >= loggerTRIES 
                   then debug ( "Could not make a connection: no send (" ++ show exception ++ ")" ) loggerDEBUGMODE
                   else do debug ( "Could not make a connection: sleeping (" ++ show exception ++ ")" ) loggerDEBUGMODE
                           threadDelay loggerDELAY
                           rec (i+1)
                
{- from Utils.hs.....because of the import-dependencies, it is not possible to import 
   this function directly -}
splitFilePath :: String -> (String, String, String)
splitFilePath filePath = 
    let slashes = "\\/"
        (revFileName, revPath) = span (`notElem` slashes) (reverse filePath)
        (baseName, ext)  = span (/= '.') (reverse revFileName)
    in (reverse revPath, baseName, dropWhile (== '.') ext)
    
combinePathAndFile :: String -> String -> String
combinePathAndFile path file =
    case path of 
        "" -> file
        _  | last path == '/' -> path ++ file
           | otherwise        -> path ++ "/" ++ file
        
fileNameWithoutPath :: String -> String
fileNameWithoutPath filePath = 
    let slashes = "\\/"
        (revFileName, _) = span (`notElem` slashes) (reverse filePath)
    in reverse revFileName

sendToAndFlush :: Handle        -- Hostname
               -> String        -- Message to send
               -> Bool          -- Debug logger?
               -> IO ()               
sendToAndFlush handle msg loggerDEBUGMODE = do  
  hPutStr handle msg
  hPutStr handle loggerSEPARATOR
  hFlush handle
--  b1 <- hIsWritable s
--  b2 <- hIsReadable s
--  putStrLn ((if b1 then "writable" else "not writable") ++ " and " ++ 
--      (if b2 then "readable" else "not readable"))
  debug "Waiting for a handshake"  loggerDEBUGMODE
  handshake <- getRetriedLine 0
  debug ("Received a handshake: " ++ show handshake) loggerDEBUGMODE
--  hClose handle
  where
    getRetriedLine i = 
      do
        line <- hGetLine handle
        return line
      `catch`
        \_ -> 
          if i+1 >= loggerTRIES 
            then do
                   debug "Did not receive anything back" loggerDEBUGMODE
                   return ""
            else do 
                   debug "Waiting to try again" loggerDEBUGMODE
                   threadDelay loggerDELAY
                   getRetriedLine (i+1)    

{- ----------------------------------------------------------------------------------------
   what    : Locale Information
   expected: ok
---------------------------------------------------------------------------------------- -}

module Main where

import System.Locale

main :: IO ()
main = 
  let def = defaultTimeLocale 
      ds  = wDays def
      ms  = months def
      ap  = amPm def
      dtf = dateTimeFmt def
      df  = dateFmt def
      tf  = timeFmt def
      tf' = time12Fmt def
  in  do putStrLn $ show ds
         putStrLn $ show ms
         putStrLn $ show ap
         putStrLn dtf
         putStrLn df
         putStrLn tf
         putStrLn tf'
--         hFlush stdout

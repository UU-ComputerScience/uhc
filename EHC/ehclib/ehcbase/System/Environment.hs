{-# LANGUAGE ImplicitPrelude #-}	-- required for haddock

-- 20080423, AD: A provisional implementation of getArgs + getProgName

module System.Environment
    ( 
      getArgs,	     -- :: IO [String]
      getProgName    -- :: IO String
{-
      getEnv,        -- :: String -> IO String
      getEnvironment
-}
  ) where

-- foreign import ccall primGetProgArgv :: [String]
foreign import ccall primGetArgC :: Int
foreign import ccall primGetArgVAt :: Int -> PackedString

getArgs       :: IO [String]
getArgs       =  ioFromPrim (\_ -> [ packedStringToString $ primGetArgVAt i | i <- [1..primGetArgC-1] ])

getProgName       :: IO String
getProgName       =  ioFromPrim (\_ -> packedStringToString $ primGetArgVAt 0)




{-| Module      :  Logger
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Utils.Logger ( logger ) where

{-# NOTINLINE logger #-}

logger :: String -> Maybe ([String],String) -> Bool -> Bool -> IO ()
logger _ _ _ _ = return ()
  


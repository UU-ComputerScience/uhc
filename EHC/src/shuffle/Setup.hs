{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
import Distribution.Simple (defaultMainWithHooks)
import Distribution.Simple.UUAGC (uuagcLibUserHook, uuagcUserHook')
import UU.UUAGC (uuagc)
import System.Environment (getEnv)
import System.IO.Error (tryIOError)

main = do 
  custom_uuagc <- tryIOError (getEnv "UUAGC")
  case custom_uuagc of
    Right uuagc_path -> defaultMainWithHooks $ uuagcUserHook' uuagc_path
    Left  _          -> defaultMainWithHooks $ uuagcLibUserHook uuagc

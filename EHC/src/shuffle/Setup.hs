{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
import Distribution.Simple (defaultMainWithHooks)
import Distribution.Simple.UUAGC (uuagcLibUserHook, uuagcUserHook')
import UU.UUAGC (uuagc)
import System.Environment (getEnv)

main = do custom_uuagc <- getEnv "UUAGC"
          defaultMainWithHooks (uuagcUserHook' custom_uuagc)

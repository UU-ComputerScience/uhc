-- | Show how to generate a simple HelloWorld program using
-- the UHC Core API.
module CoreApi where

import UHC.Light.Compiler.Core.API
import UHC.Util.Serialize
import UHC.Util.Pretty

main :: IO ()
main = do
  -- dump as textual core (just for debugging)
  -- (if the tcr file would have the same name as the bcr file, it could get picked
  -- by uhc instead of the bcr file. We always want to use the bcr file for
  -- actual compilation.)
  putPPFile "CoreApiProg.debug.tcr" (printModule defaultEHCOpts prog) 200

  -- write binary core file, which can be compiled by uhc
  putSerializeFile "CoreApiProg.bcr" prog


prog :: CModule
prog = mkModule (mkHsName [] "CoreApiProg")
                [] -- we are not exporting anything
                [mkImport $ mkHsName1 x | x <- ["CoreApiFFI", "UHC.Run"]]
                [] -- we don't define any datatypes
                body

  where body :: CExpr
        body = mkLet1Plain
                    crMainNm
                    ( mkApp crBind
                      [ mkApp crPutStrLn [mkString defaultEHCOpts "Hello World!"]
                      , mkApp crReturn [mkUnit defaultEHCOpts]
                      ]
                    )
                    (mkMain crMainNm)
        -- only used internally in this module, so we can use the
        -- ..Unique.. function to avoid name clashes.
        crMainNm = mkUniqueHsName "nl.uu.uhc.core-api-example" ["CoreApiProg"] "toRun"
        -- entities defined by a Haskell file, we have to use the normal mkHsName(1) functions.
        crBind = mkVar $ mkHsName1 "CoreApiFFI.primBind"
        crPutStrLn = mkVar $ mkHsName1 "CoreApiFFI.primPutStrLn"
        crReturn = mkVar $ mkHsName1 "CoreApiFFI.primReturn"

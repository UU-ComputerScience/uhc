module EH101.EHC.Common
( module Data.Maybe, module Data.List, module Data.Char
, module System.IO, module Control.Monad.State
, module EH.Util.CompileRun, module EH.Util.Pretty, module EH.Util.FPath, module EH.Util.Utils
, module EH101.Base.Common, module EH101.Base.Builtin, module EH101.Opts
, module EH101.Error, module EH101.Error.Pretty
, module EH101.Gam.Full
, HSState (..)
, EHState (..)
, EHCompileUnitState (..)
, EHCompileUnitKind (..)
, ecuStateToKind
, FinalCompileHow (..)
, mkShellCmd
, mkInOrOutputFPathDirFor
, mkInOrOutputFPathFor
, mkOutputFPath
, mkPerModuleOutputFPath
, mkPerExecOutputFPath
, module System.Time, module System.Directory
, hsstateIsLiteral
, hsstateShowLit
, hsstateNext
, CState (..) )
where
import Data.List
import Data.Char
import Data.Maybe
import Control.Monad.State
import System.IO
import EH.Util.CompileRun
import EH.Util.Pretty
import EH.Util.FPath
import EH.Util.Utils
import EH101.Base.Common
import EH101.Base.Builtin
import EH101.Opts
import EH101.Error
import EH101.Error.Pretty
import EH101.Gam.Full
import System.Time
import System.Directory




{-# LINE 29 "src/ehc/EHC/Common.chs" #-}
-- dummy, so module is not empty for initial variants, and exports will take effect

{-# LINE 39 "src/ehc/EHC/Common.chs" #-}
data HSState
  = HSStart                 -- starting from .hs
  | HSAllSem                -- done all semantics for .hs
  | HMOnlyMinimal           -- done minimal info only
  -- | HMStart                 -- starting from nothing, not using .hi info nor .hs file, just for linking etc
  | HSOnlyImports           -- done imports from .hs
  | HIStart                 -- starting from .hi
  | HIAllSem                -- done all semantics for .hi
  | HIOnlyImports           -- done imports from .hi
  | LHSStart                -- starting from .lhs
  | LHSOnlyImports          -- done imports from .lhs
  deriving (Show,Eq)

{-# LINE 60 "src/ehc/EHC/Common.chs" #-}
hsstateIsLiteral :: HSState -> Bool
hsstateIsLiteral LHSStart       = True
hsstateIsLiteral LHSOnlyImports = True
hsstateIsLiteral _              = False

{-# LINE 69 "src/ehc/EHC/Common.chs" #-}
hsstateShowLit :: HSState -> String
hsstateShowLit LHSStart       = "Literal"
hsstateShowLit LHSOnlyImports = "Literal"
hsstateShowLit _              = ""

{-# LINE 80 "src/ehc/EHC/Common.chs" #-}
hsstateNext :: HSState -> HSState
hsstateNext HSStart       = HSOnlyImports
hsstateNext HIStart       = HIOnlyImports
-- hsstateNext HMStart       = HMOnlyMinimal
hsstateNext LHSStart      = LHSOnlyImports
hsstateNext st            = st

{-# LINE 93 "src/ehc/EHC/Common.chs" #-}
data EHState
  = EHStart
  | EHAllSem
  deriving (Show,Eq)

{-# LINE 102 "src/ehc/EHC/Common.chs" #-}
data CState
  = CStart
  | CAllSem
  deriving (Show,Eq)

{-# LINE 111 "src/ehc/EHC/Common.chs" #-}
data EHCompileUnitState
  = ECUSUnknown
  | ECUSHaskell !HSState
  | ECUSEh      !EHState
  | ECUSC       !CState
  | ECUSGrin
  | ECUSFail
  deriving (Show,Eq)

{-# LINE 128 "src/ehc/EHC/Common.chs" #-}
data EHCompileUnitKind
  = EHCUKind_HS     -- Haskell: .hs .lhs .hi
  | EHCUKind_C      -- C: .c
  | EHCUKind_None   -- Nothing
  deriving Eq

{-# LINE 138 "src/ehc/EHC/Common.chs" #-}
ecuStateToKind :: EHCompileUnitState -> EHCompileUnitKind
ecuStateToKind s
  = case s of
      ECUSHaskell _ -> EHCUKind_HS
      ECUSC       _ -> EHCUKind_C
      _             -> EHCUKind_None

{-# LINE 153 "src/ehc/EHC/Common.chs" #-}
data FinalCompileHow
  = FinalCompile_Module
  | FinalCompile_Exec

{-# LINE 163 "src/ehc/EHC/Common.chs" #-}
mkShellCmd :: [String] -> String
mkShellCmd = concat . intersperse " "

{-# LINE 172 "src/ehc/EHC/Common.chs" #-}
mkInOrOutputFPathDirFor :: FPATH nm => InOrOutputFor -> EHCOpts -> nm -> FPath -> String -> (FPath,Maybe String)
mkInOrOutputFPathDirFor inoutputfor opts modNm fp suffix
  = (fpathSetSuff suffix fp', d)
  where (fp',d) = case inoutputfor of
                    OutputFor_Module   -> f ehcOptOutputDir
                    OutputFor_Pkg      -> f ehcOptOutputDir -- ehcOptOutputPkgLibDir
                    InputFrom_Loc l
                      | filelocIsPkg l -> f (const Nothing)
                      | otherwise      -> f ehcOptOutputDir
        f g     = case g opts of
                    Just d -> (fpathPrependDir d' $ mkFPath modNm, Just d')
                           where d' = filePathUnPrefix d
                    _      -> (fp,Nothing)

{-# LINE 193 "src/ehc/EHC/Common.chs" #-}
mkInOrOutputFPathFor :: FPATH nm => InOrOutputFor -> EHCOpts -> nm -> FPath -> String -> FPath
mkInOrOutputFPathFor inoutputfor opts modNm fp suffix
  = fst $ mkInOrOutputFPathDirFor inoutputfor opts modNm fp suffix

{-# LINE 199 "src/ehc/EHC/Common.chs" #-}
mkOutputFPath :: FPATH nm => EHCOpts -> nm -> FPath -> String -> FPath
mkOutputFPath = mkInOrOutputFPathFor OutputFor_Module

{-# LINE 208 "src/ehc/EHC/Common.chs" #-}
-- | FPath for per module output
mkPerModuleOutputFPath :: EHCOpts -> Bool -> HsName -> FPath -> String -> FPath
mkPerModuleOutputFPath opts doSepBy_ modNm fp suffix
  = fpO modNm fp
  where fpO m f= case ehcOptPkg opts of
                   Just _        -> nm_
                   _ | doSepBy_  -> nm_
                     | otherwise -> mkOutputFPath opts m f suffix
               where nm_ = mkOutputFPath opts (hsnMapQualified (const base) m) (fpathSetBase base f) suffix
                         where base = hsnShow True "_" "_" m

{-# LINE 225 "src/ehc/EHC/Common.chs" #-}
-- | FPath for final executable
mkPerExecOutputFPath :: EHCOpts -> HsName -> FPath -> Maybe String -> FPath
mkPerExecOutputFPath opts modNm fp mbSuffix
  = fpExec
  where fpExecBasedOnSrc = maybe (mkOutputFPath opts modNm fp "") (\s -> mkOutputFPath opts modNm fp s) mbSuffix
        fpExec = maybe fpExecBasedOnSrc id (ehcOptMbOutputFile opts)


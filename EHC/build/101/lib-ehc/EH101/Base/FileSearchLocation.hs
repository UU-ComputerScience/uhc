module EH101.Base.FileSearchLocation
( mkDirFileLoc
, StringPath, FileLocPath
, FileLocKind (..)
, FileLoc (..), emptyFileLoc, fileLocPkgDb
, mkPkgFileLoc
, filelocIsPkg
, FileSearchLoc
, PkgKey, PkgKey1, PkgKey2
, showPkgKey
, PackageSearchFilter (..)
, pkgSearchFilter
, PackageCfgKeyVals, PackageInfo (..), PackageMp, Module2PackageMp, PackageDatabase (..), emptyPackageMp, emptyPackageDatabase
, mkInternalPkgFileBase )
where
import EH101.Base.Common
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import Data.Version
import Data.List
import UU.Parsing
import EH.Util.ParseUtils
import EH.Util.ScanUtils
import EH101.Base.HsName
import EH101.Base.Target
import qualified EH101.ConfigInstall as Cfg




{-# LINE 38 "src/ehc/Base/FileSearchLocation.chs" #-}
data FileLocKind
  = FileLocKind_Dir									-- plain directory
  | FileLocKind_Pkg	PkgKey							-- specific package
  					String							-- with the dir inside package it was found
  | FileLocKind_PkgDb								-- yet unknown package in the package database
  deriving Eq

instance Show FileLocKind where
  show  FileLocKind_Dir		    = "directory"
  show (FileLocKind_Pkg p d)	= "package: " ++ showPkgKey p ++ "(in: " ++ d ++ ")"
  show  FileLocKind_PkgDb    	= "package database"

{-# LINE 66 "src/ehc/Base/FileSearchLocation.chs" #-}
data FileLoc
  = FileLoc
      {	filelocKind		:: FileLocKind
      , filelocDir		:: String
      }
  deriving Eq

instance Show FileLoc where
  show (FileLoc k d) = d ++ " (" ++ show k ++ ")"

emptyFileLoc :: FileLoc
emptyFileLoc = FileLoc FileLocKind_Dir ""

fileLocPkgDb :: FileLoc
fileLocPkgDb = FileLoc FileLocKind_PkgDb ""

{-# LINE 84 "src/ehc/Base/FileSearchLocation.chs" #-}
mkDirFileLoc
  = FileLoc FileLocKind_Dir

{-# LINE 93 "src/ehc/Base/FileSearchLocation.chs" #-}
mkPkgFileLoc :: PkgKey -> String -> FileLoc
mkPkgFileLoc p d = FileLoc (FileLocKind_Pkg p d) d

{-# LINE 98 "src/ehc/Base/FileSearchLocation.chs" #-}
filelocIsPkg :: FileLoc -> Bool
filelocIsPkg (FileLoc (FileLocKind_Pkg _ _) _) = True
filelocIsPkg (FileLoc  FileLocKind_PkgDb    _) = True
filelocIsPkg _                                 = False

{-# LINE 105 "src/ehc/Base/FileSearchLocation.chs" #-}
type StringPath  = [String]
type FileLocPath = [FileLoc]

{-# LINE 114 "src/ehc/Base/FileSearchLocation.chs" #-}
type FileSearchLoc = FileLoc

{-# LINE 122 "src/ehc/Base/FileSearchLocation.chs" #-}
type PkgKey1 = PkgName
type PkgKey2 = Maybe Version
type PkgKey  = (PkgKey1,PkgKey2)

instance HSNM PkgKey where
  mkHNm (n,Just v) =   mkHNmBase (n ++ "-" ++ (concat $ intersperse "." $ map show $ versionBranch v))
  mkHNm (n,_     ) =   mkHNm      n

{-# LINE 136 "src/ehc/Base/FileSearchLocation.chs" #-}
showPkgKey :: PkgKey -> String
showPkgKey = show . mkHNm

{-# LINE 145 "src/ehc/Base/FileSearchLocation.chs" #-}
data PackageSearchFilter
  = PackageSearchFilter_HideAll
  | PackageSearchFilter_HidePkg			[PkgKey]
  | PackageSearchFilter_ExposePkg		[PkgKey]
  deriving Show

{-# LINE 153 "src/ehc/Base/FileSearchLocation.chs" #-}
pkgSearchFilter :: (x -> Maybe PkgKey) -> ([PkgKey] -> PackageSearchFilter) -> [x] -> [PackageSearchFilter]
pkgSearchFilter mkKey mk ss
  = if null ps then [] else [mk ps]
  where ps = catMaybes $ map mkKey ss

{-# LINE 164 "src/ehc/Base/FileSearchLocation.chs" #-}
type PackageCfgKeyVals = Map.Map String String

data PackageInfo
  = PackageInfo
      { pkginfoLoc					:: !FileLoc						-- directory location
      , pkginfoOrder				:: !Int							-- for multiple packages the relative order
      -- , pkginfoKeyVals				:: PackageCfgKeyVals			-- key/value pairs of pkg config info
      , pkginfoExposedModules		:: !HsNameS						-- exposed modules
      , pkginfoIsExposed		    :: !Bool						-- pkg is exposed?
      }
      deriving Show

-- content of a package (keys are name, then version)
type PackageMp = Map.Map PkgKey1 (Map.Map PkgKey2 [PackageInfo])

emptyPackageMp :: PackageMp
emptyPackageMp = Map.empty

-- reverse map from module name to package key
type Module2PackageMp = Map.Map HsName [PkgKey]

-- A package database contains an actual package map, plus a function
-- that maps modules to associated package maps. The latter is computed
-- by "freezing" the package database using "pkgDbFreeze".
data PackageDatabase
  = PackageDatabase
      { pkgDbPkgMp		:: PackageMp
      , pkgDbMod2PkgMp	:: Module2PackageMp
      }
      deriving Show

emptyPackageDatabase :: PackageDatabase
emptyPackageDatabase = PackageDatabase emptyPackageMp Map.empty

{-# LINE 204 "src/ehc/Base/FileSearchLocation.chs" #-}

mkInternalPkgFileBase :: PkgKey -> String {- compiler name/version -} -> Target -> TargetFlavor -> FilePath
mkInternalPkgFileBase pkgKey compversion tgt tgtv =
  Cfg.mkInternalPkgFileBase (showPkgKey pkgKey) compversion (show tgt) (show tgtv)


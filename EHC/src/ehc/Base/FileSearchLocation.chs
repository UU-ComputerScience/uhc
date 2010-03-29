%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% UHC search locations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Encoding of searchable locations for files, in particular HS and derived files.
In principle such files reside in directories or packages.
%%]

%%[8 module {%{EH}Base.FileSearchLocation}
%%]

%%[8 import({%{EH}Base.Common})
%%]

-- parsing
%%[99 import(UU.Parsing, EH.Util.ParseUtils)
%%]
-- scanning
%%[99 import(EH.Util.ScanUtils, {%{EH}Scanner.Common}, {%{EH}Base.HsName}, {%{EH}Base.ParseUtils})
%%]


-- general imports 
%%[8 import(qualified Data.Set as Set, qualified Data.Map as Map, Data.Maybe, Data.Version, Data.List)
%%]

%%[99 import({%{EH}Base.Target}, qualified {%{EH}ConfigInstall} as Cfg)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Kind of location
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

FileLocKind indicates where something can be found. After found, a FileLocKind_PkgDb will be replaced by a FileLocKind_Pkg.

%%[99 export(FileLocKind(..))
data FileLocKind
  = FileLocKind_Dir									-- plain directory
  | FileLocKind_Pkg	PkgKey							-- specific package
  | FileLocKind_PkgDb								-- yet unknown package in the package database

instance Show FileLocKind where
  show  FileLocKind_Dir		= "directory"
  show (FileLocKind_Pkg p)	= "package: " ++ showPkgKey p
  show  FileLocKind_PkgDb	= "package database"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File location, used for search locations as well
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8.FileLoc export(FileLoc,filelocDir,emptyFileLoc)
type FileLoc = String

emptyFileLoc :: FileLoc
emptyFileLoc = ""

filelocDir :: FileLoc -> String
filelocDir = id
%%]

%%[99 -8.FileLoc export(FileLoc(..),emptyFileLoc,fileLocPkgDb)
data FileLoc
  = FileLoc
      {	filelocKind		:: FileLocKind
      , filelocDir		:: String
      }

instance Show FileLoc where
  show (FileLoc k d) = d ++ " (" ++ show k ++ ")"

emptyFileLoc :: FileLoc
emptyFileLoc = FileLoc FileLocKind_Dir ""

fileLocPkgDb :: FileLoc
fileLocPkgDb = FileLoc FileLocKind_PkgDb ""
%%]

%%[8 export(mkDirFileLoc)
mkDirFileLoc
%%[[8
  = id
%%][99
  = FileLoc FileLocKind_Dir
%%]]
%%]

%%[99 export(mkPkgFileLoc)
mkPkgFileLoc :: PkgKey -> String -> FileLoc
mkPkgFileLoc p = FileLoc (FileLocKind_Pkg p)
%%]

%%[99 export(filelocIsPkg)
filelocIsPkg :: FileLoc -> Bool
filelocIsPkg (FileLoc (FileLocKind_Pkg _) _) = True
filelocIsPkg (FileLoc  FileLocKind_PkgDb  _) = True
filelocIsPkg _                               = False
%%]

%%[8 export(StringPath,FileLocPath)
type StringPath  = [String]
type FileLocPath = [FileLoc]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File search location
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(FileSearchLoc)
type FileSearchLoc = FileLoc
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Package key
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(PkgKey,PkgKey1,PkgKey2)
type PkgKey1 = PkgName
type PkgKey2 = Maybe Version
type PkgKey  = (PkgKey1,PkgKey2)

instance HSNM PkgKey where
  mkHNm (n,Just v) =   mkHNmBase (n ++ "-" ++ (concat $ intersperse "." $ map show $ versionBranch v))
  mkHNm (n,_     ) =   mkHNm      n
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parsing/showing the package name as it is used
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
pPkgKey :: P PkgKey
pPkgKey = (pVarid <|> pConid) <+> pMb (pMINUS *> pVersion)

pVersion :: P Version
pVersion = (\v -> Version (map read v) []) <$> pList1Sep pDOT pInteger10
%%]

%%[99 export(parsePkgKey)
parsePkgKey :: String -> Maybe PkgKey
parsePkgKey
  = parseString scanOpts pPkgKey
  where scanOpts   = defaultScanOpts {scoSpecChars = Set.fromList ".-", scoAllowFloat = False}
%%]

%%[99 export(showPkgKey)
showPkgKey :: PkgKey -> String
showPkgKey = show . mkHNm
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Package search filtering
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(PackageSearchFilter(..))
data PackageSearchFilter
  = PackageSearchFilter_HideAll
  | PackageSearchFilter_HidePkg			[PkgKey]
  | PackageSearchFilter_ExposePkg		[PkgKey]
  deriving Show
%%]

%%[99 export(pkgSearchFilter)
pkgSearchFilter :: ([PkgKey] -> PackageSearchFilter) -> [String] -> [PackageSearchFilter]
pkgSearchFilter mk ss
  = if null ps then [] else [mk ps]
  where ps = catMaybes $ map parsePkgKey ss
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Package database
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(PackageCfgKeyVals,PackageInfo(..),PackageMp,Module2PackageMp,PackageDatabase(..),emptyPackageMp,emptyPackageDatabase)
type PackageCfgKeyVals = Map.Map String String

data PackageInfo
  = PackageInfo
      { pkginfoLoc					:: !FileLoc						-- directory location
      , pkginfoOrder				:: !Int							-- for multiple packages the relative order
      -- , pkginfoKeyVals				:: PackageCfgKeyVals			-- key/value pairs of pkg config info
      , pkginfoExposedModules		:: !(Set.Set HsName)			-- exposed modules
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
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constructing paths for specific files in package databases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(mkInternalPkgFileBase)

mkInternalPkgFileBase :: PkgKey -> String {- compiler name/version -} -> Target -> TargetFlavor -> FilePath
mkInternalPkgFileBase pkgKey compversion tgt tgtv =
  Cfg.mkInternalPkgFileBase (showPkgKey pkgKey) compversion (show tgt) (show tgtv)

%%]

module EH101.Base.PackageDatabase
( pkgDbLookup
, pkgDbFromDirs
, pkgDbSelectBySearchFilter
, PkgModulePartition
, pkgExposedPackages
, pkgDbFreeze
, fileLocSearch )
where
import EH101.Base.Common
import EH101.Opts
import EH101.Error
import qualified EH101.Config as Cfg
import UU.Parsing
import EH.Util.ParseUtils
import EH101.Base.Parser2
import EH.Util.ScanUtils
import EH101.Scanner.Common
import EH101.Base.HsName
import EH101.Base.ParseUtils
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Version
import Data.List
import Data.Maybe
import Data.Char
import System.Environment
import System.Directory
import Control.Monad
import System.IO
import System.Exit
import System.Environment
import EH.Util.FPath
import EH.Util.Utils
import EH.Util.Debug

{-# LINE 50 "src/ehc/Base/PackageDatabase.chs" #-}
-- | union 2 PackageMp, choosing the first one in the ordering on the fly
pkgMpUnion :: PackageMp -> PackageMp -> PackageMp
pkgMpUnion
  = Map.unionWith (Map.unionWith cmb)
  where cmb p1s p2s = head $ groupBy eqp $ sortBy cmpp $ p1s ++ p2s
        -- cmb = (++)
        eqp  p1  p2  = pkginfoOrder p1 == pkginfoOrder p2
        cmpp p1  p2  = pkginfoOrder p1 `compare` pkginfoOrder p2

-- looking up just picks the first one
-- TBD: fix this: error/warning, etc
pkgMpLookup :: PkgKey -> PackageMp -> Maybe PackageInfo
pkgMpLookup (k1,k2) m1
  = fmap head $ mapLookup2 k1 k2 m1

pkgMpDifference :: PackageMp -> PackageMp -> PackageMp
pkgMpDifference mp mpDiff
  = foldr diff mp $ Map.toList mpDiff
  where diff (k1,mp2Diff) mp
          = case Map.lookup k1 mp of
              Just mp2 | Map.null m -> Map.delete k1 mp
                       | otherwise  -> Map.insert k1 m mp
                       where m = Map.difference mp2 mp2Diff
              _ -> mp

pkgMpUnions :: [PackageMp] -> PackageMp
pkgMpUnions = foldr pkgMpUnion Map.empty

pkgMpSingletonL :: PkgKey -> [PackageInfo] -> PackageMp
pkgMpSingletonL (k1,k2) i = Map.singleton k1 (Map.singleton k2 i)

pkgMpSingleton :: PkgKey -> PackageInfo -> PackageMp
pkgMpSingleton k i = pkgMpSingletonL k [i]

{-# LINE 86 "src/ehc/Base/PackageDatabase.chs" #-}
mod2pkgMpUnion :: Module2PackageMp -> Module2PackageMp -> Module2PackageMp
mod2pkgMpUnion = Map.unionWith (++)

mod2pkgMpUnions :: [Module2PackageMp] -> Module2PackageMp
mod2pkgMpUnions = foldr mod2pkgMpUnion Map.empty

{-# LINE 98 "src/ehc/Base/PackageDatabase.chs" #-}
pkgDbLookup:: PkgKey -> PackageDatabase -> Maybe PackageInfo
pkgDbLookup key db
  = pkgMpLookup key $ pkgDbPkgMp db

pkgDbSelectMpOnKey :: PkgKey -> PackageDatabase -> PackageMp
pkgDbSelectMpOnKey key@(pkgNm,mbVersion) db
  = case mbVersion of
      Just pkgVersion -> case lkup of
                           Just m -> maybe emptyPackageMp (\i -> pkgMpSingletonL key i) $ Map.lookup mbVersion m
                           _      -> emptyPackageMp
      _               -> maybe emptyPackageMp (Map.singleton pkgNm) lkup
  where lkup = Map.lookup pkgNm $ pkgDbPkgMp db

{-# LINE 120 "src/ehc/Base/PackageDatabase.chs" #-}
-- parse content of a package config file, yielding updates to PackageInfo, ignoring unused fields
pkgCfgParse :: String -> [PackageInfo -> PackageInfo]
pkgCfgParse s
  = map (\(k,v) -> case map toLower k of
                     "exposed-modules" -> add (\ns i -> i {pkginfoExposedModules = Set.fromList ns}) parseModuleNames v
                     "exposed"         -> add (\ex i -> i {pkginfoIsExposed      = ex             }) parseBool        v
                     _                 -> id
        ) $ Map.toList kvs
  where add upd parse v = maybe id upd $ parse v
        (kvs,_) = foldr p (Map.empty,"") $ lines s
        p s (kvs,saccum)
          = case elemIndex ':' s of
              Just colpos -> (Map.insert k (appendaccum v) kvs, "")
                          where (k,_:v) = splitAt colpos s
              _ -> (kvs,appendaccum s)
          where appendaccum s | null saccum = s
                              | otherwise   = s ++ " " ++ saccum

{-# LINE 140 "src/ehc/Base/PackageDatabase.chs" #-}
-- | For rhs of field 'exposed-modules'
pModuleNames :: P [HsName]
pModuleNames = pList (tokMkQName <$> (pQConidTk <|> pConidTk))

parseModuleNames :: String -> Maybe [HsName]
parseModuleNames = parseString (hsScanOpts defaultEHCOpts) pModuleNames

{-# LINE 149 "src/ehc/Base/PackageDatabase.chs" #-}
-- | For rhs of field 'exposed'
pBool :: P Bool
pBool = True <$ pKeyTk "True" <|> False <$ pKeyTk "False"

parseBool :: String -> Maybe Bool
parseBool = parseString (defaultScanOpts {scoKeywordsTxt = Set.fromList ["True","False"]}) pBool

{-# LINE 162 "src/ehc/Base/PackageDatabase.chs" #-}
-- read content of package dir + config file
pkgMpFromDirFile :: EHCOpts -> PkgKey -> Int -> FPath -> IO PackageMp
pkgMpFromDirFile opts pkgkey order pkgfp
  = do -- print pkgfp
       -- print mbKey
       isDir <- doesDirectoryExist pkgdir
       if isDir then
              do { let fpCfg = fpathToStr $ fpathSetDir pkgdir $ fpathFromStr Cfg.ehcPkgConfigfileName
                       pkgInfo = PackageInfo (mkPkgFileLoc pkgkey pkgdir) order Set.empty True
                 ; cfgExists <- doesFileExist fpCfg
                 ; pm <- if cfgExists
                   then do h <- openFile fpCfg ReadMode
                           cfg <- hGetContents h
                           let i = foldr ($) pkgInfo (pkgCfgParse cfg)
                           i `seq` hClose h
                           return $ pkgMpSingleton pkgkey i
                   else return $ pkgMpSingleton pkgkey pkgInfo
                 -- ; print pm
                 ; return pm
                 }
         else return Map.empty
  where pkgdir = fpathToStr pkgfp

-- read content of a dir containing package dirs
pkgMpFromDir :: EHCOpts -> Int -> FilePath -> IO PackageMp
pkgMpFromDir opts order fp
  = do { isDir <- doesDirectoryExist fp
       -- ; putStrLn ("dir-exists: " ++ show (fp,isDir))
       ; if isDir
         then do { pkgWithVersions <- getDirectoryContents fp
                 ; let pkgWithVersionsBases = catMaybes $
                                              map (\f -> do { k <- parsePkgKey f
                                                            ; return $ (k,fpathPrependDir fp $ fpathFromStr $
                                                                            mkInternalPkgFileBase k (Cfg.installVariant opts)
                                                                            (ehcOptTarget opts) (ehcOptTargetFlavor opts))
                                                            })
                                                  pkgWithVersions
                 -- putStrLn (show fp)
                 -- putStrLn (show pkgWithVersions)
                 -- putStrLn (show pkgWithVersionsBases)
                 ; mps <- mapM (\ (k,f) -> pkgMpFromDirFile opts k order f) pkgWithVersionsBases
                 -- ; putStrLn (show mps)
                 ; let mpsu = pkgMpUnions mps
                 ; return mpsu
                 }
         else return Map.empty
       }

-- read and combine contents of multiple package database directories
pkgDbFromDirs :: EHCOpts -> [FilePath] -> IO PackageDatabase
pkgDbFromDirs opts fps
  = do mps <- zipWithM (pkgMpFromDir opts) [1..] fps
       let mpsu = pkgMpUnions mps
       -- putStrLn ("unions: " ++ show mpsu)
       return (emptyPackageDatabase {pkgDbPkgMp = mpsu})

{-# LINE 224 "src/ehc/Base/PackageDatabase.chs" #-}
-- select from full package db, building a db according to the search filter
pkgDbSelectBySearchFilter :: [PackageSearchFilter] -> PackageDatabase -> (PackageDatabase, [Err])
pkgDbSelectBySearchFilter searchFilters fullDb
  = (emptyPackageDatabase {pkgDbPkgMp = mp}, err)
  where (mp,err) = foldl sel (emptyPackageMp, []) searchFilters
        sel (_,e)  PackageSearchFilter_HideAll         = (emptyPackageMp,e)
        sel mpe   (PackageSearchFilter_HidePkg   keys) = foldr (one (flip pkgMpDifference)) mpe keys
        sel mpe   (PackageSearchFilter_ExposePkg keys) = foldr (one pkgMpUnion) mpe keys
        one cmb k (mp,e)
          | Map.null s = (mp,[mkErr_NamesNotIntrod emptyRange "package" [mkHNm k]] ++ e)
          | otherwise  = (cmb s mp,e)
          where s = pkgDbSelectMpOnKey k fullDb

{-# LINE 243 "src/ehc/Base/PackageDatabase.chs" #-}
type PkgModulePartition = (PkgKey,String,[HsName])

{-# LINE 253 "src/ehc/Base/PackageDatabase.chs" #-}
pkgExposedPackages :: PackageDatabase -> [PkgModulePartition]
pkgExposedPackages db
  = [ ((k1,k2),"",[])
    | (k1,mp1) <- Map.toList $ pkgDbPkgMp db
    , (k2,is ) <- Map.toList mp1
    , i        <- is						-- TBD: disambiguation
    , pkginfoIsExposed i
    ]

{-# LINE 268 "src/ehc/Base/PackageDatabase.chs" #-}
pkgDbFreeze :: PackageDatabase -> PackageDatabase
pkgDbFreeze db
  = db {pkgDbMod2PkgMp = m2p}
  where m2p
          = mod2pkgMpUnions
              [ mod2pkgMpUnions
                  [ Map.singleton m [(k1,k2)]
                  | m <- Set.toList $ pkginfoExposedModules i
                  ]
              | (k1,mp1) <- Map.toList $ pkgDbPkgMp db
              , (k2,is)  <- Map.toList mp1
              , i        <- is
              ]

{-# LINE 288 "src/ehc/Base/PackageDatabase.chs" #-}
-- | For a module, find the package to use and the location of the root dir of that package.
pkgDbSearch :: PackageDatabase -> HsName -> Maybe (PkgKey,FilePath,[Err])
pkgDbSearch db modNm
  = do pkgs <- mbPkgs
       dirOf $ disambig pkgs
  where mbPkgs   = Map.lookup modNm $ pkgDbMod2PkgMp db
        disambig pks
                 = case sortBy cmp pks of
                     [k]               -> (k,[])          -- no ambiguity
                     (k@(_,Nothing):_) -> (k,[])          -- versionless overrides others
                     ks@(k:_)          -> (k,[rngLift emptyRange Err_AmbiguousNameRef "module" "package" modNm (map (mkHNm . strOf) ks)])
                 where cmp (_,Nothing) (_,Nothing) = EQ                 -- versionless goes first
                       cmp (_,Nothing) (_,_      ) = LT
                       cmp (_,k21    ) (_,k22    ) = compare k22 k21    -- then highest version
        pkgOf  k    = pkgMpLookup k $ pkgDbPkgMp db
        strOf  k    = showPkgKey k ++ ": " ++ maybe "" (\i -> show (pkginfoLoc i) ++ ":" ++ show (pkginfoOrder i)) (pkgOf k)
        dirOf (k,e) = fmap (\i -> (k,filelocDir $ pkginfoLoc i,e)) $ pkgOf k

{-# LINE 308 "src/ehc/Base/PackageDatabase.chs" #-}
-- look up a file location, defaults to plain file search except for a package db which is then queried
fileLocSearch :: EHCOpts -> FileLoc -> HsName -> FPath -> [(FileLoc,FPath,[Err])]
fileLocSearch opts loc modNm fp
  = case {- tr "fileLocSearch1" (show modNm ++ ": " ++ show fp ++ " " ++ show loc) $ -} filelocKind loc of
      FileLocKind_PkgDb
        -> maybe [] srch $ pkgDbSearch (ehcOptPkgDb opts) modNm
        where srch (k,d,e) = [ (mkPkgFileLoc k d',fp',e) | (d',fp') <- searchFPathFromLoc d fp ]
      _ -> [ (loc,fp',[]) | (_,fp') <- searchFPathFromLoc (filelocDir loc) fp ]


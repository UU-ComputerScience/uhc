%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Package database
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
In memory database of all available packages.
Its contents are based upon
\begin{itemzie}
\item a list of package locations,
\item each holding packages,
\item with multiple versions encoded in their names.
\end{itemize}

Based on cmdline options a filtered view/selection is offered for further searching of files in
packages.
%%]

%%[99 module {%{EH}Base.PackageDatabase}
%%]

%%[99 import({%{EH}Base.Common},{%{EH}Base.Opts},{%{EH}Error})
%%]
%%[99 import(qualified {%{EH}Config} as Cfg)
%%]

-- parsing
%%[99 import(UU.Parsing, EH.Util.ParseUtils)
%%]
-- scanning
%%[99 import(EH.Util.ScanUtils, {%{EH}Scanner.Common}, {%{EH}Base.HsName}, {%{EH}Base.ParseUtils})
%%]

-- general imports
%%[99 import(qualified Data.Map as Map, qualified Data.Set as Set, Data.Version, Data.List, Data.Maybe)
%%]
%%[99 import(System.Environment, System.Directory, Control.Monad)
%%]
%%[99 import(IO,EH.Util.FPath)
%%]

-- debug
%%[99 import(EH.Util.Debug)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Package database
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
pkgMpUnion :: PackageMp -> PackageMp -> PackageMp
pkgMpUnion = Map.unionWith (Map.unionWith (++))

-- looking up just picks the first one
-- TBD: fix this: error/warning, etc
pkgMpLookup :: PkgKey -> PackageMp -> Maybe PackageInfo
pkgMpLookup (k1,k2) m1
  = do m2 <- Map.lookup k1 m1
       fmap head $ Map.lookup k2 m2

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
%%]

%%[99
mod2pkgMpUnion :: Module2PackageMp -> Module2PackageMp -> Module2PackageMp
mod2pkgMpUnion = Map.unionWith (++)

mod2pkgMpUnions :: [Module2PackageMp] -> Module2PackageMp
mod2pkgMpUnions = foldr mod2pkgMpUnion Map.empty
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Querying a PackageDatabase
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(pkgDbSelectOnKey)
pkgDbSelectOnKey :: PkgKey -> PackageDatabase -> PackageMp
pkgDbSelectOnKey key@(pkgNm,mbVersion) db
  = case mbVersion of
      Just pkgVersion -> case lkup of
                           Just m -> maybe emptyPackageMp (\i -> pkgMpSingletonL key i) $ Map.lookup mbVersion m
                           _      -> emptyPackageMp
      _               -> maybe emptyPackageMp (Map.singleton pkgNm) lkup
  where lkup = Map.lookup pkgNm $ pkgDbPkgMp db
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parse pkg config file content
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Parsing is done somewhat simplistic, in 2 steps.
First key/value pairs are extracted, these are then parsed based on the keyword.

%%[99
-- parse content of a package config file, yielding updates to PackageInfo, ignoring unused fields
pkgCfgParse :: String -> [PackageInfo -> PackageInfo]
pkgCfgParse s
  = map (\(k,v) -> case k of
                     "exposed-modules"
                       -> maybe id (\ns i -> i {pkginfoExposedModules = Set.fromList ns}) $ parseModuleNames v
                     _ -> id
        ) $ Map.toList kvs
  where (kvs,_) = foldr p (Map.empty,"") $ lines s
        p s (kvs,saccum)
          = case elemIndex ':' s of
              Just colpos -> (Map.insert k (appendaccum v) kvs, "")
                          where (k,_:v) = splitAt colpos s
              _ -> (kvs,appendaccum s)
          where appendaccum s | null saccum = s
                              | otherwise   = s ++ " " ++ saccum
%%]

%%[99
-- For rhs of exposed-modules
pModuleNames :: P [HsName]
pModuleNames = pList (tokMkQName <$> (pQConidTk <|> pConidTk))

parseModuleNames :: String -> Maybe [HsName]
parseModuleNames = parseString hsScanOpts pModuleNames
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Filling the package database, given locations in which packages reside
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(pkgDbFromDirs)
-- read content of package dir + config file
pkgMpFromDirFile :: EHCOpts -> PkgKey -> Int -> FPath -> IO PackageMp
pkgMpFromDirFile opts pkgkey order pkgfp
  = do -- print pkgfp
       -- print mbKey
       isDir <- doesDirectoryExist pkgdir
       if isDir then
              do { let fpCfg = fpathToStr $ fpathSetDir pkgdir $ fpathFromStr Cfg.ehcPkgConfigfileName
                       pkgInfo = PackageInfo (mkPkgFileLoc pkgkey pkgdir) order Set.empty
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
                                                                            (ehcOptTarget opts) (ehcOptTargetVariant opts))
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
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construct a pkg db from the full db and filtering
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(pkgDbSelectBySearchFilter)
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
          where s = pkgDbSelectOnKey k fullDb
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Freeze database, subsequent changes must be refrozen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(pkgDbFreeze)
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
              , (k2,is) <- Map.toList mp1
              , i <- is
              ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Searching
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
-- For a module, find the package to use and the location of the root dir of that package.
pkgDbSearch :: PackageDatabase -> HsName -> Maybe (PkgKey,FilePath)
pkgDbSearch db modNm
  = do pkgs <- {- tr "pkgDbSearch0" (show $ (db, pkgDbMod2PkgMp db)) -} mbPkgs
       dirOf $ disambig $ {- tr "pkgDbSearch" (show modNm ++ show pkgs) $ -} pkgs
  where mbPkgs   = Map.lookup modNm $ pkgDbMod2PkgMp db
        disambig = head . sortBy cmp
                 where cmp (_,Nothing) (_,Nothing) = EQ					-- versionless goes first
                       cmp (_,Nothing) (_,_      ) = LT
                       cmp (_,k21    ) (_,k22    ) = compare k22 k21	-- then highest version
        dirOf k = fmap (\i -> (k,filelocDir $ pkginfoLoc i)) $ pkgMpLookup k $ pkgDbPkgMp db
%%]

%%[99 export(fileLocSearch)
-- look up a file location, defaults to plain file search except for a package db which is then queried
fileLocSearch :: EHCOpts -> FileLoc -> HsName -> FPath -> [(FileLoc,FPath)]
fileLocSearch opts loc modNm fp
  = case {- tr "fileLocSearch1" (show modNm ++ ": " ++ show fp ++ " " ++ show loc) $ -} filelocKind loc of
      FileLocKind_PkgDb
        -> maybe [] srch $ pkgDbSearch (ehcOptPkgDb opts) modNm
        where srch (k,d) = [ (mkPkgFileLoc k d',fp') | (d',fp') <- searchFPathFromLoc d fp ]
      _ -> [ (loc,fp') | (_,fp') <- searchFPathFromLoc (filelocDir loc) fp ]
%%]


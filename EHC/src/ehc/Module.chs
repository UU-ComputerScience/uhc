%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Based on "A Formal Specification of the Haskell 98 Module System",
%%%          Iavor S. Diatchki, Mark P. Jones, Thomas Hallgren, Haskell Workshop 2002
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module adm
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12 module {%{EH}Module} import(Data.Maybe,Data.List,qualified Data.Set as Set,qualified Data.Map as Map,EH.Util.Utils,UU.Pretty,EH.Util.PPUtils,qualified EH.Util.Rel as Rel,{%{EH}Base.Builtin},{%{EH}Base.Common},{%{EH}Error})
%%]

%%[12 export(ModEnt(..),ModExp(..),ModEntSpec(..),ModEntSubSpec(..),ModImp(..),Mod(..),ModEntRel,ModEntDomMp,ModEntRngMp)
%%]

%%[12 export(emptyMod)
%%]

%%[12 import ({%{EH}Gam}) export(modBuiltin,modImpBuiltin)
%%]

%%[12 import ({%{EH}Core}(HsName2OffsetMp),{%{EH}Ty}(rowLabCmp))
%%]

%%[12 export(ppModMp,ppModEntDomMp,ppModEntRel,ppModEntRel')
%%]

%%[99 export(modImpPrelude)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Imp/Exp entity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12
data ModEnt
  = ModEnt
      { mentKind    :: IdOccKind
      , mentIdOcc   :: IdOcc
      , mentOwns    :: Set.Set ModEnt
      }
  deriving (Show)

instance Eq ModEnt where
  e1 == e2 = mentKind e1 == mentKind e2 && mentIdOcc e1 == mentIdOcc e2

instance Ord ModEnt where
  e1 `compare` e2
    = case mentKind e1 `compare` mentKind e2 of
        EQ -> mentIdOcc e1 `compare` mentIdOcc e2
        c  -> c

type ModEntRel     = Rel.Rel HsName  ModEnt
type ModEntDomMp   = Map.Map HsName [ModEnt]
type ModEntRngMp   = Map.Map IdOcc  [HsName]

mentIsCon :: ModEnt -> Bool
mentIsCon e = mentKind e == IdOcc_Data || mentKind e == IdOcc_Class

%%]

%%[12
-- intended for parsing
ppModEnt :: (HsName -> PP_Doc) -> ModEnt -> PP_Doc
ppModEnt pn e
  = ppCurlysCommasBlock (l1 ++ l2)
  where l1 = [pp (mentKind e),ppIdOcc pn (mentIdOcc e)]
        l2 = if Set.null (mentOwns e) then [] else [ppCurlysCommasBlock (map (ppModEnt pn) $ Set.toList $ mentOwns e)]

instance PP ModEnt where
  pp = ppModEnt pp

-- intended for parsing
ppModEntRel' :: (HsName -> PP_Doc) -> ModEntRel -> PP_Doc
ppModEntRel' pn = ppCurlysAssocL pn (ppModEnt pn) . Rel.toList

ppModEntRel :: ModEntRel -> PP_Doc
ppModEntRel = ppModEntRel' pp

ppModEntDomMp :: ModEntDomMp -> PP_Doc
ppModEntDomMp = ppCurlysCommasBlock . map (\(a,b) -> pp a >|< "<>" >|< ppBracketsCommas b) . Map.toList
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Imp/Exp's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12
data ModExp
  = ModExpEnt ModEntSpec
  | ModExpMod HsName
  deriving (Show)

data ModEntSpec
  = ModEntSpec HsName (Maybe ModEntSubSpec)
  deriving (Show)

data ModEntSubSpec
  = ModEntSubAll
  | ModEntSubs [HsName]
  deriving (Show)

data ModImp
  = ModImp
      { mimpQualified   :: Bool
      , mimpSource      :: HsName
      , mimpAs          :: HsName
      , mimpHiding      :: Bool
      , mimpImpL        :: [ModEntSpec]
      }
  deriving (Show)

emptyModImp :: ModImp
emptyModImp = ModImp False hsnUnknown hsnUnknown True []

modImpBuiltin :: ModImp
modImpBuiltin
  = emptyModImp
      { mimpSource      = hsnModBuiltin
      , mimpAs          = hsnModBuiltin
      }
%%]

%%[99
modImpPrelude :: ModImp
modImpPrelude
  = emptyModImp
      { mimpSource      = hsnModPrelude
      , mimpAs          = hsnModPrelude
      }
%%]

%%[12
instance PP ModExp where
  pp (ModExpEnt s) = pp s
  pp (ModExpMod m) = "module" >#< m

instance PP ModEntSpec where
  pp (ModEntSpec n s) = n >|< maybe empty pp s

instance PP ModEntSubSpec where
  pp ModEntSubAll = pp "(..)"
  pp (ModEntSubs ns) = ppParensCommas ns

instance PP ModImp where
  pp i = mimpSource i >|< ppParensCommas (mimpImpL i)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12
data Mod
  = Mod
      { modName         :: HsName
      , modNameInSrc    :: Maybe HsName
      , modExpL         :: Maybe [ModExp]
      , modImpL         :: [ModImp]
      , modDefs         :: ModEntRel
      , modInstNmL      :: [HsName]
      }
  deriving (Show)

emptyMod = Mod hsnUnknown Nothing Nothing [] Rel.empty []

modBuiltin
  = emptyMod
      { modName         = hsnModBuiltin
      , modDefs         = defs
      }
  where defs
          = Rel.fromList [ (n,ModEnt IdOcc_Type (IdOcc n IdOcc_Type) Set.empty) | (n,_) <- gamToAssocL initTyGam ]
            `Rel.union` Rel.fromList [ (n,ModEnt IdOcc_Kind (IdOcc n IdOcc_Kind) Set.empty) | (n,_) <- gamToAssocL initKiGam ]
%%]

%%[12
instance PP Mod where
  pp m = modName m >|< "/" >|< modNameInSrc m
         >-< indent 2 ("IMP" >#< ppParensCommas (modImpL m) >-< "EXP" >#< maybe empty ppParensCommas (modExpL m) >-< "DEF" >#< ppModEntRel (modDefs m))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 5.1 Importing or exporting an entity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1212
modEntSpec :: Bool -> ModEntRel -> ModEntSpec -> ModEntRel
modEntSpec isHiding rel (ModEntSpec x subspec)
  = Rel.unions [mSpec,mSub]
  where mSpec       = Rel.restrictRng consider (Rel.restrictDom (==x) rel)
        allSubs     = mentOwns `unionMapSet` Rel.rng mSpec
        subs        = Rel.restrictRng (`Set.member` allSubs) rel
        mSub        = case subspec of
                        Nothing              -> Rel.empty
                        Just ModEntSubAll    -> subs
                        Just (ModEntSubs xs) -> Rel.restrictDom ((`elem` xs) . hsnQualified) subs
        consider
          | isHiding && isNothing subspec = const True
          | otherwise                     = not . mentIsCon
%%]

%%[12
modEntSpec :: Bool -> ModEntRel -> ModEntSpec -> ModEntRel
modEntSpec isHiding rel (ModEntSpec x subspec)
  | isHiding && isNothing subspec
              = mSpec
  | otherwise = Rel.unions [mSpec,mSub mSpec]
  where mSpec       = Rel.restrictDom (==x) rel
        mSub spec   = case subspec of
                        Nothing              -> Rel.empty
                        Just ModEntSubAll    -> subs
                        Just (ModEntSubs xs) -> Rel.restrictDom ((`elem` xs) . hsnQualified) subs
                    where allSubs     = mentOwns `unionMapSet` Rel.rng spec
                          subs        = Rel.restrictRng (`Set.member` allSubs) rel
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 5.2 Export relations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12
modExports :: Mod -> ModEntRel -> ModEntRel
modExports mod inscp
  = case modExpL mod of
      Nothing -> modDefs mod
      Just es -> hsnQualified `Rel.mapDom` Rel.unions exps
              where exps = modExpListEntry inscp `map` es
%%]

%%[12
modExpListEntry :: ModEntRel -> ModExp -> ModEntRel
modExpListEntry inscp (ModExpEnt it)
  = modEntSpec False inscp it
modExpListEntry inscp (ModExpMod m)
  = (hsnSetQual m `Rel.mapDom` unqs) `Rel.intersection` qs
  where (qs,unqs) = Rel.partitionDom hsnIsQual inscp
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 5.3 In-scope relations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12
modInscope :: Mod -> (HsName -> ModEntRel) -> ModEntRel
modInscope m expsOf
  = Rel.unions [imports,locals]
  where defEnts = modDefs m
        locals  = Rel.unions
                    [ hsnQualified `Rel.mapDom` defEnts
                    , hsnSetQual (modName m) `Rel.mapDom` defEnts 
                    ]
        imports = Rel.unions $ map (modImp expsOf) (modImpL m)
%%]

%%[12
modImp :: (HsName -> ModEntRel) -> ModImp -> ModEntRel
modImp expsOf imp
  | mimpQualified imp = qs
  | otherwise         = Rel.unions [unqs,qs]
  where qs       = hsnSetQual (mimpAs imp) `Rel.mapDom` incoming
        unqs     = hsnQualified `Rel.mapDom` incoming
        listed   = Rel.unions $ map (modEntSpec isHiding exps) (mimpImpL imp)
        isHiding = mimpHiding imp
        exps     = expsOf (mimpSource imp)
        incoming
          | isHiding  = exps `Rel.difference` listed
          | otherwise = listed
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 5.4 Recursive modules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12
modInsOuts :: (HsName -> ModEntRel) -> [Mod] -> [(ModEntRel,ModEntRel)]
modInsOuts otherExps mods
  = inscps `zip` exps
  where inscps       = computeIs exps
        exps         = lfpAfter nextExps $ replicate (length mods) Rel.empty
        nextExps     = computeEs . computeIs
        computeEs is = zipWith modExports mods is
        computeIs es = map (`modInscope` toFun es) mods
        toFun es m   = maybe (otherExps m) (es !!) (lookup m modIxs)
        modIxs       = map modName mods `zip` [0..]
%%]

%%[12
lfpAfter :: Eq x => (x -> x) -> x -> x
lfpAfter f x
  = if fx == x then fx else lfpAfter f fx
  where fx = f x
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 6 Error detection
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12
checkMod :: (HsName -> Maybe ModEntRel) -> ModEntRel -> Mod -> [Err]
checkMod expsOf inscp mod
  = checkAmbigExps modExports
    ++ if null missingModules
       then checkExpSpec inscp mod
            ++ [ err | (imp,Just exps) <- impSources, err <- checkImp exps imp ]
       else [mkErr_NamesNotIntrod "module" missingModules]
  where Just modExports = expsOf (modName mod)
        impSources      = [ (imp,expsOf (mimpSource imp)) | imp <- modImpL mod ]
        missingModules  = nub [ mimpSource imp | (imp,Nothing) <- impSources ]
%%]

%%[12
checkAmbigExps :: ModEntRel -> [Err]
checkAmbigExps exps
  = concatMap isAmbig (Set.toList (Rel.dom exps))
  where isAmbig n = ambig n cons ++ ambig n other
                  where (cons,other) = partition mentIsCon (Rel.apply exps n)
        ambig n ents@(_:_:_) | not (null a)
                             = [Err_AmbiguousExport n (map (pp . ioccNm . mentIdOcc) $ concat $ a)]
                             where a = [ l | l@(_:_:_) <- groupSortOn (ioccKind . mentIdOcc) ents ]
        ambig n _            = []
%%]

%%[1212
checkEntSpec :: Bool -> (HsName -> Err) -> (HsName -> HsName -> Err) -> ModEntSpec -> ModEntRel -> [Err]
checkEntSpec isHiding errUndef errUndefSub (ModEntSpec x subspec) rel
  = case xents of
      []   -> [errUndef x]
      ents -> concatMap chk ents
  where xents   = filter consider (Rel.apply rel x)
        chk ent = case subspec of
                    Just (ModEntSubs subs)
                      -> map (errUndefSub x) (filter (not . (`Set.member` subsInScope)) subs)
                      where subsInScope
                              = Set.map hsnQualified
                                $ Rel.dom
                                $ Rel.restrictRng (`Set.member` mentOwns ent) rel
                    _ -> []
        consider
          | isHiding && isNothing subspec = const True
          | otherwise                     = not . mentIsCon
%%]

%%[12
checkEntSpec :: Bool -> (HsName -> Err) -> (HsName -> HsName -> Err) -> ModEntSpec -> ModEntRel -> [Err]
checkEntSpec isHiding errUndef errUndefSub (ModEntSpec x subspec) rel
  | isHiding && isNothing subspec
              = case xents of
                  []   -> [errUndef x]
                  _    -> []
  | otherwise = case xents of
                  []   -> [errUndef x]
                  ents -> concatMap chk $ filter mentIsCon $ ents
  where xents   = Rel.apply rel x
        chk ent = case subspec of
                    Just (ModEntSubs subs)
                      -> map (errUndefSub x) (filter (not . (`Set.member` subsInScope)) subs)
                      where subsInScope
                              = Set.map hsnQualified
                                $ Rel.dom
                                $ Rel.restrictRng (`Set.member` mentOwns ent) rel
                    _ -> []
%%]

%%[12
checkExpSpec :: ModEntRel -> Mod -> [Err]
checkExpSpec inscp mod
  = case modExpL mod of
      Nothing   -> []
      Just exps -> concatMap chk exps
  where aliases = modName mod : mimpAs `map` modImpL mod
        chk (ModExpMod x)
          | x `elem` aliases = []
          | otherwise        = [mkErr_NamesNotIntrod "module alias" [x]]
        chk (ModExpEnt spec) = checkEntSpec False err1 err2 spec inscp
        err1   x = mkErr_NamesNotIntrod ("export") [x]
        err2 e x = mkErr_NamesNotIntrod ("subexport of export " ++ show e) [x]
%%]

%%[12
checkImp :: ModEntRel -> ModImp -> [Err]
checkImp exps imp
  = concatMap chk (mimpImpL imp)
  where src = mimpSource imp
        chk spec = checkEntSpec (mimpHiding imp) err1 err2 spec exps
        err1   x = mkErr_NamesNotIntrod ("module " ++ show src ++ " import") [x]
        err2 i x = mkErr_NamesNotIntrod ("module " ++ show src ++ " subimport of import " ++ show i) [x]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 7 Top level (The semantics of a Haskell program)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12 export(ModMpInfo(..),ModMp,emptyModMpInfo,mkModMpInfo)
data ModMpInfo
  = ModMpInfo
      { mmiInscps   :: ModEntRel
      , mmiExps     :: ModEntRel
      , mmiNmOffMp  :: HsName2OffsetMp
      }

instance Show ModMpInfo where
  show _ = "ModMpInfo"

instance PP ModMpInfo where
  pp i =   "In scp:" >#< (ppAssocL $ Rel.toList $ mmiInscps i)
       >-< "Exps  :" >#< (ppAssocL $ Rel.toList $ mmiExps   i)

emptyModMpInfo :: ModMpInfo
emptyModMpInfo = mkModMpInfo Rel.empty Rel.empty

mkModMpInfo :: ModEntRel -> ModEntRel -> ModMpInfo
mkModMpInfo i e
  = ModMpInfo
      { mmiInscps   = i
      , mmiExps     = e
      , mmiNmOffMp  = expsNmOffMp e
      }

type ModMp = Map.Map HsName ModMpInfo

ppModMp :: ModMp -> PP_Doc
ppModMp = vlist . map (\(n,i) -> n >#< pp i) . Map.toList
%%]

The exported names of the module

%%[12
expsNmOffMp :: ModEntRel -> HsName2OffsetMp
expsNmOffMp exps
  = Map.fromList
    $ flip zip [0..]
    $ sortByOn rowLabCmp hsnQualified
    $ nub
    $ [ ioccNm $ mentIdOcc e | e <- Set.toList $ Rel.rng exps, mentKind e == IdOcc_Val || mentKind e == IdOcc_Inst ]
%%]

%%[12 export(modMpCombine)
modMpCombine ::  [Mod] -> ModMp -> (ModMp,[Err])
modMpCombine ms mp
  = (newMp,concat errs)
  where expsOf mp n     = mmiExps $ Map.findWithDefault emptyModMpInfo n mp
        rels            = modInsOuts (expsOf mp) ms
        (inscps,exps)   = unzip rels
        newMp           = (Map.fromList $ zipWith3 (\n i o -> (n,mkModMpInfo i o)) (map modName ms) inscps exps)
                           `Map.union` mp
        errs            = zipWith (checkMod (fmap mmiExps . (`Map.lookup` newMp))) inscps ms
%%]

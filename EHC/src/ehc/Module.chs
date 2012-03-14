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

%%[50 module {%{EH}Module} import({%{EH}Base.Builtin},{%{EH}Base.Common},{%{EH}Error},{%{EH}NameAspect})
%%]

%%[50 import(Data.Maybe,Data.List,qualified Data.Set as Set,qualified Data.Map as Map)
%%]

%%[50 import(EH.Util.Utils,EH.Util.Pretty,qualified EH.Util.Rel as Rel)
%%]

%%[50 export(ModEnt(..),ModExp(..),ModEntSpec(..),ModEntSubSpec(..),ModImp(..),Mod(..),ModEntRel,ModEntDomMp,ModEntRngMp)
%%]

%%[50 export(emptyMod)
%%]

%%[50 import ({%{EH}Gam},{%{EH}Gam.TyGam},{%{EH}Gam.KiGam}) 
%%]

%%[(50 codegen) import ({%{EH}Core}(HsName2OffsetMp))
%%]

%%[50 import(Control.Monad, {%{EH}Base.Binary}, {%{EH}Base.Serialize})
%%]

%%[99 export(modImpPrelude)
%%]

%%[102 import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Imp/Exp entity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
data ModEnt
  = ModEnt
      { mentKind    :: !IdOccKind
      , mentIdOcc   :: !IdOcc
      , mentOwns    :: !(Set.Set ModEnt)
      , mentRange	:: !Range
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

%%[50 export(mentrelStrip)
mentStrip :: ModEnt -> ModEnt
mentStrip e = e {mentRange = emptyRange}

mentrelStrip :: ModEntRel -> ModEntRel
mentrelStrip = Rel.mapDomRng (\(n,e) -> (n,mentStrip e))
%%]

%%[50
deriving instance Typeable ModEnt
deriving instance Data ModEnt
%%]

%%[50
-- intended for parsing
ppModEnt :: ModEnt -> PP_Doc
ppModEnt e
  = ppCurlysCommasBlock (l1 ++ l2)
  where l1 = [pp (mentKind e),pp (mentIdOcc e)]
        l2 = if Set.null (mentOwns e) then [] else [ppCurlysCommasBlock (map ppModEnt $ Set.toList $ mentOwns e)]

-- intended for parsing
ppModEntRel :: ModEntRel -> PP_Doc
ppModEntRel = ppCurlysAssocL pp ppModEnt . Rel.toList

%%]
ppModEntDomMp :: ModEntDomMp -> PP_Doc
ppModEntDomMp = ppCurlysCommasBlock . map (\(a,b) -> pp a >|< "<>" >|< ppBracketsCommas b) . Map.toList

%%[50
instance PP ModEnt where
  pp = ppModEnt

instance PP ModEntRel where
  pp = ppModEntRel
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Filter Map, for filtering HIInfo by visibility as known from a ModEntRel
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(ModEntRelFilterMp,mentrelFilterMpUnion,mentrelFilterMpUnions,mentrelToFilterMp,mentrelToFilterMp')
-- | names used per category of identifier
type ModEntRelFilterMp = Map.Map IdOccKind HsNameS

mentrelFilterMpUnion :: ModEntRelFilterMp -> ModEntRelFilterMp -> ModEntRelFilterMp
mentrelFilterMpUnion = Map.unionWith Set.union

mentrelFilterMpUnions :: [ModEntRelFilterMp] -> ModEntRelFilterMp
mentrelFilterMpUnions [] = Map.empty
mentrelFilterMpUnions l  = foldr mentrelFilterMpUnion Map.empty l

-- | extract ModEntRelFilterMp from export relation
mentrelToFilterMp' :: Bool -> [HsName] -> ModEntRel -> ModEntRelFilterMp
mentrelToFilterMp' inclOwns exclModNmL r
  = rget True $ Rel.rng r
  where get (ModEnt {mentIdOcc=(IdOcc {ioccKind=k, ioccNm=n}), mentOwns=owns})
                       = mentrelFilterMpUnion (mentrelFilterMpSingleton exclModNmL k n) (rget inclOwns owns)
        rget True occs = mentrelFilterMpUnions [ get e | e <- Set.toList occs ]
        rget _    _    = Map.empty

-- | extract ModEntRelFilterMp from export relation
mentrelToFilterMp :: [HsName] -> ModEntRel -> ModEntRelFilterMp
mentrelToFilterMp = mentrelToFilterMp' True
%%]

%%[50 export(mentrelFilterMpModuleNames)
-- | extract used module names from ModEntRelFilterMp
mentrelFilterMpModuleNames :: ModEntRelFilterMp -> HsNameS
mentrelFilterMpModuleNames m = Set.unions [ Set.map fromJust $ Set.filter isJust $ Set.map hsnQualifier s | s <- Map.elems m ]
%%]

%%[50 export(mentrelFilterMpSingleton)
-- | construct a singleton, only not of the current module
mentrelFilterMpSingleton :: [HsName] -> IdOccKind -> HsName -> ModEntRelFilterMp
mentrelFilterMpSingleton exclModNmL k n
  = case hsnQualifier n of
      Just m | not (m `elem` exclModNmL)
        -> Map.singleton k (Set.singleton n)
        where
      _ -> Map.empty
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Imp/Exp's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
data ModExp
  = ModExpEnt !ModEntSpec
  | ModExpMod !HsName
  deriving (Show)

data ModEntSpec
  = ModEntSpec !HsName !Range !(Maybe ModEntSubSpec)
  deriving (Show)

data ModEntSubSpec
  = ModEntSubAll
  | ModEntSubs ![HsName]
  deriving (Show)

data ModImp
  = ModImp
      { mimpQualified   :: !Bool
      , mimpSource      :: !HsName
      , mimpAs          :: !HsName
      , mimpHiding      :: !Bool
      , mimpImpL        :: ![ModEntSpec]
      , mimpRange		:: !Range
      }
  deriving (Show)

emptyModImp :: ModImp
emptyModImp = ModImp False hsnUnknown hsnUnknown True [] emptyRange

%%]

%%[50 export(modImpBuiltin)
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

%%[50
instance PP ModExp where
  pp (ModExpEnt s) = pp s
  pp (ModExpMod m) = "module" >#< m

instance PP ModEntSpec where
  pp (ModEntSpec n _ s) = n >|< maybe empty pp s

instance PP ModEntSubSpec where
  pp ModEntSubAll = pp "(..)"
  pp (ModEntSubs ns) = ppParensCommas ns

instance PP ModImp where
  pp i = mimpSource i >|< ppParensCommas (mimpImpL i)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(emptyMod')
data Mod
  = Mod
      { modName         :: !HsName
      , modNameInSrc    :: !(Maybe HsName)
      , modExpL         :: !(Maybe [ModExp])
      , modImpL         :: ![ModImp]
      , modDefs         :: !ModEntRel
      , modHiddenExps   :: !ModEntRel
      , modInstNmL      :: ![HsName]
      }
  deriving (Show)

emptyMod' n = Mod n Nothing Nothing [] Rel.empty Rel.empty []
emptyMod = emptyMod' hsnUnknown
%%]

%%[50 export(modBuiltin)
modBuiltin
  = emptyMod
      { modName         = hsnModBuiltin
      , modDefs         = defs
      }
  where defs
          = Rel.fromList [ (n,ModEnt IdOcc_Type (IdOcc n IdOcc_Type) Set.empty emptyRange) | (n,_) <- gamToAssocL initTyGam ]
            `Rel.union` Rel.fromList [ (n,ModEnt IdOcc_Kind (IdOcc n IdOcc_Kind) Set.empty emptyRange) | (n,_) <- gamToAssocL initKiGam ]
%%]

%%[50
instance PP Mod where
  pp m = modName m >|< "/" >|< modNameInSrc m
         >-< indent 2 (   "IMP" >#< ppParensCommas (modImpL m)
                      >-< "EXP" >#< maybe empty ppParensCommas (modExpL m)
                      >-< "HID" >#< pp (modHiddenExps m)
                      >-< "DEF" >#< pp (modDefs m)
                      )
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 5.1 Importing or exporting an entity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
modEntSpec :: Bool -> ModEntRel -> ModEntSpec -> ModEntRel
modEntSpec isHiding rel (ModEntSpec x _ subspec)
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

%%[50
modExports :: Mod -> ModEntRel -> ModEntRel
modExports mod inscp
  = case modExpL mod of
      Nothing -> modDefs mod
      Just es -> hsnQualified `Rel.mapDom` Rel.unions exps
              where exps = modExpListEntry inscp `map` es
%%]

%%[50
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

%%[50
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

%%[50
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

%%[50
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

%%[50
lfpAfter :: Eq x => (x -> x) -> x -> x
lfpAfter f x
  = if fx == x then fx else lfpAfter f fx
  where fx = f x
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 6 Error detection
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
checkMod :: (HsName -> Maybe ModEntRel) -> ModEntRel -> Mod -> [Err]
checkMod expsOf inscp mod
  = checkAmbigExps modExports
    ++ if null missingModules
       then checkExpSpec inscp mod
            ++ [ err | (imp,Just exps) <- impSources, err <- checkImp exps imp ]
       else [rngLift emptyRange mkErr_NamesNotIntrod' "module" missingModules]
  where Just modExports = expsOf (modName mod)
        impSources      = [ (imp,expsOf (mimpSource imp)) | imp <- modImpL mod ]
        missingModules  = nubOn fst [ mkThingAnd1Range (mimpRange imp) (mimpSource imp) | (imp,Nothing) <- impSources ]
%%]

%%[50
checkAmbigExps :: ModEntRel -> [Err]
checkAmbigExps exps
  = concatMap isAmbig (Set.toList (Rel.dom exps))
  where isAmbig n = ambig n cons ++ ambig n other
                  where (cons,other) = partition mentIsCon (Rel.apply exps n)
        ambig n ents@(_:_:_) | not (null a)
                             = [rngLift emptyRange Err_AmbiguousExport n (map mkn $ concat $ a)]
                             where a   = [ l | l@(_:_:_) <- groupSortOn (ioccKind . mentIdOcc) ents ]
%%[[50
                                   mkn   = pp . ioccNm . mentIdOcc
%%][99
                                   mkn o = (ioccNm $ mentIdOcc o, Just [(mentRange o,Nothing)])
%%]]
        ambig n _            = []
%%]

%%[50
checkEntSpec :: Bool -> (HsName -> Range -> Err) -> (HsName -> HsName -> Err) -> ModEntSpec -> ModEntRel -> [Err]
checkEntSpec isHiding errUndef errUndefSub (ModEntSpec x xrange subspec) rel
  | isHiding && isNothing subspec
              = case xents of
                  []   -> [errUndef x xrange]
                  _    -> []
  | otherwise = case xents of
                  []   -> [errUndef x xrange]
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

%%[50
checkExpSpec :: ModEntRel -> Mod -> [Err]
checkExpSpec inscp mod
  = case modExpL mod of
      Nothing   -> []
      Just exps -> concatMap chk exps
  where aliases = modName mod : mimpAs `map` modImpL mod
        chk (ModExpMod x)
          | x `elem` aliases = []
          | otherwise        = [rngLift emptyRange mkErr_NamesNotIntrod "module alias" [x]]
        chk (ModExpEnt spec) = checkEntSpec False err1 err2 spec inscp
        err1   x r = rngLift emptyRange mkErr_NamesNotIntrod' ("export") [mkThingAnd1Range r x]
        err2 e x   = rngLift emptyRange mkErr_NamesNotIntrod  ("subexport of export " ++ show e) [x]
%%]

%%[50
checkImp :: ModEntRel -> ModImp -> [Err]
checkImp exps imp
  = concatMap chk (mimpImpL imp)
  where src = mimpSource imp
        chk spec = checkEntSpec (mimpHiding imp) err1 err2 spec exps
        err1   x r = rngLift emptyRange mkErr_NamesNotIntrod' ("module " ++ show src ++ " import") [mkThingAnd1Range r x]
        err2 i x   = rngLift emptyRange mkErr_NamesNotIntrod  ("module " ++ show src ++ " subimport of import " ++ show i) [x]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 7 Top level (The semantics of a Haskell program)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(ModMpInfo(..),ModMp,emptyModMpInfo,mkModMpInfo)
data ModMpInfo
  = ModMpInfo
      { mmiInscps   		:: !ModEntRel
      , mmiExps     		:: !ModEntRel
      , mmiHiddenExps     	:: !ModEntRel
%%[[(50 codegen)
      , mmiNmOffMp  		:: !HsName2OffsetMp		-- cached mapping of names to offsets, for all that is exported, visible or hidden
%%]]
      }

instance Show ModMpInfo where
  show _ = "ModMpInfo"

instance PP ModMpInfo where
  pp i =   "In scp     :" >#< (ppAssocL $ Rel.toList $ mmiInscps       i)
       >-< "Exps       :" >#< (ppAssocL $ Rel.toList $ mmiExps         i)
       >-< "Hidden Exps:" >#< (ppAssocL $ Rel.toList $ mmiHiddenExps   i)

emptyModMpInfo :: ModMpInfo
emptyModMpInfo = mkModMpInfo hsnUnknown Rel.empty Rel.empty Rel.empty

mkModMpInfo :: HsName -> ModEntRel -> ModEntRel -> ModEntRel -> ModMpInfo
mkModMpInfo modNm i e he
  = resetModMpInfo modNm
    $ ModMpInfo
        { mmiInscps   		= i
        , mmiExps     		= e
        , mmiHiddenExps     = he
%%[[(50 codegen)
        , mmiNmOffMp  		= Map.empty
%%]]
        }

resetModMpInfo :: HsName -> ModMpInfo -> ModMpInfo
%%[[50
resetModMpInfo _     i = i
%%][(50 codegen)
resetModMpInfo modNm i = i {mmiNmOffMp = expsNmOffMp modNm $ mmiExps i `Rel.union` mmiHiddenExps i}
%%]]
%%]

%%[50 export(ppModMp)
type ModMp = Map.Map HsName ModMpInfo

ppModMp :: ModMp -> PP_Doc
ppModMp = vlist . map (\(n,i) -> n >#< pp i) . Map.toList
%%]

%%[50 export(modMpAddHiddenExps)
modMpAddHiddenExps :: HsName -> [(HsName,IdOccKind)] -> ModMp -> ModMp
modMpAddHiddenExps modNm newExpNms mm
  = Map.update (\i@(ModMpInfo {mmiHiddenExps=he})
                  -> Just $ resetModMpInfo modNm
                          $ i { mmiHiddenExps
                                  = Rel.fromList [ (n, ModEnt occk (IdOcc n occk) Set.empty emptyRange) | (n,occk) <- newExpNms ]
                                    `Rel.union` he
                              }
               ) modNm mm
%%]

The exported names of the module

%%[(50 codegen)
expsNmOffMp :: HsName -> ModEntRel -> HsName2OffsetMp
expsNmOffMp modNm exps
  = Map.fromList
    $ flip zip [0..]
    $ sortBy cmpHsNameOnNm
    $ nub
    $ [ nm
      | e <- Set.toList $ Rel.rng exps
      , mentKind e == IdOcc_Val
      , let nm     = ioccNm (mentIdOcc e)
            mbqual = hsnQualifier nm
      , isJust mbqual		-- unqualified names cannot be exported, but they should not intro'd in the 1st place!! TBD 20100303 AD
      , panicJust ("Module.expsNmOffMp: " ++ show nm) mbqual == modNm
      ]
%%]

%%[50 export(modMpCombine)
modMpCombine ::  [Mod] -> ModMp -> (ModMp,[Err])
modMpCombine ms mp
  = (newMp,concat errs)
  where expsOf mp n     = mmiExps $ Map.findWithDefault emptyModMpInfo n mp
        rels            = modInsOuts (expsOf mp) ms
        (inscps,exps)   = unzip rels
        newMp           = (Map.fromList $ zipWith4 (\n i o ho -> (n,mkModMpInfo n i o ho)) (map modName ms) inscps exps (map modHiddenExps ms))
                           `Map.union` mp
        errs            = zipWith (checkMod (fmap mmiExps . (`Map.lookup` newMp))) inscps ms
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: ForceEval
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[102
instance ForceEval ModEnt where
  fevCount (ModEnt k i o) = cmUnions [cm1 "ModEnt",fevCount k,fevCount i,fevCount o]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
instance Serialize ModEnt where
  sput (ModEnt a b c d) = sput a >> sput b >> sput c >> sput d
  sget = liftM4 ModEnt sget sget sget sget
%%]


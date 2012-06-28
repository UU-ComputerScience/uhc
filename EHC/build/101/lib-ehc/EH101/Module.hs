module EH101.Module
( ModEnt (..), ModExp (..), ModEntSpec (..), ModEntSubSpec (..), ModImp (..), Mod (..), ModEntRel, ModEntDomMp, ModEntRngMp
, emptyMod
, mentrelStrip
, ModEntRelFilterMp, mentrelFilterMpUnion, mentrelFilterMpUnions, mentrelToFilterMp, mentrelToFilterMp'
, mentrelFilterMpModuleNames
, mentrelFilterMpSingleton
, modImpBuiltin
, emptyMod'
, modBuiltin
, ModMpInfo (..), ModMp, emptyModMpInfo, mkModMpInfo
, ppModMp
, modMpAddHiddenExps
, modMpCombine
, modImpPrelude )
where
import EH101.Base.Builtin
import EH101.Base.Common
import EH101.Error
import EH101.NameAspect
import Data.Maybe
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import EH.Util.Utils
import EH.Util.Pretty
import qualified EH.Util.Rel as Rel
import EH101.Gam
import EH101.Gam.TyGam
import EH101.Gam.KiGam
import EH101.Core (HsName2OffsetMp)
import Control.Monad
import EH101.Base.Binary
import EH101.Base.Serialize





{-# LINE 50 "src/ehc/Module.chs" #-}
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


{-# LINE 78 "src/ehc/Module.chs" #-}
mentStrip :: ModEnt -> ModEnt
mentStrip e = e {mentRange = emptyRange}

mentrelStrip :: ModEntRel -> ModEntRel
mentrelStrip = Rel.mapDomRng (\(n,e) -> (n,mentStrip e))

{-# LINE 86 "src/ehc/Module.chs" #-}
deriving instance Typeable ModEnt
deriving instance Data ModEnt

{-# LINE 91 "src/ehc/Module.chs" #-}
-- intended for parsing
ppModEnt :: ModEnt -> PP_Doc
ppModEnt e
  = ppCurlysCommasBlock (l1 ++ l2)
  where l1 = [pp (mentKind e),pp (mentIdOcc e)]
        l2 = if Set.null (mentOwns e) then [] else [ppCurlysCommasBlock (map ppModEnt $ Set.toList $ mentOwns e)]

-- intended for parsing
ppModEntRel :: ModEntRel -> PP_Doc
ppModEntRel = ppCurlysAssocL pp ppModEnt . Rel.toList


{-# LINE 107 "src/ehc/Module.chs" #-}
instance PP ModEnt where
  pp = ppModEnt

instance PP ModEntRel where
  pp = ppModEntRel

{-# LINE 119 "src/ehc/Module.chs" #-}
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

{-# LINE 144 "src/ehc/Module.chs" #-}
-- | extract used module names from ModEntRelFilterMp
mentrelFilterMpModuleNames :: ModEntRelFilterMp -> HsNameS
mentrelFilterMpModuleNames m = Set.unions [ Set.map fromJust $ Set.filter isJust $ Set.map hsnQualifier s | s <- Map.elems m ]

{-# LINE 150 "src/ehc/Module.chs" #-}
-- | construct a singleton, only not of the current module
mentrelFilterMpSingleton :: [HsName] -> IdOccKind -> HsName -> ModEntRelFilterMp
mentrelFilterMpSingleton exclModNmL k n
  = case hsnQualifier n of
      Just m | not (m `elem` exclModNmL)
        -> Map.singleton k (Set.singleton n)
        where
      _ -> Map.empty

{-# LINE 165 "src/ehc/Module.chs" #-}
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


{-# LINE 196 "src/ehc/Module.chs" #-}
modImpBuiltin :: ModImp
modImpBuiltin
  = emptyModImp
      { mimpSource      = hsnModBuiltin
      , mimpAs          = hsnModBuiltin
      }

{-# LINE 205 "src/ehc/Module.chs" #-}
modImpPrelude :: ModImp
modImpPrelude
  = emptyModImp
      { mimpSource      = hsnModPrelude
      , mimpAs          = hsnModPrelude
      }

{-# LINE 214 "src/ehc/Module.chs" #-}
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

{-# LINE 234 "src/ehc/Module.chs" #-}
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

{-# LINE 251 "src/ehc/Module.chs" #-}
modBuiltin
  = emptyMod
      { modName         = hsnModBuiltin
      , modDefs         = defs
      }
  where defs
          = Rel.fromList [ (n,ModEnt IdOcc_Type (IdOcc n IdOcc_Type) Set.empty emptyRange) | (n,_) <- gamToAssocL initTyGam ]
            `Rel.union` Rel.fromList [ (n,ModEnt IdOcc_Kind (IdOcc n IdOcc_Kind) Set.empty emptyRange) | (n,_) <- gamToAssocL initKiGam ]

{-# LINE 262 "src/ehc/Module.chs" #-}
instance PP Mod where
  pp m = modName m >|< "/" >|< modNameInSrc m
         >-< indent 2 (   "IMP" >#< ppParensCommas (modImpL m)
                      >-< "EXP" >#< maybe empty ppParensCommas (modExpL m)
                      >-< "HID" >#< pp (modHiddenExps m)
                      >-< "DEF" >#< pp (modDefs m)
                      )

{-# LINE 276 "src/ehc/Module.chs" #-}
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

{-# LINE 295 "src/ehc/Module.chs" #-}
modExports :: Mod -> ModEntRel -> ModEntRel
modExports mod inscp
  = case modExpL mod of
      Nothing -> modDefs mod
      Just es -> hsnQualified `Rel.mapDom` Rel.unions exps
              where exps = modExpListEntry inscp `map` es

{-# LINE 304 "src/ehc/Module.chs" #-}
modExpListEntry :: ModEntRel -> ModExp -> ModEntRel
modExpListEntry inscp (ModExpEnt it)
  = modEntSpec False inscp it
modExpListEntry inscp (ModExpMod m)
  = (hsnSetQual m `Rel.mapDom` unqs) `Rel.intersection` qs
  where (qs,unqs) = Rel.partitionDom hsnIsQual inscp

{-# LINE 317 "src/ehc/Module.chs" #-}
modInscope :: Mod -> (HsName -> ModEntRel) -> ModEntRel
modInscope m expsOf
  = Rel.unions [imports,locals]
  where defEnts = modDefs m
        locals  = Rel.unions
                    [ hsnQualified `Rel.mapDom` defEnts
                    , hsnSetQual (modName m) `Rel.mapDom` defEnts
                    ]
        imports = Rel.unions $ map (modImp expsOf) (modImpL m)

{-# LINE 329 "src/ehc/Module.chs" #-}
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

{-# LINE 348 "src/ehc/Module.chs" #-}
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

{-# LINE 361 "src/ehc/Module.chs" #-}
lfpAfter :: Eq x => (x -> x) -> x -> x
lfpAfter f x
  = if fx == x then fx else lfpAfter f fx
  where fx = f x

{-# LINE 372 "src/ehc/Module.chs" #-}
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

{-# LINE 385 "src/ehc/Module.chs" #-}
checkAmbigExps :: ModEntRel -> [Err]
checkAmbigExps exps
  = concatMap isAmbig (Set.toList (Rel.dom exps))
  where isAmbig n = ambig n cons ++ ambig n other
                  where (cons,other) = partition mentIsCon (Rel.apply exps n)
        ambig n ents@(_:_:_) | not (null a)
                             = [rngLift emptyRange Err_AmbiguousExport n (map mkn $ concat $ a)]
                             where a   = [ l | l@(_:_:_) <- groupSortOn (ioccKind . mentIdOcc) ents ]
                                   mkn o = (ioccNm $ mentIdOcc o, Just [(mentRange o,Nothing)])
        ambig n _            = []

{-# LINE 402 "src/ehc/Module.chs" #-}
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

{-# LINE 423 "src/ehc/Module.chs" #-}
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

{-# LINE 438 "src/ehc/Module.chs" #-}
checkImp :: ModEntRel -> ModImp -> [Err]
checkImp exps imp
  = concatMap chk (mimpImpL imp)
  where src = mimpSource imp
        chk spec = checkEntSpec (mimpHiding imp) err1 err2 spec exps
        err1   x r = rngLift emptyRange mkErr_NamesNotIntrod' ("module " ++ show src ++ " import") [mkThingAnd1Range r x]
        err2 i x   = rngLift emptyRange mkErr_NamesNotIntrod  ("module " ++ show src ++ " subimport of import " ++ show i) [x]

{-# LINE 452 "src/ehc/Module.chs" #-}
data ModMpInfo
  = ModMpInfo
      { mmiInscps   		:: !ModEntRel
      , mmiExps     		:: !ModEntRel
      , mmiHiddenExps     	:: !ModEntRel
      , mmiNmOffMp  		:: !HsName2OffsetMp		-- cached mapping of names to offsets, for all that is exported, visible or hidden
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
        , mmiNmOffMp  		= Map.empty
        }

resetModMpInfo :: HsName -> ModMpInfo -> ModMpInfo
resetModMpInfo modNm i = i {mmiNmOffMp = expsNmOffMp modNm $ mmiExps i `Rel.union` mmiHiddenExps i}

{-# LINE 494 "src/ehc/Module.chs" #-}
type ModMp = Map.Map HsName ModMpInfo

ppModMp :: ModMp -> PP_Doc
ppModMp = vlist . map (\(n,i) -> n >#< pp i) . Map.toList

{-# LINE 501 "src/ehc/Module.chs" #-}
modMpAddHiddenExps :: HsName -> [(HsName,IdOccKind)] -> ModMp -> ModMp
modMpAddHiddenExps modNm newExpNms mm
  = Map.update (\i@(ModMpInfo {mmiHiddenExps=he})
                  -> Just $ resetModMpInfo modNm
                          $ i { mmiHiddenExps
                                  = Rel.fromList [ (n, ModEnt occk (IdOcc n occk) Set.empty emptyRange) | (n,occk) <- newExpNms ]
                                    `Rel.union` he
                              }
               ) modNm mm

{-# LINE 515 "src/ehc/Module.chs" #-}
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

{-# LINE 532 "src/ehc/Module.chs" #-}
modMpCombine ::  [Mod] -> ModMp -> (ModMp,[Err])
modMpCombine ms mp
  = (newMp,concat errs)
  where expsOf mp n     = mmiExps $ Map.findWithDefault emptyModMpInfo n mp
        rels            = modInsOuts (expsOf mp) ms
        (inscps,exps)   = unzip rels
        newMp           = (Map.fromList $ zipWith4 (\n i o ho -> (n,mkModMpInfo n i o ho)) (map modName ms) inscps exps (map modHiddenExps ms))
                           `Map.union` mp
        errs            = zipWith (checkMod (fmap mmiExps . (`Map.lookup` newMp))) inscps ms

{-# LINE 557 "src/ehc/Module.chs" #-}
instance Serialize ModEnt where
  sput (ModEnt a b c d) = sput a >> sput b >> sput c >> sput d
  sget = liftM4 ModEnt sget sget sget sget


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

%%[8 module {%{EH}Module} import(Data.Maybe,Data.List,qualified Data.Set as Set,UU.Pretty,qualified EH.Util.Rel as Rel,{%{EH}Base.Common},{%{EH}Error})
%%]

%%[8 export(ModEnt(..),ModExp(..),ModEntSpec(..),ModEntSubSpec(..),ModImp(..),Mod(..),ModEntRel)
%%]

%%[8 export(emptyMod)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Imp/Exp entity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data ModEnt
  = ModEntVal   { mentName :: HsName }
  | ModEntType  { mentName :: HsName }
  | ModEntData  { mentName :: HsName, mentSubs :: Set.Set ModEnt }
  | ModEntClass { mentName :: HsName, mentSubs :: Set.Set ModEnt }
  deriving (Show,Eq,Ord)

type ModEntRel = Rel.Rel HsName ModEnt

mentIsCon :: ModEnt -> Bool
mentIsCon (ModEntData  _ _) = True
mentIsCon (ModEntClass _ _) = True
mentIsCon _                 = False

mentOwns :: ModEnt -> Set.Set ModEnt
mentOwns (ModEntData  _ s) = s
mentOwns (ModEntClass _ s) = s
mentOwns _                 = Set.empty
%%]

%%[8
instance PP ModEnt where
  pp e = pp (mentName e)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Imp/Exp's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data ModExp
  = ModExpEnt ModEntSpec
  | ModExpMod HsName

data ModEntSpec
  = ModEntSpec HsName (Maybe ModEntSubSpec)

data ModEntSubSpec
  = ModEntSubAll
  | ModEntSubs [HsName]

data ModImp
  = ModImp
      { mimpQualified   :: Bool
      , mimpSource      :: HsName
      , mimpAs          :: HsName
      , mimpHiding      :: Bool
      , mimpImpL        :: [ModEntSpec]
      }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data Mod
  = Mod
      { modName     :: HsName
      , modExpL     :: Maybe [ModExp]
      , modImpL     :: [ModImp]
      , modDefs     :: ModEntRel
      }

emptyMod = Mod hsnUnknown Nothing [] Rel.empty
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 5.1 Importing or exporting an entity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 5.2 Export relations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
modExports :: Mod -> ModEntRel -> ModEntRel
modExports mod inscp
  = case modExpL mod of
      Nothing -> modDefs mod
      Just es -> hsnQualified `Rel.mapDom` Rel.unions exps
              where exps = modExpListEntry inscp `map` es
%%]

%%[8
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

%%[8
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

%%[8
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

%%[8
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

%%[8
lfpAfter :: Eq x => (x -> x) -> x -> x
lfpAfter f x
  = if fx == x then fx else lfpAfter f fx
  where fx = f x
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 6 Error detection
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
checkMod :: (HsName -> Maybe ModEntRel) -> ModEntRel -> Mod -> [Err]
checkMod expsOf inscp mod
  = checkAmbigExps modExports
    ++ if null missingModules
       then checkExpSpec inscp mod
            ++ [ err | (imp,Just exps) <- impSources, err <- checkImp exps imp ]
       else map (\n -> Err_NamesNotIntrod "module" [n]) missingModules
  where Just modExports = expsOf (modName mod)
        impSources      = [ (imp,expsOf (mimpSource imp)) | imp <- modImpL mod ]
        missingModules  = nub [ mimpSource imp | (imp,Nothing) <- impSources ]
%%]

%%[8
checkAmbigExps :: ModEntRel -> [Err]
checkAmbigExps exps
  = concatMap isAmbig (Set.toList (Rel.dom exps))
  where isAmbig n = ambig n cons ++ ambig n other
                  where (cons,other) = partition mentIsCon (Rel.apply exps n)
        ambig n ents@(_:_:_) = [Err_AmbiguousExport n (map pp ents)]
        ambig n _            = []
%%]

%%[8
checkEntSpec :: Bool -> (HsName -> Err) -> (HsName -> HsName -> Err) -> ModEntSpec -> ModEntRel -> [Err]
checkEntSpec isHiding errUndef errUndefSub (ModEntSpec x subspec) rel
  = case xents of
      []   -> [errUndef x]
      ents -> concatMap chk ents
  where xents   = filter consider (Rel.apply rel x)
        chk ent = case subspec of
                    Just (ModEntSubs subs)
                      -> map (errUndefSub x) (filter (not . (`Set.member` subsInScope)) subs)
                      where subsInScope = Set.map hsnQualified $ Rel.dom $ Rel.restrictRng (`Set.member` mentOwns ent) rel
                    _ -> []
        consider
          | isHiding && isNothing subspec = const True
          | otherwise                     = not . mentIsCon
%%]

%%[8
checkExpSpec :: ModEntRel -> Mod -> [Err]
checkExpSpec inscp mod
  = case modExpL mod of
      Nothing   -> []
      Just exps -> concatMap chk exps
  where aliases = modName mod : mimpAs `map` modImpL mod
        chk (ModExpMod x)
          | x `elem` aliases = []
          | otherwise        = [Err_NamesNotIntrod "module alias" [x]]
        chk (ModExpEnt spec) = checkEntSpec False err1 err2 spec inscp
        err1   x = Err_NamesNotIntrod ("export") [x]
        err2 e x = Err_NamesNotIntrod ("subexport of export " ++ show e) [x]
%%]

%%[8
checkImp :: ModEntRel -> ModImp -> [Err]
checkImp exps imp
  = concatMap chk (mimpImpL imp)
  where src = mimpSource imp
        chk spec = checkEntSpec (mimpHiding imp) err1 err2 spec exps
        err1   x = Err_NamesNotIntrod ("module " ++ show src ++ " import") [x]
        err2 i x = Err_NamesNotIntrod ("module " ++ show src ++ " subimport of import " ++ show i) [x]
%%]


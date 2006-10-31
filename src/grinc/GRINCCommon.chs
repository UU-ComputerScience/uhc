%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Grinc Common
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{GRIN}GRINCCommon}
%%]
%%[8 import( {%{EH}Base.Common}, qualified Data.Map as Map, qualified Data.Set as Set, Char(isDigit))
%%]

%%[8 export(wildcardNm, wildcardNr, evalNm, evalNr,  applyNm, applyNr, isSpecialBind, getNr, throwTag, blackholeTag)

wildcardNm = HNm "_"
wildcardNr = HNPos (0)
evalNm     = HNm "!eval"
evalNr     = HNPos 1
applyNm    = HNm "!apply"
applyNr    = HNPos 2

isSpecialBind f = f == evalNm || f == applyNm

getNr :: HsName -> Int
getNr (HNPos i) = i
getNr a         = error $ "not a numbered name: " ++ show a

--note: this is copied to HeapPointsToFixpoint.chs
blackholeTag  =  GrTag_Lit GrTagHole  0 (HNm "blackhole")
throwTag      =  GrTag_Lit GrTagFun   0 (HNm "rethrow")
%%]

%%[8 import({%{EH}GrinCode})
%%]

%%[8 export(IdentNameMap, IdentOneToMany, RenameMap, mergeRenameMap, getName, getName')

type IdentNameMap   = (Array Int HsName, Map.Map Int Int)
type IdentOneToMany = (Int,  [Int])
type RenameMap      = [(Int,  [Int])]

mergeRenameMap :: IdentNameMap -> RenameMap -> IdentNameMap
mergeRenameMap (a,m) rm = (a,foldl addToMap m rm)
    where
    addToMap m (orig,vars) = foldl (\m v -> Map.insert v orig m) m vars

getName :: IdentNameMap -> Int -> String
getName m i = show $ getName' m (HNPos i)

getName' :: IdentNameMap -> HsName -> HsName
getName' (names, m) nm@(HNPos i)
  = if wildcardNr == nm
    then wildcardNm
    else if applyNr == nm
    then applyNm
    else if evalNr == nm
    then evalNm
    else let newNm = findNewVar' i ""
         in if   isDigit (head (show newNm))
            then HNm ('x':'_':show i)
            else newNm
    where
    inBetween n (l, h) = n >= l && n <= h
    findNewVar' v suffix = if v `inBetween` bounds names
                           then hsnSuffix (names ! v) suffix
                           else maybe (hsnSuffix (HNPos v) suffix) id (Map.lookup v m >>= return . flip findNewVar' ("_" ++ show v ++ suffix))
getName' _  nm = error $ "findNewVar: Not a number: " ++ show nm

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Heap Points To Analysis Result %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8.analysis import( {%{GRIN}HeapPointsToFixpoint},Data.Array, Data.Monoid) export(HptMap, getEnvVar, getHeapLoc, absFetch, addEnvVar, addEnvVars, getTags, getNodes, isBottom, AbstractValue(..), listInsert)


type HptMap        = ((Array Int AbstractEnvElement, Array Int AbstractHeapElement), Map.Map Int AbstractValue)
getEnvVar :: HptMap -> Int -> AbstractValue
getEnvVar ((ea, _),m) i  | snd (bounds ea) >= i = aeBaseSet (ea ! i)
                         | otherwise            = Map.findWithDefault (AV_Error $ "variable "++ show i ++ " not found") i m
getHeapLoc :: HptMap -> Int -> AbstractValue
getHeapLoc ((_, ha),_) i = ahBaseSet (ha ! i)

absFetch :: HptMap -> HsName -> AbstractValue
absFetch a (HNPos i) = case getEnvVar a i of
                             AV_Locations l -> mconcat $ map (getHeapLoc a) (Set.toList l)
                             AV_Nothing     -> AV_Nodes Map.empty
                             AV_Error s     -> error $ "analysis error: " ++ s
                             AV_Basic       -> error $ "variable " ++ show i ++ " is a basic value"
                             AV_Nodes _     -> error $ "variable " ++ show i ++ "is a node variable"
absFetch a x = error ("absFetch tried on " ++ show x)

getTags av = case av of
                 AV_Tags  ts -> Set.toList ts
                 _           -> map fst (getNodes av)

getNodes av = case av of
                  AV_Nodes n  -> Map.toAscList n
                  AV_Nothing  -> []
                  AV_Error s  -> error $ "analysis error: " ++  s
                  _           -> error $ "not a node: " ++ show av

isBottom av = case av of
                  AV_Nothing      ->  True
                  AV_Locations l  ->  Set.null l
                  AV_Nodes n      ->  Map.null n
                  AV_Error s      ->  error $ "analysis error: " ++ s
                  otherwise       ->  False

addEnvVar :: HptMap -> Int -> AbstractValue -> HptMap
addEnvVar (a,fm) i v = (a, Map.insert i v fm)

addEnvVars :: HptMap -> [(Int, AbstractValue)] -> HptMap
addEnvVars (a,fm) l = (a, foldl (flip $ uncurry Map.insert) fm l)

listInsert l fm = foldl (flip $ uncurry Map.insert) fm l
%%]

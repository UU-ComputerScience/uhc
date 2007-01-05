%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]
%%[8 module {%{GRIN}GRINCCommon}
%%]
%%[8 import( qualified Data.Map as Map, qualified Data.Set as Set, Data.Array, Data.Monoid, Char(isDigit) )
%%]
%%[8 import( {%{EH}Base.Common}, {%{EH}Base.Builtin} )
%%]
%%[8 import( {%{EH}GrinCode} )
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Special names                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(RenameMap, wildcardNm, wildcardNr, evalNm, evalNr,  applyNm, applyNr, mainNr, isSpecialBind, getNr, throwTag)

type RenameMap      = [(Int,  [Int])]

wildcardNm = HNm "_"
wildcardNr = HNmNr 0 (Just wildcardNm)
evalNm     = HNm "!eval"
evalNr     = HNmNr 1 (Just evalNm)
applyNm    = HNm "!apply"
applyNr    = HNmNr 2 (Just applyNm)

mainNr     = HNmNr 3 (Just (hsnPrefix "fun_" hsnMain))
--mainNr     = HNmNr 3 (Just (hsnMain))

isSpecialBind f = f == evalNm || f == applyNm || f==hsnMain

getNr :: HsName -> Int
getNr (HNmNr i _) = i
getNr (HNPos i)   = error $ "getNr tried on HNPos " ++ show i
getNr a           = error $ "getNr tried on " ++ show a

throwTag      =  GrTag_Lit GrTagFun   0 (HNm "rethrow")
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Abstract interpretation domain %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(Location, Variable, AbstractValue(..), AbstractNode)
%%]

%%[8.AbstractValue

type Location = Int
type Variable = Int


data AbstractValue
  = AV_Nothing
  | AV_Basic
  | AV_Locations (Set.Set Location)
  | AV_Nodes (Map.Map GrTag [AbstractValue])
  | AV_Tags  (Set.Set GrTag)
  | AV_Error !String
    deriving (Eq, Ord)

type AbstractNode = (GrTag, [AbstractValue]) -- an AV_Nodes can not occur inside an AbstractNode

instance Show AbstractValue where
    show av = case av of
                  AV_Nothing      -> "BOT"
                  AV_Basic        -> "BAS"
                  AV_Locations ls -> "LOCS" ++ show (Set.elems ls)
                  AV_Nodes     ns -> "NODS" ++ show (Map.assocs ns)
                  AV_Tags      ts -> "TAGS" ++ show (Set.elems ts)
                  AV_Error     s  -> "ERR: " ++ s

instance Monoid AbstractValue where
    mempty  = AV_Nothing
    mappend a          AV_Nothing = a
    mappend AV_Nothing b          = b
    mappend a          b          = case (a,b) of
                                      (AV_Basic       , AV_Basic       ) -> AV_Basic
                                      (AV_Locations al, AV_Locations bl) -> AV_Locations (Set.union al bl)
                                      (AV_Nodes     an, AV_Nodes     bn) -> AV_Nodes (an `mergeNodes` bn)
                                      (AV_Tags      at, AV_Tags      bt) -> AV_Tags (Set.union at bt)
                                      (AV_Error     _ , _              ) -> a
                                      (_              , AV_Error     _ ) -> b
                                      (AV_Basic       , _              ) -> b   -- works, but is it correct? --JF
                                      (_              , AV_Basic       ) -> a   -- works, but is it correct? --JF
                                      otherwise                          -> AV_Error $ "Wrong variable usage: Location, node or basic value mixed" ++ show a ++ " / " ++ show b

mergeNodes an bn = Map.unionWith (zipWith mappend) an bn

-- (Ord GrTag) is needed for (Ord AbstractValue) which is needed for Map.unionWith in mergeNodes
instance Ord GrTag where
    compare t1 t2 = case t1 of
                        GrTag_Any         -> case t2 of
                                                 GrTag_Any         -> EQ
                                                 otherwise         -> LT
                        GrTag_Unboxed     -> case t2 of
                                                 GrTag_Any         -> GT
                                                 GrTag_Unboxed     -> EQ
                                                 otherwise         -> LT
                        GrTag_Lit c1 _ n1 -> case t2 of
                                                 GrTag_Any         -> GT
                                                 GrTag_Unboxed     -> GT
                                                 GrTag_Lit c2 _ n2 -> case compare c1 c2 of
                                                                          EQ -> compare n1 n2
                                                                          a  -> a
                                                 GrTag_Var _       -> LT
                        GrTag_Var n1      -> case t2 of
                                                 GrTag_Var n2      -> compare n1 n2
                                                 otherwise         -> GT

{-
-- obsolete abstract domain for Locations
data AbstractHeapElement = AbstractHeapElement
    { ahBaseSet    ::  !AbstractValue
    , ahSharedSet  ::  !(Maybe AbstractValue)
    }
    deriving (Eq)

-- TODO: which should ahSharedSet hold: <value when shared> - <value when uniq> or <value when shared>
-- Note: ahSharedSet currently holds the former, Nothing means it the cell shared, Just means unique (and shared part is kept off the record)

instance Show AbstractHeapElement where
    show (AbstractHeapElement b s) =  "unique = "       ++ show b
                                       ++ ";\tshared = "  ++ show s
-}

%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Abstract interpretation constraints     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(Equation, Equations, EquationRhs(..), HeapEquation, HeapEquations, HeapEquationRhs, EvalMap, ApplyMap )

type Equation      = (Variable, EquationRhs)
type HeapEquation  = (Location, HeapEquationRhs)
type Equations     = [Equation]
type HeapEquations = [HeapEquation]

type HeapEquationRhs = ((GrTag, [Maybe Variable]), Maybe Variable)
data EquationRhs
  = EquationKnownToBe !AbstractValue
  | EquationShouldBe  ![Variable]
  | EquationEval      Variable Variable
  | EquationApply     Variable [Variable] Variable
  | EquationSelect    Variable GrTag Int
  | EquationTag       GrTag [Maybe Variable] (Maybe Variable)
    deriving (Show, Eq)

type EvalMap     = AssocL GrTag Int
type ApplyMap    = AssocL GrTag (Either GrTag Int)

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Abstract interpretation result          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(HptMap, getEnvVar, absFetch, addEnvVar, addEnvVars, getTags, getNodes, isBottom)

type HptMap        = (Array Int AbstractValue, Array Int AbstractValue, Map.Map Int AbstractValue)


getEnvVar :: HptMap -> Int -> AbstractValue
getEnvVar (ea,_,m) i  | snd (bounds ea) >= i = (ea ! i)
                      | otherwise            = Map.findWithDefault (AV_Error $ "variable "++ show i ++ " not found") i m
                         
getHeapLoc :: HptMap -> Int -> AbstractValue
getHeapLoc (_,ha,_) i = ha ! i  -- ahBaseSet (ha ! i)

absFetch :: HptMap -> HsName -> AbstractValue
absFetch a (HNmNr i _) = case getEnvVar a i of
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
addEnvVar (e,h,fm) i v = (e,h, Map.insert i v fm)

addEnvVars :: HptMap -> [(Int, AbstractValue)] -> HptMap
addEnvVars (e,h,fm) l = (e,h, foldl (flip $ uncurry Map.insert) fm l)

%%]

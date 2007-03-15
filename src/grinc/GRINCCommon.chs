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
evalNm     = hsnEval
evalNr     = HNmNr 1 (Just evalNm)
applyNm    = hsnApply
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

%%[8 export(Location, Variable, AbstractValue(..), AbstractCall, AbstractCallList)
%%]

%%[8.AbstractValue

type Location = Int
type Variable = Int


data AbstractValue
  = AbsBottom
  | AbsBasic
  | AbsTags  (Set.Set GrTag)
  | AbsLocs  (Set.Set Location)
  | AbsNodes (Map.Map GrTag [AbstractValue])
  | AbsError !String
    deriving (Eq, Ord)


type AbstractCall
  = (Variable, [Maybe Variable])
  
type AbstractCallList
  = [AbstractCall]


instance Show AbstractValue where
    show av = case av of
                  AbsBottom   -> "BOT"
                  AbsBasic    -> "BAS"
                  AbsTags  ts -> "TAGS" ++ show (Set.elems ts)
                  AbsLocs  ls -> "LOCS" ++ show (Set.elems ls)
                  AbsNodes ns -> "NODS" ++ show (Map.assocs ns)
                  AbsError s  -> "ERR: " ++ s

instance Monoid AbstractValue where
    mempty                                  =  AbsBottom
    mappend  a                 AbsBottom    =  a
    mappend    AbsBottom    b               =  b
    mappend    AbsBasic        AbsBasic     =  AbsBasic
    mappend   (AbsTags  at)   (AbsTags  bt) =  AbsTags      (Set.union at bt)
    mappend   (AbsLocs  al)   (AbsLocs  bl) =  AbsLocs (Set.union al bl)
    mappend   (AbsNodes an)   (AbsNodes bn) =  AbsNodes     (Map.unionWith (zipWith mappend) an bn)
    mappend a@(AbsError _ ) _               =  a
    mappend _               b@(AbsError _ ) =  b
    mappend a               b               =  AbsError $ "Wrong variable usage: Location, node or basic value mixed" ++ show a ++ " / " ++ show b


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

%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Abstract interpretation constraints     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(Equation(..), Equations, HeapEquation(..), HeapEquations, EvalMap, ApplyMap )

data Equation
  = IsKnown               Variable  !AbstractValue
  | IsEqual               Variable  ![Variable]
  | IsEvaluation          Variable  Variable                   Variable
  | IsApplication  (Maybe Variable) [Variable]                 Variable
  | IsSelection           Variable  Variable GrTag Int
  | IsConstruction        Variable  GrTag [Maybe Variable]     (Maybe Variable)
    deriving (Show, Eq)

data HeapEquation
  = WillStore             Location  GrTag [Maybe Variable]     (Maybe Variable)
    deriving (Show, Eq)

type Equations     = [Equation]
type HeapEquations = [HeapEquation]

type EvalMap     = AssocL GrTag Int
type ApplyMap    = AssocL GrTag (Either GrTag Int)

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Abstract interpretation result          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(HptMap, getEnvVar, absFetch, addEnvVar, addEnvVars, getTags, getNodes, isBottom, showHptMap)

type HptMap        = (Array Int AbstractValue, Array Int AbstractValue, Map.Map Int AbstractValue)

showHptElem :: (Int,AbstractValue) -> String
showHptElem (n,v) = show n ++ ": " ++ show v

showHptMap :: HptMap -> String
showHptMap (ae, ah, aex)
  =  unlines (  ( "HEAP"
                : map showHptElem (assocs ah)
                )
             ++ ( "BASE ENVIRONMENT"
                : map showHptElem (assocs ae)
                )
             ++ ( "EXTENDED ENVIRONMENT"
                : map showHptElem (Map.toAscList aex)
                )
             )
     
     
getEnvVar :: HptMap -> Int -> AbstractValue
getEnvVar (ea,_,m) i  | snd (bounds ea) >= i = (ea ! i)
                      | otherwise            = Map.findWithDefault (AbsError $ "variable "++ show i ++ " not found") i m
                         
getHeapLoc :: HptMap -> Int -> AbstractValue
getHeapLoc (_,ha,_) i = ha ! i  -- ahBaseSet (ha ! i)

absFetch :: HptMap -> HsName -> AbstractValue
absFetch a (HNmNr i _) = case getEnvVar a i of
                             AbsLocs l -> mconcat $ map (getHeapLoc a) (Set.toList l)
                             AbsBottom     -> AbsNodes Map.empty
                             AbsError s     -> error $ "analysis error: " ++ s
                             AbsBasic       -> error $ "variable " ++ show i ++ " is a basic value"
                             AbsNodes _     -> error $ "variable " ++ show i ++ "is a node variable"
absFetch a x = error ("absFetch tried on " ++ show x)

getTags av = case av of
                 AbsTags  ts -> Set.toList ts
                 _           -> map fst (getNodes av)

getNodes av = case av of
                  AbsNodes n  -> Map.toAscList n
                  AbsBottom   -> []
                  AbsError s  -> error $ "analysis error: " ++  s
                  _           -> error $ "not a node: " ++ show av

isBottom av = case av of
                  AbsBottom   ->  True
                  AbsLocs  l  ->  Set.null l
                  AbsNodes n  ->  Map.null n
                  AbsError s  ->  error $ "analysis error: " ++ s
                  otherwise   ->  False

addEnvVar :: HptMap -> Int -> AbstractValue -> HptMap
addEnvVar (e,h,fm) i v = (e,h, Map.insert i v fm)

addEnvVars :: HptMap -> [(Int, AbstractValue)] -> HptMap
addEnvVars (e,h,fm) l = (e,h, foldl (flip $ uncurry Map.insert) fm l)

%%]

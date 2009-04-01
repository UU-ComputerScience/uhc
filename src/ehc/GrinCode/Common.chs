%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]
%%[(8 codegen grin) module {%{EH}GrinCode.Common}
%%]
%%[(8 codegen grin) import( qualified Data.Map as Map, qualified Data.Set as Set, Data.Array, Data.Monoid, Char(isDigit) )
%%]
%%[(8 codegen grin) import( {%{EH}Base.Common}, {%{EH}Base.Builtin} )
%%]
%%[(8 codegen grin) import( {%{EH}GrinCode} )
%%]
%%[(8 codegen grin) hs import(Debug.Trace)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Special names                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen grin) export(wildcardNm, wildcardNr, mainNr, getNr, throwTag, hsnMainFullProg, conName, evaluateNr, evaluateArgNr)

wildcardNm = HNm "_"
wildcardNr = HNmNr 0 (OrigLocal wildcardNm)

getNr :: HsName -> Int
getNr (HNmNr i _) = i
getNr (HNPos i)   = error $ "getNr tried on HNPos " ++ show i
getNr a           = error $ "getNr tried on " ++ show a

throwTag      =  GrTag_Fun (HNm "rethrow")

%%[[8
hsnMainFullProg = hsnMain
%%][99
hsnMainFullProg = hsnSuffix hsnMain "FullProg"
%%]]

mainNr     = HNmNr 1 (OrigFunc hsnMainFullProg)

evaluateNr    = HNmNr 3 (OrigFunc (HNm "evaluate"))
evaluateArgNr = HNmNr 5 (OrigNone)

%%]



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Abstract interpretation domain %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen grin) export(Location, Variable, AbstractLocs(..), AbstractNodes(..), AbstractValue(..), AbstractCall, AbstractCallList)
%%]

%%[(8 codegen grin).AbstractValue

type Location = Int
type Variable = Int



data AbstractLocs
  = Locs (Set.Set Location) (Maybe (Set.Set GrTag))
    deriving (Eq, Ord)

data AbstractNodes
  = Nodes (Map.Map GrTag [AbstractValue])
--  | Nodes2 (Map.Map GrTag [Set.Set Variable])
    deriving (Eq, Ord)

data AbstractValue
  = AbsBottom
  | AbsBasic
  | AbsTags  (Set.Set GrTag)
  | AbsLocs  AbstractLocs  -- (Set.Set Location) (Maybe (Set.Set GrTag))
  | AbsNodes AbstractNodes -- (Map.Map GrTag [AbstractValue])
  | AbsUnion (Map.Map GrTag  AbstractValue )
  | AbsError String
    deriving (Eq, Ord)


type AbstractCall
  = (Variable, [Maybe Variable])
  
type AbstractCallList
  = [AbstractCall]


instance Show AbstractLocs where
  show (Locs ls ml) = show (Set.elems ls) ++ show ml

instance Show AbstractNodes where
  show (Nodes ns) = show (Map.assocs ns)
--  show (Nodes2 ns) = show (Map.assocs ns)

instance Show AbstractValue where
    show av = case av of
                  AbsBottom   -> "BOT"
                  AbsBasic    -> "BAS"
                  AbsTags  ts -> "TAGS" ++ show (Set.elems ts)
                  AbsLocs  al -> "LOCS" ++ show al
                  AbsNodes an -> "NODS" ++ show an
                  AbsUnion xs -> "UNION" ++ show (Map.assocs xs)
                  AbsError s  -> "ERR: " ++ s


limitIntersect (Just a) (Just b) = Just (Set.intersection a b)
limitIntersect Nothing  b        = b
limitIntersect a        _        = a


instance Monoid AbstractLocs where
   mempty = Locs Set.empty Nothing
   mappend (Locs al am) (Locs bl bm) = let cl = Set.union al bl
                                       in  --if   Set.size cl < 5000
                                           --then 
                                                  Locs cl (limitIntersect am bm)
                                           -- else trace ("large set: " ++ show cl) AllLocs

instance Monoid AbstractNodes where
   mempty = Nodes Map.empty
   mappend (Nodes  an) (Nodes  bn) = Nodes (Map.unionWith (zipWith mappend)   an bn)
--   mappend (Nodes2 an) (Nodes2 bn) = Nodes (Map.unionWith (zipWith Set.union) an bn)


instance Monoid AbstractValue where
    mempty                                  =  AbsBottom
    mappend  a                 AbsBottom    =  a
    mappend    AbsBottom    b               =  b
    mappend    AbsBasic        AbsBasic     =  AbsBasic
    mappend   (AbsTags  at)   (AbsTags  bt) =  AbsTags      (Set.union at bt)
    mappend   (AbsLocs  al)   (AbsLocs  bl) =  AbsLocs      (mappend al bl)
    mappend   (AbsNodes an)   (AbsNodes bn) =  AbsNodes     (mappend an bn)
    mappend   (AbsUnion am)   (AbsUnion bm) =  AbsUnion     (Map.unionWith          mappend  am bm)
    mappend a@(AbsError _ ) _               =  a
    mappend _               b@(AbsError _ ) =  b
    mappend a               b               =  AbsError $ "Wrong variable usage: Location, node or basic value mixed" ++ show a ++ " / " ++ show b


-- (Ord GrTag) is needed for (Ord AbstractValue) which is needed for Map.unionWith in mergeNodes


conNumber :: GrTag -> Int
-- Final tags first
conNumber (GrTag_Con _ _ _) = 1
conNumber (GrTag_PApp _ _)  = 2
conNumber GrTag_Rec         = 3
conNumber GrTag_Unboxed     = 4
conNumber GrTag_World       = 5
conNumber GrTag_Any         = 6
-- "Hole" separates final tags from unevaluated tags (this fact is exploited Grin2Silly, for generating code for Reenter alternatives)
conNumber GrTag_Hole        = 7
-- Unevaluated tags last
conNumber (GrTag_Fun _)     = 8
conNumber (GrTag_App _)     = 9


conName :: GrTag -> HsName
conName (GrTag_App nm) = nm
conName (GrTag_Fun nm) = nm
conName (GrTag_PApp _ nm) = nm
conName (GrTag_Con _ _ nm) = nm

conInt :: GrTag -> Int
conInt (GrTag_PApp i _ ) = i
conInt (GrTag_Con _ i _ ) = i

instance Ord GrTag where
  compare t1 t2 = let x = conNumber t1
                      y = conNumber t2
                  in  case compare x y of 
                        EQ -> if  x >= 3 && x <= 7
                              then -- Rec/Unboxed/World/Any/Hole
                                   EQ
                              else -- App/Fun/PApp/Con, all have a name
                                   case compare (conName t1) (conName t2) of
                                     EQ -> if  x >= 8
                                           then -- App/Fun
                                                EQ
                                           else -- Papp/Con, both have an int
                                                compare (conInt t1) (conInt t2)
                                     a  -> a
                        a  -> a


%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Abstract interpretation constraints     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen grin) export(Equation(..), Equations, HeapEquation(..), HeapEquations, Limitation, Limitations, limitIntersect)

data Equation
  = IsKnown               Variable  AbstractValue
  | IsEqual               Variable  Variable
  | IsSelection           Variable  Variable Int GrTag
  | IsConstruction        Variable  GrTag [Maybe Variable]       (Maybe Variable)
  | IsEnumeration         Variable  Variable
  | IsEvaluation          Variable  Variable                     Variable
  | IsApplication         Variable  [Variable]                   Variable
    deriving (Show, Eq)

data HeapEquation
  = WillStore             Location  GrTag [Maybe Variable]
  | WillEquate            Location  Variable
    deriving (Show, Eq)

type Limitation
  = (Variable, [GrTag])

type Equations     = [Equation]
type HeapEquations = [HeapEquation]
type Limitations   = [Limitation]

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Abstract interpretation result          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen grin) export(HptMap, getBaseEnvList, getEnvVar, getHeapLoc, absFetch, addEnvElems, getEnvSize, getTags, getNodes, isBottom, showHptMap, isPAppTag, isFinalTag, isApplyTag, filterTaggedNodes, getApplyNodeVars)

type HptMap  = ( Array Int AbstractValue   -- env
               , Array Int AbstractValue   -- heap
               )

showHptElem :: (Int,AbstractValue) -> String
showHptElem (n,v) = show n ++ ": " ++ show v

showHptMap :: HptMap -> String
showHptMap (ae, ah)
  =  unlines (  ( "HEAP"
                : map showHptElem (assocs ah)
                )
             ++ ( "BASE ENVIRONMENT"
                : map showHptElem (assocs ae)
                )
             )
     

getBaseEnvList :: HptMap -> [(Int,AbstractValue)]
getBaseEnvList (ae,_) = assocs ae
     
getEnvVar :: HptMap -> Int -> AbstractValue
getEnvVar (ae,_) i  | snd (bounds ae) >= i = (ae ! i)
                    | otherwise            = trace ("variable "++ show i ++ " not found") AbsBottom   -- AbsError $ "variable "++ show i ++ " not found"
                         
getHeapLoc :: HptMap -> Int -> AbstractValue
getHeapLoc (_,ah) i = ah ! i


limit :: Maybe (Set.Set GrTag) -> AbstractValue -> AbstractValue
limit Nothing v = v
limit (Just tset) (AbsNodes (Nodes ns)) = AbsNodes (Nodes (Map.fromList (filter (validTag tset) (Map.toList ns))))
limit _ v = error ("limit applied to non-Node " ++ show v)

validTag ts (t@(GrTag_Con _ _ _) , _)  = Set.member t ts
validTag _  _                          = True


absFetch :: HptMap -> HsName -> AbstractValue
absFetch a (HNmNr i _) = case getEnvVar a i of
                             AbsLocs (Locs l m) -> mconcat $ map (limit m . getHeapLoc a) (Set.toList l)
                             AbsBottom     -> AbsNodes (Nodes Map.empty)
                             AbsError s     -> error $ "analysis error absFetch: " ++ show a ++ s
                             AbsBasic       -> error $ "variable " ++ show i ++ " is a basic value"
                             AbsNodes _     -> error $ "variable " ++ show i ++ " is a node variable"
absFetch a x = error ("absFetch tried on " ++ show x)

getTags av = case av of
                 AbsTags  ts -> Just (Set.toList ts)
                 AbsBottom   -> Nothing
                 _           -> Just (map fst (getNodes av))

getNodes av = case av of
                  AbsNodes (Nodes n)  -> Map.toAscList n
                  AbsBottom   -> []
                  AbsError s  -> error $ "analysis error getNodes: " ++  s
                  _           -> error $ "not a node: " ++ show av

isBottom av = case av of
                  AbsBottom   ->  True
                  AbsLocs (Locs l m) ->  Set.null l
                  AbsNodes n  ->  False -- Map.null n
                  AbsError s  ->  error $ "analysis error isBottom: " ++ s
                  otherwise   ->  False

addEnvElems :: HptMap -> [AbstractValue] -> HptMap
addEnvElems (e,h) vs
  =  let (low, high) = bounds e
         extra = length vs 
         e2 = listArray (low, high+extra) (elems e ++ vs)
     in (e2,h)

getEnvSize :: HptMap -> Int
getEnvSize (e,h) = let (low,high) = bounds e
                   in  high-low+1

isPAppTag :: GrTag -> Bool
isPAppTag (GrTag_PApp _ _) = True
isPAppTag _                = False

isFinalTag :: GrTag -> Bool
isFinalTag  GrTag_Any        = True
isFinalTag  GrTag_Hole       = True
isFinalTag  GrTag_Unboxed    = True
isFinalTag (GrTag_PApp _ _)  = True
isFinalTag (GrTag_Con _ _ _) = True
isFinalTag _                 = False

isApplyTag (GrTag_App _)     = True
isApplyTag _                 = False


filterTaggedNodes :: (GrTag->Bool) -> AbstractValue -> AbstractValue
filterTaggedNodes p (AbsNodes (Nodes nodes)) = let newNodes = Map.filterWithKey (const . p) nodes
                                               in -- if Map.null newNodes then AbsBottom else 
                                                  AbsNodes (Nodes newNodes)
filterTaggedNodes p av               = av


getApplyNodeVars :: AbstractValue -> [ Variable ]
getApplyNodeVars (AbsNodes (Nodes nodes)) = [ getNr nm  | (GrTag_App nm) <- Map.keys nodes ]
getApplyNodeVars _                = []

%%]

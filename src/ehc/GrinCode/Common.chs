%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]
%%[(8 codegen grin) module {%{EH}GrinCode.Common}
%%]
%%[(8 codegen grin) import( qualified Data.Map as Map, qualified Data.Set as Set, Data.Array, Data.Monoid, Char(isDigit), Control.Monad )
%%]
%%[(8 codegen grin) import( {%{EH}Base.Common}, {%{EH}Base.Builtin} )
%%]
%%[(8 codegen grin) import( {%{EH}GrinCode} )
%%]
%%[(8 codegen grin) hs import(Debug.Trace)
%%]
%%[(8 codegen grin) hs import(Data.Typeable(Typeable), Data.Generics(Data), {%{EH}Base.Binary}, {%{EH}Base.Serialize}, Control.Monad (ap))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Special names                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen grin) export(wildcardNm, wildcardNr, mainNr, getNr, throwTag, hsnMainFullProg, conName, evaluateNr, evaluateArgNr)

wildcardNm = hsnFromString "_"
wildcardNr = HNmNr 0 (OrigLocal wildcardNm)

getNr :: HsName -> Int
getNr (HNmNr i _)      = i
getNr (HsName_Pos i)   = error $ "getNr tried on HNPos " ++ show i
getNr a                = error $ "getNr tried on " ++ show a

throwTag      =  GrTag_Fun (hsnFromString "rethrow")

%%[[8
hsnMainFullProg = hsnPrefix "fun0~" hsnMain
%%][99
hsnMainFullProg = hsnSuffix hsnMain "FullProg" -- should be: hsnUniqifyStr HsNameUniqifier_New "FullProg" hsnMain
%%]]

mainNr     = HNmNr 1 (OrigFunc hsnMainFullProg)

evaluateNr    = HNmNr 3 (OrigFunc (hsnFromString "evaluate"))
evaluateArgNr = HNmNr 5 (OrigNone)

%%]


%%[(8 codegen grin) export(tagArity)

tagArity :: GrTag -> Map.Map Int Int -> Int
tagArity (GrTag_Fun       nm) arityMap = maybe (error ("Fun " ++ show nm ++ "not in aritymap " ++ show arityMap)) id        (Map.lookup (getNr nm) arityMap)
tagArity (GrTag_App       nm) arityMap = maybe (error ("App " ++ show nm ++ "not in aritymap " ++ show arityMap)) id        (Map.lookup (getNr nm) arityMap)
tagArity (GrTag_PApp n    nm) arityMap = maybe (error ("Pap " ++ show nm ++ "not in aritymap " ++ show arityMap)) (\x->x-n) (Map.lookup (getNr nm) arityMap)
tagArity (GrTag_Con ann _ nm) _        = gtannArity ann
tagArity  GrTag_Unboxed       _        = 1
tagArity  GrTag_Hole          _        = 0
tagArity t                    _        = error ("tagArity " ++ show t)

%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Abstract interpretation domain %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen grin) export(AbstractNodesG(..), AbstractValueG(..), AbstractCallG, AbstractCallListG, mapAbstractNodes, mapAbstractValue)
%%]
%%[(8 codegen grin) export(Variable, AbstractNodes, AbstractValue, AbstractCall, AbstractCallList)
%%]

%%[(8 codegen grin).AbstractValue

type Variable = Int

type AbstractNodes = AbstractNodesG Variable
data AbstractNodesG var
  = Nodes (Map.Map GrTag [Set.Set var])
    deriving (Eq, Ord, Data, Typeable)

instance (Ord var, Binary var) => Binary (AbstractNodesG var) where
  put (Nodes m) = put m
  get = liftM Nodes get

-- 'fmap' on AbstractNodesG; cannot be an instance of Functor because of the
-- Ord constraints.
mapAbstractNodes :: (Ord a, Ord b) => (a -> b) -> AbstractNodesG a -> AbstractNodesG b
mapAbstractNodes f (Nodes m) = Nodes (Map.map (map (Set.map f)) m)

type AbstractValue = AbstractValueG Variable
data AbstractValueG var
  = AbsBottom
  | AbsBasic
  | AbsImposs
  | AbsTags  (Set.Set GrTag)
  | AbsNodes (AbstractNodesG var)
  -- We allow for three different representation of pointers. They can't be mixed. The choice is made in SolveEqs.
  | AbsPtr   (AbstractNodesG var)                                             -- this is a direct representation
  | AbsPtr0  (AbstractNodesG var) (Set.Set var)                          -- the variables that stored this value for the first time is recorded
  | AbsPtr1  (AbstractNodesG var) (Set.Set var)                          -- the values of those variables are not joined anymore in the AbstractNodes, so all variables in the list should be queried when doing absFetch
  | AbsPtr2  (AbstractNodesG var) (Set.Set var) (Set.Set var)       -- this representation still doesn't work
  | AbsUnion (Map.Map GrTag  (AbstractValueG var) )
  | AbsError String
    deriving (Eq, Ord, Data, Typeable)

instance (Ord var, Binary var) => Binary (AbstractValueG var) where
  put absV = case absV of
    AbsBottom           -> do putWord8  0
    AbsBasic            -> do putWord8  1
    AbsImposs           -> do putWord8  2
    AbsTags s           -> do putWord8  3; put s
    AbsNodes ns         -> do putWord8  4; put ns
    AbsPtr   ns         -> do putWord8  5; put ns
    AbsPtr0  ns vs      -> do putWord8  6; put ns; put vs
    AbsPtr1  ns vs      -> do putWord8  7; put ns; put vs
    AbsPtr2  ns vs1 vs2 -> do putWord8  8; put ns; put vs1; put vs2
    AbsUnion  ts        -> do putWord8  9; put ts
    AbsError s          -> do putWord8 10; put s
  get = do
    t <- getWord8
    case t of
      0  -> return AbsBottom
      1  -> return AbsBasic
      2  -> return AbsImposs
      3  -> liftM AbsTags get
      4  -> liftM AbsNodes get
      5  -> liftM AbsPtr get
      6  -> liftM2 AbsPtr0 get get
      7  -> liftM2 AbsPtr1 get get
      8  -> liftM3 AbsPtr2 get get get
      9  -> liftM AbsUnion get
      10 -> liftM AbsError get

-- 'fmap' on AbstractValuesG; cannot be an instance of Functor because of the
-- Ord constraints.
mapAbstractValue :: (Ord a, Ord b) => (a -> b) -> AbstractValueG a -> AbstractValueG b
mapAbstractValue f absV = case absV of
    AbsBottom           -> AbsBottom
    AbsBasic            -> AbsBasic
    AbsImposs           -> AbsImposs
    AbsTags s           -> AbsTags s
    AbsNodes ns         -> AbsNodes (mANs ns)
    AbsPtr   ns         -> AbsPtr (mANs ns)
    AbsPtr0  ns vs      -> AbsPtr0 (mANs ns) (Set.map f vs)
    AbsPtr1  ns vs      -> AbsPtr1 (mANs ns) (Set.map f vs)
    AbsPtr2  ns vs1 vs2 -> AbsPtr2 (mANs ns) (Set.map f vs1) (Set.map f vs2)
    AbsUnion  ts        -> AbsUnion (Map.map mAV ts)
    AbsError s          -> AbsError s
  where mAV  = mapAbstractValue f
        mANs = mapAbstractNodes f


type AbstractCall = AbstractCallG Variable
type AbstractCallG var
  = (var, [Maybe var])

type AbstractCallList = AbstractCallListG Variable
type AbstractCallListG var
  = [AbstractCallG var]


instance (Show var, Ord var) => Show (AbstractNodesG var) where
  show (Nodes ns) = show (Map.assocs ns)

instance (Show var, Ord var) => Show (AbstractValueG var) where
    show av = case av of
                  AbsBottom   -> "BOT"
                  AbsImposs   -> "IMP"
                  AbsBasic    -> "BAS"
                  AbsTags  ts -> "TAGS" ++ show (Set.elems ts)
                  AbsNodes an -> "NODS" ++ show an
                  AbsPtr   an -> "PTR"  ++ show an
                  AbsPtr0  an vs -> "PTR0"  ++ show an  ++ show vs
                  AbsPtr1  an vs -> "PTR1"  ++ show an  ++ show vs
                  AbsPtr2  an vs1 vs2 -> "PTR2"  ++ show an  ++ show vs1 ++ show vs2
                  AbsUnion xs -> "UNION" ++ show (Map.assocs xs)
                  AbsError s  -> "ERR: " ++ s


limitIntersect (Just a) (Just b) = Just (Set.intersection a b)
limitIntersect Nothing  b        = b
limitIntersect a        _        = a


instance Ord var => Monoid (AbstractNodesG var) where
   mempty = Nodes Map.empty
   mappend (Nodes an) (Nodes bn) = Nodes (Map.unionWith (zipWith Set.union) an bn)

instance (Show var, Ord var) => Monoid (AbstractValueG var) where
    mempty                                  =  AbsBottom
    mappend  a                 AbsBottom    =  a
    mappend    AbsBottom    b               =  b
    mappend  a                 AbsImposs    =  a
    mappend    AbsImposs    b               =  b
    mappend    AbsBasic        AbsBasic     =  AbsBasic
    mappend   (AbsTags  at)   (AbsTags  bt) =  AbsTags      (Set.union at bt)
    mappend   (AbsNodes an)   (AbsNodes bn) =  AbsNodes     (mappend an bn)
    mappend   (AbsPtr   an)   (AbsPtr   bn) =  AbsPtr       (mappend an bn)
    mappend   (AbsPtr0  an vs)(AbsPtr0  bn ws)=  AbsPtr0    (mappend an bn) (Set.union vs ws)
    mappend   (AbsPtr1  an vs)(AbsPtr1  bn ws)=  AbsPtr1    (mappend an bn) (Set.union vs ws)
    mappend   (AbsPtr2  an vs vs2)(AbsPtr2  bn ws ws2)=  AbsPtr2    (mappend an bn) (Set.union vs ws) (Set.union vs2 ws2)
    mappend   (AbsUnion am)   (AbsUnion bm) =  AbsUnion     (Map.unionWith          mappend  am bm)
    mappend a@(AbsError _ ) _               =  a
    mappend _               b@(AbsError _ ) =  b
    mappend a               b               =  AbsError $ "Wrong variable usage: pointer, node or basic value mixed" ++ show a ++ " / " ++ show b


-- (Ord GrTag) is needed for (Ord AbstractValue) which is needed for Map.unionWith in mergeNodes


conNumber :: GrTag -> Int
-- Final tags first
conNumber (GrTag_Con _ _ _) = 1
conNumber (GrTag_PApp _ _)  = 2
conNumber GrTag_Rec         = 3
conNumber GrTag_Unboxed     = 4
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
                                   case cmpHsNameOnNm (conName t1) (conName t2) of
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

%%[(8 codegen grin) export(Equation, EquationG(..), Equations, EquationsG, Limitation, LimitationG, Limitations, LimitationsG, limitIntersect)

type Equation = EquationG Variable
data EquationG var
  = IsBasic               var
  | IsImpossible          var
  | IsTags                var  [GrTag]
  | IsPointer             var  GrTag [Maybe var]
  | IsConstruction        var  GrTag [Maybe var]       (Maybe var)
  | IsUpdate              var  var
  | IsEqual               var  var
  | IsSelection           var  var Int GrTag
  | IsEnumeration         var  var
  | IsEvaluation          var  var                     var
  | IsApplication         var  [var]                   var
    deriving (Show, Eq, Data, Typeable)

instance Binary var => Binary (EquationG var) where
  put eq = case eq of
    IsBasic        v              ->  do putWord8  0; put v
    IsImpossible   v              ->  do putWord8  1; put v
    IsTags         v  gs          ->  do putWord8  2; put v; put gs
    IsPointer      v  g   mvs     ->  do putWord8  3; put v; put g; put mvs
    IsConstruction v  g   mvs mv  ->  do putWord8  4; put v; put g; put mvs; put mv
    IsUpdate       v1 v2          ->  do putWord8  5; put v1; put v2
    IsEqual        v1 v2          ->  do putWord8  6; put v1; put v2
    IsSelection    v1 v2  i   g   ->  do putWord8  7; put v1; put v2; put i; put g
    IsEnumeration  v1 v2          ->  do putWord8  8; put v1; put v2
    IsEvaluation   v1 v2  v3      ->  do putWord8  9; put v1; put v2; put v3
    IsApplication  v1 vs  v2      ->  do putWord8 10; put v1; put vs; put v2
  get = do
    t <- getWord8
    case t of
      0  -> liftM IsBasic get
      1  -> liftM IsImpossible get
      2  -> liftM2 IsTags get get
      3  -> liftM3 IsPointer get get get
      4  -> liftM4 IsConstruction get get get get
      5  -> liftM2 IsUpdate get get
      6  -> liftM2 IsEqual get get
      7  -> liftM4 IsSelection get get get get
      8  -> liftM2 IsEnumeration get get
      9  -> liftM3 IsEvaluation get get get
      10 -> liftM3 IsApplication  get get get

instance Functor EquationG where
  fmap f eq = case eq of
    IsBasic        v              ->  IsBasic (f v)
    IsImpossible   v              ->  IsImpossible (f v)
    IsTags         v  gs          ->  IsTags (f v) gs
    IsPointer      v  g   mvs     ->  IsPointer (f v) g (fmap (fmap f) mvs)
    IsConstruction v  g   mvs mv  ->  IsConstruction (f v) g (fmap (fmap f) mvs) (fmap f mv)
    IsUpdate       v1 v2          ->  IsUpdate (f v1) (f v2)
    IsEqual        v1 v2          ->  IsEqual (f v1) (f v2)
    IsSelection    v1 v2  i   g   ->  IsSelection (f v1) (f v2) i g
    IsEnumeration  v1 v2          ->  IsEnumeration (f v1) (f v2)
    IsEvaluation   v1 v2  v3      ->  IsEvaluation (f v1) (f v2) (f v3)
    IsApplication  v1 vs  v2      ->  IsApplication (f v1) (fmap f vs) (f v2)

type Limitation = LimitationG Variable
type LimitationG var
  = (var, [GrTag])

type Equations    = [Equation]
type Limitations  = [Limitation]

type EquationsG   var = [EquationG var]
type LimitationsG var = [LimitationG var]

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Abstract interpretation result          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen grin) export(HptMap, PartialHptMap, getBaseEnvList, getEnvVar, absFetch, addEnvElems, getEnvSize, getTags, getNodes, isBottom, showHptMap, isPAppTag, isFinalTag, isApplyTag, filterTaggedNodes, getApplyNodeVars)

type HptMap  = Array Int AbstractValue

type PartialHptMap var = Map.Map var (AbstractValueG var)


showHptElem :: (Int,AbstractValue) -> String
showHptElem (n,v) = show n ++ ": " ++ show v

showHptMap :: HptMap -> String
showHptMap ae
  =  unlines (map showHptElem (assocs ae))

getBaseEnvList :: HptMap -> [(Int,AbstractValue)]
getBaseEnvList ae = assocs ae
     
getEnvVar :: HptMap -> Int -> AbstractValue
getEnvVar ae i  | snd (bounds ae) >= i = (ae ! i)
                | otherwise            = trace ("variable "++ show i ++ " not found") AbsBottom   -- AbsError $ "variable "++ show i ++ " not found"
                         

limit :: (Show var, Ord var) => Maybe (Set.Set GrTag) -> AbstractValueG var -> AbstractValueG var
limit Nothing v = v
limit (Just tset) (AbsNodes (Nodes ns)) = AbsNodes (Nodes (Map.fromList (filter (validTag tset) (Map.toList ns))))
limit _ v = error ("limit applied to non-Node " ++ show v)

validTag ts (t@(GrTag_Con _ _ _) , _)  = Set.member t ts
validTag _  _                          = True



absFetchDirect :: HptMap -> Variable -> AbstractValue
absFetchDirect a i  = case getEnvVar a i of
                        AbsPtr1 an vs   -> AbsNodes an
                        AbsPtr2 an vs ws-> mconcat (AbsNodes an :  map (getEnvVar a) (Set.toList ws))
                        AbsBottom    -> AbsNodes (Nodes Map.empty)
                        av           -> error ("AbsFetchDirect i=" ++ show i ++ " av=" ++ show av)


absFetch :: HptMap -> HsName -> AbstractValue
absFetch a (HNmNr i _) = case getEnvVar a i of
                             AbsPtr  an       -> AbsNodes an
                             AbsPtr0 an vs    -> AbsNodes an
                             AbsPtr1 an vs    -> mconcat (AbsNodes an :  map (absFetchDirect a) (Set.toList vs))
                             AbsPtr2 an vs ws -> mconcat (AbsNodes an :  map (absFetchDirect a) (Set.toList vs) ++ map (getEnvVar a) (Set.toList ws))
                             AbsBottom     -> AbsNodes (Nodes Map.empty)
                             AbsError s     -> error $ "analysis error absFetch: " ++ show a ++ s
                             AbsBasic       -> error $ "variable " ++ show i ++ " is a basic value"
                             AbsNodes _     -> error $ "variable " ++ show i ++ " is a node variable"
absFetch a x = error ("absFetch tried on " ++ show x)

getTags av = case av of
                 AbsTags  ts -> Just (Set.toList ts)
                 AbsBottom   -> Nothing
                 AbsNodes (Nodes n)  -> Just (map fst (Map.toAscList n))

getNodes av = case av of
                 AbsNodes (Nodes n)  -> Map.toAscList n
                 AbsBottom   -> []
                 AbsImposs   -> []
                 AbsError s  -> error $ "analysis error getNodes2: " ++  s
                 _           -> error $ "not a node2: " ++ show av

isBottom av = case av of
                  AbsBottom   ->  True
                  AbsNodes n  ->  False -- Map.null n
                  AbsError s  ->  error $ "analysis error isBottom: " ++ s
                  otherwise   ->  False

addEnvElems :: HptMap -> [AbstractValue] -> HptMap
addEnvElems e vs
  =  let (low, high) = bounds e
         extra = length vs 
         e2    = listArray (low, high+extra) (elems e ++ vs)
     in e2

getEnvSize :: HptMap -> Int
getEnvSize e  = let (low,high) = bounds e
                in  high-low+1

isPAppTag :: GrTag -> Bool
isPAppTag (GrTag_PApp _ _) = True
isPAppTag _                = False

isFinalTag :: GrTag -> Bool
isFinalTag  GrTag_Hole       = False
isFinalTag  GrTag_Unboxed    = True
isFinalTag (GrTag_PApp _ _)  = True
isFinalTag (GrTag_Con _ _ _) = True
isFinalTag _                 = False

isApplyTag (GrTag_App _)     = True
isApplyTag _                 = False


filterTaggedNodes :: (GrTag->Bool) -> AbstractValueG var -> AbstractValueG var
filterTaggedNodes p (AbsNodes (Nodes nodes)) = let newNodes = Map.filterWithKey (const . p) nodes
                                               in AbsNodes (Nodes newNodes)
filterTaggedNodes p av               = av

getApplyNodeVars :: AbstractValue -> [ Variable ]
getApplyNodeVars (AbsNodes (Nodes nodes)) = [ getNr nm  | (GrTag_App nm) <- Map.keys nodes ]
getApplyNodeVars _                = []

%%]

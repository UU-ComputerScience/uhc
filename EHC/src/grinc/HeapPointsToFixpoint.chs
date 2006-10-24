%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

We want to have two mappings. The eval mapping and the apply mapping.

The eval mapping consist of a mapping from Tag (with arity information) to a Grin function, or the keyword unit
The apply mapping consist of a mapping from  tag to tag or to a grin function

examples:
eval:
FInt(1) -> unit
Fupto(2) -> upto
Fsum(2)  -> sum

apply:
Pupto_2(0) -> Pupto_1
Pupto_1(1) -> upto

Now we retrieve the equations and initial environment and heap:

- Every variable gets a number
- The equations are represented by a base set a change set and an equation part

On every iteration we merge the changeSet with the BaseSet and define the new
changeSet based on the previous values. This is only possible if we update all equations each iteration.

Note: we can implement the heap and environment as an array. With constant
lookup times.

TODO: Shared set and Unique set instead base and shared part

%%[8 module {%{GRIN}HeapPointsToFixpoint} import(Data.Maybe, Data.List, Data.Ix, Data.Monoid, Data.Array.ST, Data.Array.IArray, Control.Monad.ST, Control.Monad, {%{EH}Base.Common}, {%{EH}GrinCode})
%%]

%%[8  import(UU.Pretty, {%{EH}GrinCode.Pretty}) export("module Data.Monoid")
%%]


%%[8.OrdTag
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
%%]

%%[8.imports import(qualified Data.Set as Set, qualified Data.Map as Map)
%%]

%%[8.AbstractValue import(Data.List) export(AbstractValue(..), AbstractNode, Location, Variable)
data AbstractValue
  = AV_Nothing
  | AV_Basic
  | AV_Locations (Set.Set Location) -- TODO: use Data.Set Location
  | AV_Nodes (Map.Map GrTag [AbstractValue]) -- TODO: use Data.Map GrTag [AbstractValue]
  | AV_Tags  (Set.Set GrTag) -- TODO: use Data.Set GrTag
  | AV_Error !String
    deriving (Eq, Ord)

instance Show AbstractValue where
    show av = case av of
                  AV_Nothing      -> "bot"
                  AV_Basic        -> "bas"
                  AV_Locations ls -> "{" ++ show ls ++ "}"
                  AV_Nodes     ns -> "{" ++ show ns ++ "}"
                  AV_Tags      ts -> "{" ++ show ts ++ "}"
                  AV_Error     s  -> "E: " ++ s

type AbstractNode = (GrTag, [AbstractValue]) -- an AV_Nodes can not occur inside an AbstractNode
type Location = Int

type Variable = Int

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
%%]

%%[8.Heap export(AbstractHeap, AbstractHeapElement(..), AbstractHeapModifier, AbstractNodeModifier)
type AbstractHeap s = STArray s Location AbstractHeapElement
data AbstractHeapElement = AbstractHeapElement
    { ahBaseSet    ::  !AbstractValue
    , ahSharedSet  ::  !(Maybe AbstractValue)
    , ahMod        ::  !AbstractHeapModifier
    }
    deriving (Eq)

-- TODO: which should ahSharedSet hold: <value when shared> - <value when uniq> or <value when shared>
-- Note: ahSharedSet currently holds the former, Nothing means it the cell shared, Just means unique (and shared part is kept off the record)

instance Show AbstractHeapElement where
    show (AbstractHeapElement b s m) =    "unique = "       ++ show b
                                       ++ ";\tshared = "  ++ show s
                                       ++ ";\tmod = "     ++ show m

type AbstractHeapModifier = (AbstractNodeModifier, Maybe Variable)
type AbstractNodeModifier = (GrTag, [Maybe Variable]) --(tag, [fields])

updateHeapElement :: AbstractHeapElement -> AbstractEnv s -> ST s AbstractHeapElement
updateHeapElement he env = do
    { (baseChange, sharedChange) <- heapChangeSet (ahMod he) env
    ; let  sharedSet       =  ahSharedSet he
           newBaseSet'     =  baseChange   `mappend` ahBaseSet he
           newBaseSet      =  maybe (sharedChange `mappend` newBaseSet') (const newBaseSet') sharedSet
           newSharedSet    =  maybe Nothing (Just . mappend sharedChange) sharedSet
    ; return (he { ahBaseSet = newBaseSet, ahSharedSet = newSharedSet })
    }
%%]

%%[8.Environment export(AbstractEnv, AbstractEnvElement(..), AbstractEnvModifier(..))
type AbstractEnv s = STArray s Variable AbstractEnvElement
data AbstractEnvElement = AbstractEnvElement
    { aeBaseSet   :: !AbstractValue
    , aeIsShared  :: !Bool
    , aeMod       :: !AbstractEnvModifier
    }
    deriving (Eq)

instance Show AbstractEnvElement where
    show (AbstractEnvElement b s m) =  "base = " ++ show b
                                       ++ ";\tshared = " ++ show s
                                       ++ ";\tmod = " ++ show m

data AbstractEnvModifier
  = EnvSetAV !AbstractValue
  | EnvUnion ![Variable] (Maybe AbstractEnvModifier) -- The Maybe contains only EnvSelect which is used for the apply function calls
  | EnvEval Variable Variable
  | EnvApp Variable [ApplyArg] Variable
  | EnvSelect Variable GrTag Int
  | EnvTag GrTag [Maybe Variable] (Maybe Variable)
    deriving (Show, Eq)

type ApplyArg = Either Variable AbstractEnvModifier -- only contains the EnvTag here

updateEnvElement :: AbstractEnvElement -> AbstractEnv s -> AbstractHeap s -> AssocL GrTag (Either GrTag Int) -> ST s AbstractEnvElement
updateEnvElement ee env heap applyMap = do
    { newChangeSet <- envChangeSet (aeMod ee) env heap applyMap
    ; let newBaseSet = newChangeSet `mappend` aeBaseSet ee
    ; return (ee { aeBaseSet = newBaseSet })
    }
%%]

%%[8.sharingAnalysis
addSharingInfo :: AbstractEnv s -> AbstractHeap s -> ST s ()
addSharingInfo env heap = getElems env >>= mapM_ (setSharingInfo heap)

setSharingInfo heap v = case aeBaseSet v of
                           AV_Locations ls  ->  when (aeIsShared v) (mapM_ (setShared heap) (Set.toList ls))
                           otherwise        ->  return ()

setShared heap l = do
    { he <- lookupHeap heap l
    ; maybe (return ())  (\s -> do { let  baseSet  =  ahBaseSet he `mappend` s
                                          newElem  =  he { ahBaseSet = baseSet
                                                         , ahSharedSet = Nothing
                                                         }
                                   ; writeArray heap l newElem
                                   }
                         )
                         (ahSharedSet he)
    }
%%]

%%[8.heapChangeSet
heapChangeSet :: AbstractHeapModifier -> AbstractEnv s -> ST s (AbstractValue, AbstractValue)
heapChangeSet ((tag, deps), resultDep) env = do
    { locs        <- mapM getBaseSet deps
    ; resCS       <- getBaseSet resultDep
    ; exceptPtrs  <- getBaseSet (resultDep >>= return . (+1))
    ; let exceptNode  =  case exceptPtrs of
                            AV_Nothing                   -> AV_Nothing
                            AV_Locations l | Set.null l  -> AV_Nothing
                            otherwise                    -> AV_Nodes (Map.singleton throwTag [AV_Basic, exceptPtrs])
          uniqueAV    =  AV_Nodes (Map.singleton tag locs)
          sharedAV    =  exceptNode `mappend` resCS
    ; return (uniqueAV, sharedAV)
    }
    where
    throwTag      =  GrTag_Lit GrTagFun 0 (HNm "rethrow")
--    blackholeTag  =  GrTag_Lit GrTagHole 0 (HNm "blackhole")
    getBaseSet    =  maybe (return AV_Nothing) (\v -> lookupEnv env v >>= return . aeBaseSet)
%%]

%%[8.envChangeSet

envChangeSet :: AbstractEnvModifier -> AbstractEnv s -> AbstractHeap s -> AssocL GrTag (Either GrTag Int) -> ST s AbstractValue
envChangeSet am env heap applyMap = case am of
                                        EnvSetAV    av     -> return av
                                        EnvUnion    vs mbS -> let addApplyArgument s av = envChangeSet s env heap applyMap >>= return . flip mappend av
                                                              in mapM valAbsEnv vs >>= return . mconcat >>= maybe return addApplyArgument mbS
                                        EnvEval     v ev     -> valAbsEnv v >>= evalChangeSet ev
                                        EnvApp      f a ev   -> do { pnodes  <- valAbsEnv f
                                                                   ; argsVal <- mapM (\(Left v) -> valAbsEnv v) a
                                                                   ; applyChangeSet pnodes argsVal ev
                                                                   }
                                        EnvSelect   v n i  -> valAbsEnv v >>= return . selectChangeSet n i
                                        EnvTag      t f r  -> tagChangeSet t f r
    where
    --valAbsEnv :: Variable -> ST s AbstractValue
    valAbsEnv v = do
        { elem <- lookupEnv env v
        ; return (aeBaseSet elem)
        }
    --valAbsHeap :: Location -> ST s (AbstractValue, AbstractValue)
    valAbsHeap l = do
        { elem <- lookupHeap heap l
        ; let resultVar = snd (ahMod elem)
        ; exceptions <- maybe (return Nothing) (\v -> valAbsEnv (v+1) >>= return . Just) resultVar
        ; return (ahBaseSet elem `mappend` maybe AV_Nothing id (ahSharedSet elem), exceptions)
        }
    evalFilter (AV_Nodes nodes) = let isValueTag t = case t of
                                                         GrTag_Any          -> True
                                                         GrTag_Unboxed      -> True
                                                         GrTag_Lit cat _ _  -> case cat of
                                                                                   GrTagCon     -> True
                                                                                   GrTagPApp _  -> True
                                                                                   GrTagApp     -> False
                                                                                   GrTagFun     -> False
                                                                                   GrTagHole    -> False
                                                                                   GrTagRec     -> False
                                                         GrTag_Var _        -> error $ "tag variable unexpected: " ++ show t
                                      isValueNode t f   = isValueTag t
                                      newNodes          = Map.filterWithKey isValueNode nodes
                                  in if Map.null newNodes then AV_Nothing else AV_Nodes newNodes
    evalFilter av               = av
    --evalChangeSet :: AbstractValue -> ST s AbstractValue
    evalChangeSet exceptVar av = case av of
                                   AV_Nothing      -> return av
                                   AV_Locations ls -> do { res <- mapM valAbsHeap (Set.toList ls)
                                                         ; let (vs,es)  = unzip res
                                                               v        = mconcat (map evalFilter vs)
                                                               e        = mconcat [ x | (Just x) <- es ]
                                                         ; appendExceptions env exceptVar e
                                                         ; return v
                                                         }
                                   AV_Error _      -> return av
                                   otherwise       -> return $ AV_Error "Variable passed to eval is not a location"
    selectChangeSet :: GrTag -> Int -> AbstractValue -> AbstractValue
    selectChangeSet nm idx av = case av of
                                  AV_Nothing    -> av
                                  AV_Nodes   ns -> maybe AV_Nothing (!! idx) (Map.lookup nm ns)
                                  AV_Error _    -> av
                                  otherwise     -> AV_Error "Variable passed to eval is not a node"
    --tagChangeSet :: GrTag -> [Maybe Variable] -> (Maybe Variable) -> ST s AbstractValue
    tagChangeSet t flds r = do { vars <- mapM (maybe (return AV_Basic) valAbsEnv) flds
                               ; let newNodes = AV_Nodes (Map.singleton t vars)
                               ; maybe (return newNodes) (\v -> valAbsEnv v >>= return . mappend newNodes) r
                               }
    --applyChangeSet :: AbstractValue -> [AbstractValue] -> ST s AbstractValue
    applyChangeSet f argsVal exceptVar = foldM applyChangeSet1 f argsVal
        where
        --applyChangeSet1 :: AbstractValue -> AbstractValue -> ST s AbstractValue
        applyChangeSet1 f arg = let partialApplicationNodes = [ node | node@((GrTag_Lit (GrTagPApp _) _ _), _) <- getNodes f ]
                                    --getNewNode :: GrTag -> [AbstractValue] -> ST s AbstractValue
                                    getNewNode tag args       = let newArgs = args ++ [arg]
                                                                    partialF tag' = return $ AV_Nodes (Map.singleton tag' newArgs)
                                                                    saturatedF var =
                                                                        do { appendApplyArg env (AV_Nodes (Map.singleton (GrTag_Var (HNPos var))
                                                                                                                         newArgs
                                                                                                          )
                                                                                                )
                                                                           ; e <- valAbsEnv (var+1)
                                                                           ; appendExceptions env exceptVar e
                                                                           ; valAbsEnv var
                                                                           }
                                                                in either partialF saturatedF
                                                                          (fromJust' ("tag missing in applyMap: " ++ show tag) $ lookup tag applyMap)
                                in mapM (uncurry getNewNode) partialApplicationNodes >>= return . mconcat
%%]

%%[8 ghc (6.6,_)
abstractBounds :: Ix i => STArray s i a -> ST s (i, i)
abstractBounds = getBounds

abstractIndices :: Ix i => STArray s i a -> ST s [i]
abstractIndices a = getBounds a >>= return . range
%%]
%%[8 ghc (_,6.4.2)
abstractBounds :: Ix i => STArray s i a -> ST s (i, i)
abstractBounds = return . bounds

abstractIndices :: Ix i => STArray s i a -> ST s [i]
abstractIndices = return . indices
%%]

%%[8 import(Data.Either) export(lookupEnv)
fromLeft  = either id                                     (const $ error "fromLeft: found right value")
fromRight = either (const $ error "fromRight: found left value") id

fromJust' s Nothing  = error $ "fromJust' Maybe:" ++ s
fromJust' _ (Just a) = a
--to break circular dependencies a copy paste from {%{GRIN}GRINCCommon}:
getNodes av = case av of
                  AV_Nodes n  -> Map.toList n
                  AV_Nothing  -> []
                  AV_Error s  -> error $ "analysis error: " ++  s
                  _           -> error $ "not a node: " ++ show av


lookupEnv :: AbstractEnv s -> Variable -> ST s AbstractEnvElement
lookupEnv env idx = readArray env idx

lookupHeap :: AbstractHeap s -> Location -> ST s AbstractHeapElement
lookupHeap heap idx = readArray heap idx

appendApplyArg :: AbstractEnv s -> AbstractValue -> ST s ()
appendApplyArg env av = do { (applyArgIdx,_) <- abstractBounds env
                           ; elem <- readArray env applyArgIdx
                           ; let applyArg = aeBaseSet elem
                                 newElem  = elem { aeBaseSet = av `mappend` applyArg }
                           ; writeArray env applyArgIdx newElem
                           }

appendExceptions :: AbstractEnv s -> Variable -> AbstractValue -> ST s ()
appendExceptions env handlerVar av = do { elem <- readArray env handlerVar
                                        ; let exceptions = aeBaseSet elem
                                              newElem  = elem { aeBaseSet = av `mappend` exceptions }
                                        ; writeArray env handlerVar newElem
                                        }
%%]

%%[8.partialOrder
a <=! b = all (flip elem b) a
%%]

%%[8.fixpoint export(Label)
type Label       = Either Variable Location
type WorkList    = [Label]
type Depends     = (Label -> [Label])
type Analysis  s = (AbstractEnv s, AbstractHeap s)

{-
fixpoint :: Analysis s -> Depends -> WorkList -> (Label -> Analysis s -> ST s (Analysis s, Bool)) -> ST s (Analysis s)
fixpoint base deps []              f = return base
fixpoint base deps (work:worklist) f = do
    { (step, changed) <- f work base
    ; if changed
      then fixpoint step deps (deps work ++ worklist) f
      else fixpoint base deps worklist f
    }
-}

fixpoint labels f = countFixpoint 1
    where
    countFixpoint count = do
        { let doStep hasChanges l = f l >>= return . (hasChanges ||)
        ; changes <- foldM doStep False labels
        ; if changes
          then countFixpoint (count+1)
          else return count
        }
%%]

%%[8 export(heapPointsTo)
heapPointsTo :: AbstractEnv s -> AbstractHeap s -> AssocL GrTag (Either GrTag Int) -> ST s Int
heapPointsTo env heap applyMap =
    do { indHeap <- abstractIndices heap
       ; indEnv  <- abstractIndices env
       ; let { labels        = heapLabels ++ envLabels
             ; heapLabels    = map Right indHeap
             ; envLabels     = map Left (tail $ indEnv)
             ; f (Left i) = do
                 { e  <- lookupEnv env i
                 ; e' <- updateEnvElement e env heap applyMap
                 ; let changed = isChanged (aeBaseSet e) (aeBaseSet e')
                 ; when changed (writeArray env i e')
                 ; return changed
                 }
             ; f (Right i) = do
                { e  <- lookupHeap heap i
                ; e' <- updateHeapElement e env
                ; let changed = isChanged (ahBaseSet e) (ahBaseSet e') || isChanged (ahSharedSet e) (ahSharedSet e')
                ; when changed (writeArray heap i e')
                ; return changed
                }
             }
        -- @tracef l a = do { r@((env,heap), c) <- f l a
        --                ; msg <- if c then either (\i -> lookupEnv env i >>= return . show) (\i -> lookupHeap heap i >>= return . show) l else return "nothing"
        --                ; trace ("step: " ++ show l ++ " changed: " ++ show msg) $ return r
        --                }
       ; count <- fixpoint labels f
       ; addSharingInfo env heap
       ; return count
       }

--AbstractValue -> AbstractValue -> Bool
isChanged :: Eq a => a -> a -> Bool
isChanged old new = old /= new
                    --case (old, new) of
                    --  (AV_Locations ol, AV_Locations nl) -> not $ nl <=! ol
                    --  (AV_Nodes     on, AV_Nodes nn    ) -> not $ nn <=! on
                    --  otherwise                          -> old /= new
%%]

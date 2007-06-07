%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Force evaluation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 module {%{EH}Base.ForceEval}
%%]

%%[99 import(qualified Data.Map as Map, qualified Data.Set as Set, Data.List)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Class interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(ForceEval(..))
class ForceEval a where
  forceEval :: a -> a
  forceEval x | x `seq` True = x
%%]

%%[99 export(forceEval')
forceEval' :: ForceEval a => a -> ()
forceEval' x | forceEval x `seq` True = ()
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
fseq :: a -> a -> a
fseq = seq
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
instance ForceEval Int
instance ForceEval Integer
instance ForceEval Char
instance ForceEval ()

instance (ForceEval a) => ForceEval (Maybe a) where
  forceEval j@(Just x) | forceEval x `seq` True = j
  forceEval Nothing                             = Nothing
  -- forceEval x = fmap forceEval x `seq` x

instance (ForceEval a,ForceEval b) => ForceEval (a,b) where
  forceEval x@(a,b) | forceEval a `seq` forceEval b `seq` True = x

instance ForceEval a => ForceEval [a] where
  forceEval [] = []
  forceEval l@(x:xs) | forceEval x `seq` forceEval xs `seq` True = l
  -- forceEval x = foldl' (\l e -> forceEval e `seq` l) () x `seq` x

instance (Ord a, ForceEval a) => ForceEval (Set.Set a) where
  forceEval x | forceEval (Set.toList x) `seq` True = x

instance (ForceEval k, ForceEval v) => ForceEval (Map.Map k v) where
  forceEval x | forceEval (Map.toList x) `seq` True = x
%%]
instance (Ord a, ForceEval a) => ForceEval (Set.Set a) where
  forceEval x = Set.map forceEval x `seq` x

instance (ForceEval k, ForceEval v) => ForceEval (Map.Map k v) where
  forceEval x = Map.mapWithKey (\(k::k) (v::v) -> forceEval k `seq` forceEval v `seq` ()) `seq` x

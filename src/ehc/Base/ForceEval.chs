%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Force evaluation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 module {%{EH}Base.ForceEval}
%%]

%%[99 import(qualified Data.Map as Map, qualified Data.Set as Set, Data.List)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Counting
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[101 export(CountMp,emptyCM,cm1,cmUnion,cmUnions)
type CountMp = Map.Map String Int

cmN :: String -> Int -> CountMp
cmN s n = Map.singleton s n

cm1 :: String -> CountMp
cm1 s = cmN s 1

emptyCM :: CountMp
emptyCM = cm1 "??"

cmUnion :: CountMp -> CountMp -> CountMp
cmUnion = Map.unionWith (+)

cmUnions :: [CountMp] -> CountMp
cmUnions = Map.unionsWith (+)
%%]

%%[101 hs export(cmShow)
cmTotal :: CountMp -> Int
cmTotal m = sum $ Map.elems m

cmShow :: CountMp -> String
cmShow m = (show $ cmTotal m) ++ ": " ++ show m
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Class interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(ForceEval(..))
class ForceEval a where
  forceEval :: a -> a
  forceEval x | x `seq` True = x
%%[[101
  fevCount :: a -> CountMp
  fevCount x | x `seq` True = emptyCM
  fevSize :: a -> Int
  fevSize x = cmTotal $ fevCount x
%%]]
%%]

%%[99 export(forceEval')
forceEval' :: ForceEval a => a -> ()
forceEval' x | forceEval x `seq` True = ()
%%]

%%[101 export(fevShow)
fevShow :: ForceEval a => String -> a -> String
fevShow m x = m ++ ": " ++ cmShow (fevCount x)
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
instance ForceEval Bool
%%[[101
  where
    fevCount x | x `seq` True = cm1 "Bool"
%%]]

instance ForceEval Int
%%[[101
  where
    fevCount x | x `seq` True = cm1 "Int"
%%]]

instance ForceEval Integer
%%[[101
  where
    fevCount x | x `seq` True = cm1 "Integer"
%%]]

instance ForceEval Char
%%[[101
  where
    fevCount x | x `seq` True = cm1 "Char"
%%]]

instance ForceEval ()
%%[[101
  where
    fevCount x | x `seq` True = cm1 "()"
%%]]

instance (ForceEval a) => ForceEval (Maybe a) where
  forceEval j@(Just x) | forceEval x `seq` True = j
  forceEval Nothing                             = Nothing
%%[[101
  fevCount (Just x) = cmUnions [cm1 "Just",fevCount x]
  fevCount Nothing  = cm1 "Nothing"
%%]]

instance (ForceEval a,ForceEval b) => ForceEval (a,b) where
  forceEval x@(a,b) | forceEval a `seq` forceEval b `seq` True = x
%%[[101
  fevCount (a,b) = cmUnions [fevCount a,fevCount b]
%%]]

instance ForceEval a => ForceEval [a] where
  forceEval [] = []
  forceEval l@(x:xs) | forceEval x `seq` forceEval xs `seq` True = l
  -- forceEval x = foldl' (\l e -> forceEval e `seq` l) () x `seq` x
%%[[101
  fevCount x = cmUnions (cmN ":" (length x) : cm1 "[]" : map fevCount x)
%%]]

instance (Ord a, ForceEval a) => ForceEval (Set.Set a) where
  forceEval x | forceEval (Set.toList x) `seq` True = x
%%[[101
  fevCount = fevCount . Set.toList
%%]]

instance (ForceEval k, ForceEval v) => ForceEval (Map.Map k v) where
  forceEval x | forceEval (Map.toList x) `seq` True = x
%%[[101
  fevCount = fevCount . Map.toList
%%]]
%%]

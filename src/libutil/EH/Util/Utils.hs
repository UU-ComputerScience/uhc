module EH.Util.Utils where

-- import EH.Util.Pretty
import Data.Char
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Graph as Graph

-------------------------------------------------------------------------
-- Set
-------------------------------------------------------------------------

unionMapSet :: Ord b => (a -> Set.Set b) -> (Set.Set a -> Set.Set b)
unionMapSet f = Set.unions . map f . Set.toList

-------------------------------------------------------------------------
-- Map
-------------------------------------------------------------------------

inverseMap :: (Ord k, Ord v') => (k -> v -> (v',k')) -> Map.Map k v -> Map.Map v' k'
inverseMap mk = Map.fromList . map (uncurry mk) . Map.toList

showStringMapKeys :: Map.Map String x -> String -> String
showStringMapKeys m sep = concat $ intersperse sep $ Map.keys m

-------------------------------------------------------------------------
-- List
-------------------------------------------------------------------------

hdAndTl' :: a -> [a] -> (a,[a])
hdAndTl' _ (a:as) = (a,as)
hdAndTl' n []     = (n,[])

hdAndTl :: [a] -> (a,[a])
hdAndTl = hdAndTl' undefined

maybeNull :: r -> ([a] -> r) -> [a] -> r
maybeNull n f l = if null l then n else f l

maybeHd :: r -> (a -> r) -> [a] -> r
maybeHd n f = maybeNull n (f . head)

wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy p l
  = w l
  where w [] = []
        w l  = let (l',ls') = break p l
               in  l' : case ls' of []       -> []
                                    (_:[])   -> [[]]
                                    (_:ls'') -> w ls''

initlast :: [a] -> Maybe ([a],a)
initlast as
  = il [] as
  where il acc [a]    = Just (reverse acc,a)
        il acc (a:as) = il (a:acc) as
        il _   _      = Nothing

initlast2 :: [a] -> Maybe ([a],a,a)
initlast2 as
  = il [] as
  where il acc [a,b]  = Just (reverse acc,a,b)
        il acc (a:as) = il (a:acc) as
        il _   _      = Nothing

firstNotEmpty :: [[x]] -> [x]
firstNotEmpty = maybeHd [] id . filter (not . null)

-- saturate a list, that is:
-- for all indices i between min and max,
-- if there is no listelement x for which  get x  returns i,
-- add an element  mk i  to the list

listSaturate :: (Enum a,Ord a) => a -> a -> (x -> a) -> (a -> x) -> [x] -> [x]
listSaturate min max get mk xs
  = [ Map.findWithDefault (mk i) i mp | i <- [min..max] ]
  where mp = Map.fromList [ (get x,x) | x <- xs ]

-- saturate a list with values from assoc list, that is:
-- for all indices i between min and max,
-- if there is no listelement x for which  get x  returns i,
-- add a candidate from the associationlist (which must be present) to the list

listSaturateWith :: (Enum a,Ord a) => a -> a -> (x -> a) -> [(a,x)] -> [x] -> [x]
listSaturateWith min max get missing l
  = listSaturate min max get mk l
  where mp = Map.fromList missing
        mk a = panicJust "listSaturateWith" $ Map.lookup a mp

-- variant on span, predicate on full list
spanOnRest :: ([a] -> Bool) -> [a] -> ([a],[a])
spanOnRest p []       = ([],[])
spanOnRest p xs@(x:xs')
	 | p xs      = (x:ys, zs)
	 | otherwise = ([],xs)
					   where (ys,zs) = spanOnRest p xs'

-------------------------------------------------------------------------
-- Tupling, untupling
-------------------------------------------------------------------------

tup123to1  (a,_,_) = a			-- aka fst3
tup123to12 (a,b,_) = (a,b)
tup12to123 c (a,b) = (a,b,c)

-------------------------------------------------------------------------
-- String
-------------------------------------------------------------------------

strWhite :: Int -> String
strWhite sz = replicate sz ' '

strPad :: String -> Int -> String
strPad s sz = s ++ strWhite (sz - length s)

strCapitalize :: String -> String
strCapitalize s
  = case s of
      (c:cs) -> toUpper c : cs
      _      -> s

strToInt :: String -> Int
strToInt = foldl (\i c -> i * 10 + ord c - ord '0') 0

-------------------------------------------------------------------------
-- Split for qualified name
-------------------------------------------------------------------------

splitForQualified :: String -> [String]
splitForQualified s
    = ws''
    where ws  = wordsBy (=='.') s
          ws' = case initlast2 ws of
                  Just (ns,n,"") -> ns ++ [n ++ "."]
                  _              -> ws
          ws''= case break (=="") ws' of
                  (nq,(_:ns)) -> nq ++ [concatMap ("."++) ns]
                  _ -> ws'

-------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------

panic m = error ("panic: " ++ m)

-------------------------------------------------------------------------
-- group/sort/nub combi's
-------------------------------------------------------------------------

isSortedByOn :: (b -> b -> Ordering) -> (a -> b) -> [a] -> Bool
isSortedByOn cmp sel l
  = isSrt l
  where isSrt (x1:tl@(x2:_)) = cmp (sel x1) (sel x2) /= GT && isSrt tl
        isSrt _              = True

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = sortByOn compare

sortByOn :: (b -> b -> Ordering) -> (a -> b) -> [a] -> [a]
sortByOn cmp sel = sortBy (\e1 e2 -> sel e1 `cmp` sel e2)

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn sel = groupBy (\e1 e2 -> sel e1 == sel e2)

groupSortOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortOn sel = groupOn sel . sortOn sel

groupByOn :: (b -> b -> Bool) -> (a -> b) -> [a] -> [[a]]
groupByOn eq sel = groupBy (\e1 e2 -> sel e1 `eq` sel e2)

groupSortByOn :: (b -> b -> Ordering) -> (a -> b) -> [a] -> [[a]]
groupSortByOn cmp sel = groupByOn (\e1 e2 -> cmp e1 e2 == EQ) sel . sortByOn cmp sel

nubOn :: Eq b => (a->b) -> [a] -> [a]
nubOn sel = nubBy (\a1 a2 -> sel a1 == sel a2)

-- | The 'consecutiveBy' function groups like groupBy, but based on a function which says whether 2 elements are consecutive
consecutiveBy                  :: (a -> a -> Bool) -> [a] -> [[a]]
consecutiveBy _        []      =  []
consecutiveBy isConsec (x:xs)  =  ys : consecutiveBy isConsec zs
  where (ys,zs) = consec x xs
        consec x []                        = ([x],[])
        consec x yys@(y:ys) | isConsec x y = let (yys',zs) = consec y ys in (x:yys',zs)
                            | otherwise    = ([x],yys)

-------------------------------------------------------------------------
-- Ordering
-------------------------------------------------------------------------

orderingLexic :: [Ordering] -> Ordering
orderingLexic = foldr1 (\o1 o2 -> if o1 == EQ then o2 else o1)

-------------------------------------------------------------------------
-- Maybe
-------------------------------------------------------------------------

panicJust :: String -> Maybe a -> a
panicJust m = maybe (panic m) id

infixr 0  $?

($?) :: (a -> Maybe b) -> Maybe a -> Maybe b
f $? mx = do x <- mx
             f x

orMb :: Maybe a -> Maybe a -> Maybe a
orMb m1 m2 = maybe m2 (const m1) m1
-- orMb = maybeOr Nothing Just Just

maybeAnd :: x -> (a -> b -> x) -> Maybe a -> Maybe b -> x
maybeAnd n jj ma mb
  = case ma of
      Just a
        -> case mb of {Just b -> jj a b ; _ -> n}
      _ -> n

maybeOr :: x -> (a -> x) -> (b -> x) -> Maybe a -> Maybe b -> x
maybeOr n fa fb ma mb
  = case ma of
      Just a -> fa a
      _      -> case mb of
                  Just b -> fb b
                  _      -> n

-------------------------------------------------------------------------
-- Strongly Connected Components
-------------------------------------------------------------------------

scc :: Ord n => [(n,[n])] -> [[n]]
scc = map Graph.flattenSCC . Graph.stronglyConnComp . map (\(n,ns) -> (n, n, ns))


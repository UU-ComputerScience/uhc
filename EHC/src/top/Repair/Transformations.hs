-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  portable
-----------------------------------------------------------------------------

module Top.Repair.Transformations where

import List (inits, tails)
import Top.Types
import Top.Repair.AExpr

-- * Transformation functions

-- | transformation to permute function arguments
permute :: RepairInfo info => Transform info
permute (App info fun args) =  [ App info fun args' | args' <- perms args]
permute _ = []


-- | change the type @t@ of an expression into @[t]@
listify :: RepairInfo info => Transform info
listify aexp = 
    [Lst emptyInfo [aexp]]

-- | change the type @[t]@ of an expression into @t@ if @[t]@ is a singleton list
unlistify :: RepairInfo info => Transform info
unlistify (Lst _ [lelem])  = [lelem]
unlistify _ = []


-- | introduce a new argument to an application
insertArgument :: RepairInfo info => Transform info
insertArgument (App info fun args) = 
    let 
        argsplit = zip (inits args) (tails args)
        newarg = Blk emptyInfo
        newarglists = case args of [] ->[[newarg]]; _ -> map (\(i,t) ->(i++[newarg]++t)) argsplit
                
    in
        [App info fun args' | args' <- newarglists]
insertArgument _ = []

-- | remove an argument from an application
deleteArgument :: RepairInfo info => Transform info
deleteArgument (App info fun args) = 
    let 
        delElem as i = (take (i-1) as) ++ (drop i as)
        newarglists = map (delElem args) (enumFromTo 1 (length args))
    in
        [App info fun args' | args' <- newarglists]
deleteArgument _ = []

-- | reorder applications by parenthesizing subexpressions
flattenApp :: RepairInfo info => Transform info
flattenApp (App info fun args) =
    let 
        buildapp (l,(ain, aout)) = App info fun (l ++ [App emptyInfo (head ain) (tail ain)] ++ aout)
        split2 = splitTwice 1 args
    in
        map buildapp split2
flattenApp _ = []

-- | reorder applications by removing parenthesis
flattenAppRev :: RepairInfo info => Transform info
flattenAppRev (App info fun args) = 
    let 
        flattenRev (a:as) rlists =  
            let newrlists =
                 case a of
                    app@(App _ fn arg) -> (map (++[app]) rlists) ++ (map (++[fn]++arg) rlists)
                    noapp -> map (++[noapp]) rlists
            in
                flattenRev as newrlists
        flattenRev [] rlists = rlists
    in
        [App info fun args' | args' <- flattenRev args [[]] {- , args'/=args -} ]
flattenAppRev _ = []


-- | permute elements of a tuple
permuteTuple :: RepairInfo info => Transform info
permuteTuple (Tup info telems) = [ Tup info telems' | telems' <- perms telems]
permuteTuple _ = []

-- | curry
curryTuple :: RepairInfo info => Transform info
curryTuple (App info fun args) = 
    let
        currySingle (Tup _ telems) = telems
        currySingle notup = [notup]
        
    in
        [App info fun args' | args' <- (optmap currySingle args [[]]) {- , args'/=args -} ]
curryTuple _ = []

-- | sibling identifiers
type Sibling = [(String, Tp)]

-- | sibling rivalry
sibling :: RepairInfo info => [Sibling] -> Transform info
sibling siblings (Var _ identifier) = 
    let 
        match = filter ((/=[]) . (filter ((== identifier) . fst))) siblings
        alternatives as = [Var (makeInfo tp) identifier' | (identifier', tp) <- as]
    in
        case match  of 
            [] -> []
            matchfound -> alternatives (head matchfound)
sibling _ _ = []

-- | coercion
coerce :: RepairInfo info => Transform info
coerce = undefined

-- | uncurry
uncurryTuple :: RepairInfo info => Transform info
uncurryTuple (App info fun args) = 
    let
        buildapp (l,(ain, aout)) = App info fun (l++[Tup emptyInfo ain] ++ aout)
        split2 = splitTwice 0 args
    in
        map buildapp split2
uncurryTuple _ = []

-- * Auxiliary functions

-- | split a list into 3 groups with the length of middle group being > ml
splitTwice :: Int -> [a] -> [([a], ([a], [a]))]
splitTwice ml elems= 
    let 
        minlength = filter ((>ml) . length . fst . snd)
        splt as = zip (inits as) (tails as)
    in    
        concatMap (minlength . (\(l,r)->(map (\rs->(l,rs)) (splt r)))) (splt elems)

-- | optional map
optmap :: (a -> [a]) -> [a] -> [[a]] -> [[a]]
optmap f (e:es) fes = optmap f es ((map (++(f e)) fes) ++ (map (++[e]) fes))
optmap _ _ fes = fes

-- | permutate a list
perms :: [a] -> [[a]]
perms [] =  [ [] ]
perms (a:x) =  [ z | y <- perms x, z <- insertions a y ]

-- | make variations of a list by inserting a new element at all possible locations
insertions :: a -> [a] -> [[a]]
insertions a [] =  [ [a] ]
insertions a x@(b:y) = (a:x) : [ b:z | z <- insertions a y ]


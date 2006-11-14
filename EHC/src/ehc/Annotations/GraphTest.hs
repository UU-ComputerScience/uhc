module GraphTest where


--
-- Experimental approach to constraint graphs
-- Only for experimentation, not finished yet
--
-- In the conventional constraint graphs, each
-- constraint corresponds to an edge. In this
-- representation, each variable and constraint
-- correspond to vertices. Edges correspond to
-- plugs. We can then represent the constraint
-- graph as a plus/min scaling system for each
-- variable.
--


import qualified Data.Graph.Inductive as G
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.List
import Data.Maybe


data Constr
  = Int :=>=: Int
  | Comp :<=: Int
  deriving (Eq, Ord, Show)
data Comp
  = Atom Int
  | Comp :+: Comp
  | Comp :-: Comp
  deriving (Eq, Ord, Show)

type ConstrGraph = G.Gr Operation ()
data Operation = Plus | Max | Var !Int deriving (Eq, Ord, Show)

testc = [1 :=>=: 2, 2 :=>=: 3, 2 :=>=: 4, (Atom 2 :+: Atom 4) :<=: 5, 5 :=>=: 6, 6 :=>=: 1 ]
testg = constrsToConstraintGraph testc
testn = killNonSacred [1,5] testg
testi = inferSubst testn [(1,1)] 0 upSum upMax

upSum = trunc . sum
upMax [] = 0
upMax xs = trunc (maximum xs)
trunc x = x `min` 2

constrsToConstraintGraph :: [Constr] -> ConstrGraph
constrsToConstraintGraph cs
  = G.mkGraph ns' (map (\(x,y) -> (x,y,())) es')
  where
    keepOld = flip const
    (vsMap, u) = foldr buildVsMap (Map.empty, 1) cs
    find k = Map.findWithDefault (error "Not in vsMap") k vsMap
    
    (ns, es, u2) = foldr build ([], [], u) cs
    ns' = ns ++ concat [[(u1,Var n),(u2, Max)] | (n, (u1,u2)) <- Map.assocs vsMap ]
    es' = es ++ [(u2,u1) | (_, (u1,u2)) <- Map.assocs vsMap]
    
    build (a :=>=: b) (ns, es, u)
      = let (s, _) = find a
            (_, t) = find b
         in (ns, (s, t) : es, u)
    build (c :<=: b) r
      = let (_, t) = find b
         in buildComp c t r
    buildComp (Atom i) t (ns, es, u)
      = let (s, _) = find i
         in (ns, (s,t):es, u)
    buildComp (a :+: b) t (ns, es, u)
      = let u' = u+1
            (ns1,es1,u1) = buildComp a u (ns, es, u)
            (ns2,es2,u2) = buildComp b u (ns1, es1, u1)
         in ((u, Plus) : ns2, (u, t) : es2, u2)
    buildComp (a :-: b) t (ns, es, u)
      = let u' = u+1
            (ns1,es1,u1) = buildComp a u (ns, es, u)
            (ns2,es2,u2) = buildComp b u (ns1, es1, u1)
         in ((u, Max) : ns2, (u, t) : es2, u2)
  
    buildVsMap (a :=>=: b) (m, u)
      = let u1 = u+2
            u2 = u1+2
            m1 = Map.insertWith keepOld a (u,u+1) m
            m2 = Map.insertWith keepOld b (u1,u1+1) m1
         in (m2, u2)
    buildVsMap (c :<=: b) (m, u)
      = let u1 = u+2
            m1 = Map.insertWith keepOld b (u,u+1) m
            r  = buildVsMapComp c m1 u1
         in r
    
    buildVsMapComp (Atom i) m u = (Map.insertWith keepOld i (u,u+1) m, u+2)
    buildVsMapComp (a :+: b) m u = let (m',u') = buildVsMapComp a m u in buildVsMapComp b m' u'
    buildVsMapComp (a :-: b) m u = let (m',u') = buildVsMapComp a m u in buildVsMapComp b m' u'

inferSubst :: ConstrGraph -> [(Int, Int)] -> Int -> ([Int] -> Int) -> ([Int] -> Int) -> [(Int, Int)]
inferSubst g initial bot fPlus fMax
  = let ns = G.labNodes g
        s  = map (\(n,a) -> (n, repl a)) ns
     in trans . nubBy (\(a,_) (b,_) -> a == b) $ iter (G.nodes g) s
  where
    mp = Map.fromList initial
    repl (Var i) = Map.findWithDefault bot i mp
    repl _       = bot
    
    trans s
      = [(i,a) | (v,a) <- s, let r = fromJust (G.lab g v), isVar r, let (Var i) = r ]
      
    isVar (Var _) = True
    isVar _       = False
    
    iter [] subst = subst
    iter (n : ns) subst
      = let vOld = fromJust (n `lookup` subst)
            ss = G.pre g n
            vs = map (fromJust . (`lookup` subst)) ss
            o = fromJust (G.lab g n)
            vNew = fMax [ vOld
                        , case o of
                            Var _ -> head vs
                            Plus  -> fPlus vs
                            Max   -> fMax vs
                        ]
            subst' = ((n,vNew) : subst)
         in if vNew == vOld
            then iter ns subst'
            else iter (G.suc g n ++ ns) subst'

instantiate :: ConstrGraph -> ConstrGraph -> [(Int, Int)] -> ConstrGraph
instantiate src dst mp
  = dst

killNonSacred :: [Int] -> ConstrGraph -> ConstrGraph
killNonSacred sac g
  = let vs = G.labNodes g
        ns = [n | (n,a) <- vs, not (isSacVar a)]
        isSacVar (Var i) = i `elem` sac
        isSacVar _       = True
        es = G.labEdges g
     in G.mkGraph [v | v@(n,_) <- vs, not (n `elem` ns)] ([(p, s, ()) | n <- ns, p <- G.pre g n, s <- G.suc g n] ++ [e | e@(a,b,_) <- es, not (a `elem` ns || b `elem` ns) ])

reduce :: ConstrGraph -> ConstrGraph
reduce
  = undefined

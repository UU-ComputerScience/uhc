import EH99.GrinCode.Common hiding (Variable)
import Data.Graph
import Data.List
import Data.Ord
import Data.Array
import Control.Arrow
import Debug.Trace

-- type VarMp = IntMap.IntMap [Int]
-- 
-- eqPrereqEqs :: IntMap.IntMap [Int] -> Equation -> (Equation, Int, [Int])
-- eqPrereqEqs mp eq = 


eqPrereqs :: Equation -> (Variable, [Variable])
eqPrereqs eq =
  case eq of
    (IsBasic v)               -> nodeps v
    (IsTags  v _)             -> nodeps v
    (IsPointer v _ _)         -> nodeps v
    (IsConstruction v _ _ _)  -> nodeps v
    (IsUpdate v d)            -> dep v d
    (IsEqual v d)             -> dep v d
    (IsSelection v d _ _)     -> dep v d
    (IsEnumeration v d)       -> dep v d
    (IsEvaluation  v d _)     -> dep v d
    (IsApplication v ds _)    -> (Variable v, map Variable ds)
  where nodeps v    = (Variable v, [])
        dep    v d  = (Variable v, [Variable d])


data Node = Eq Int Equation | Var Int Variable
  deriving Show
type Nodes = [Node]

newtype Variable = Variable Int
  deriving Show
type Variables = [Variable]

type VarMp  = Variable -> Int
type NodeMp = Int -> Node

data EqSet = EqSet
  { nodes  :: Nodes
  , varMp  :: VarMp
  , nodeMp :: NodeMp
  }

mkEqSet :: Equations -> Variables -> EqSet
mkEqSet eqs vars =
  EqSet
    { nodes  = nds
    , varMp  = vmp
    , nodeMp = nmp
    }
  where neqs  = map (Eq undefined)  eqs
        nvars = map (Var undefined) vars
        nds   = zipWith plak [0..] (neqs ++ nvars)
        plak i (Eq _ e)  = Eq i e
        plak i (Var _ v) = Var i v
        vmp (Variable n) = n + length neqs
        arr = listArray (0, pred (length nds)) nds
        nmp = (arr !)


traceShowIt' a = traceShow ("%%" ++ show a ++ "%%")
traceShowIt a = traceShowIt' a a

nummerEqs :: Int -> Equations -> (Nodes, Int)
nummerEqs i []     = ([], i)
nummerEqs i (e:es) = first (Eq i e :) $ nummerEqs (succ i) es

nummerVars :: Nodes -> Int -> Variables -> (Nodes, Int)
nummerVars nds i []     = (nds, i)
nummerVars nds i (v:vs) = first (Var i v :) $ nummerVars nds (succ i) vs

mkEdges :: VarMp -> Node -> [(Int, [Int])]
mkEdges _  (Var i _) = [(i, [])]
mkEdges mp (Eq i eq) = (i, [mp vout]) : map mkIn vins
  where (vout, vins) = eqPrereqs eq
        mkIn   vin   = (mp vin, [i])

groupSrcNode :: [(Int, [Int])] -> [(Int, [Int])]
groupSrcNode = map merge . groupBy (same fst) . sortBy (comparing fst)
  where same f x y = f x == f y
        merge [] = error "Empty list in groupBy result"
        merge xs = (fst (head xs), concatMap snd xs)

-- mkGraph :: EqSet -> [(Int, [Int])] -> (Graph, Vertex -> Node)
mkGraph eqs es = (graph, vmp)
  where uptuple (i, os) = (nodeMp eqs i, i, os)
        (graph, vers)   = graphFromEdges' $ map uptuple es
        vmp v = let (n, _, _) = vers v in n

aap eqs = mkGraph eqs . groupSrcNode . concatMap (mkEdges (varMp eqs)) . nodes $ eqs

vbEqs = mkEqSet [IsEqual x y, IsApplication f [g, x] nee, IsEqual g x, IsEqual f aap]
                (map Variable [x, y, f, g, aap, nee])
  where x = 0
        y = 1
        f = 2
        g = 3
        aap = 4
        nee = (-1)


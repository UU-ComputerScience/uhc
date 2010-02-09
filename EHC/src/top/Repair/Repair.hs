-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  portable
-----------------------------------------------------------------------------

module Top.Repair.Repair where

import Control.Monad.State
import Top.Qualifiers.TypeClasses () -- required for instance definition
import Top.Constraints.TypeConstraintInfo () -- required for instance definition
import Top.Types
import Top.Solvers.GreedySolver
import Top.Solvers.SolveConstraints
import Top.Constraints.Equality (EqualityConstraint(..)) 
import Top.Repair.AExpr
import Data.Maybe

-------------------
repair :: RepairInfo info => AExpr info -> [String]
repair aexpr = [showSimple aexpr]

-- | use a state monad to store information about used type-variables and equalityconstraints
type TI a = State (Int, [EqualityConstraint ()]) a

-- | set the counter
setUnique :: Int -> TI ()
setUnique i = modify (\(_, cs) -> (i, cs))

-- | increase the counter
nextUnique :: TI Int
nextUnique =
   do i <- gets fst
      setUnique (i+1)
      return i

-- maak een equality constraint (en bewaar deze in de monad)
-- Let op: deze operator "shadowed" de definie uit  Top.Constraints.EqualityConstraint!
(.==.) :: Tp -> Tp -> TI ()
t1 .==. t2 = modify (\(i, cs) -> (i, Equality t1 t2 () : cs))

-- haal het type uit de "info", of introduceer een nieuwe type variabele
typeFromInfo :: RepairInfo info => info -> TI Tp
typeFromInfo = maybe (nextUnique >>= return . TVar) return . getType

collect :: RepairInfo info => AExpr info -> TI Tp
collect aexpr =
   case aexpr of
      App info fun args ->
         do tpFun <- collect fun
            tps   <- mapM collect args
            tp    <- typeFromInfo info
            tpFun .==. foldr (.->.) tp tps
            return tp
      If info grd lb rb ->
         do t1 <- collect grd
            t2 <- collect lb
            t3 <- collect rb
            tp <- typeFromInfo info
            t1 .==. boolType
            t2 .==. tp
            t3 .==. tp
            return tp
      Lst info lelems ->
         do tps <- mapM collect lelems
            tp  <- typeFromInfo info
            mapM_ ((tp .==.) . listType) tps
            return tp
      Tup info telems ->
        do  tps <- mapM collect telems
            tp <- typeFromInfo info
            tp .==. tupleType tps
            return tp
      _ ->
         typeFromInfo (getInfo aexpr)


-- * Consistency checking functions



-- | check if the expression is well typed
wellTyped :: RepairInfo info => AExpr info -> Bool
wellTyped aexpr =
   let unique  = nextTypeVar aexpr
       (i, cs) = execState (collect aexpr) (unique, [])
       result :: SolveResult () Predicates ()
       result  = runGreedy standardClasses noOrderedTypeSynonyms i cs
   in null (errorsFromResult result)

-- * Repair functions

-- | repair the expression, if it is not correct already
doRepair :: RepairInfo info => [Transform info] -> Int -> AExpr info -> [(AExpr info, Int)]
doRepair transformations depth aexpr
   | wellTyped aexpr = [(aexpr, depth)]
   | depth <= 0      = []
   | otherwise       =
        let xs = [ x | t <- transformations, x <- t aexpr ]
        in concatMap (doRepair transformations (depth - 1)) xs

-- | apply a transformation at every possible node
everywhere :: Transform info -> Transform info
everywhere transformation = rec
    where
        recList [] = []
        recList (aexpr:rest) =
            [ aexpr':rest | aexpr' <- rec aexpr ] ++
            [ aexpr:rest' | rest' <- recList rest ]
        
        rec aexpr =
            transformation aexpr ++
                case aexpr of
                App info fun args ->
                    [ App info fun' args | fun'  <- rec fun ] ++
                    [ App info fun args' | args' <- recList args ]
                If info grd lb rb ->
                    [If info grd' lb rb | grd' <- rec grd] ++
                    [If info grd lb' rb | lb' <- rec lb] ++
                    [If info grd lb rb' | rb' <- rec rb]
                Lst info elems ->
                    [Lst info elems' | elems' <- recList elems]
                Tup info elems ->
                    [Tup info elems' | elems' <- recList elems]
                _ -> transformation aexpr
        
        
-- * Auxiliary functions
nextTypeVar :: RepairInfo info => AExpr info -> Int
nextTypeVar = (+1) . maximum . (0:) . ftv . map getType . getInfos
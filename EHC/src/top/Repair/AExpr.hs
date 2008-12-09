-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  portable
-----------------------------------------------------------------------------

module Top.Repair.AExpr where

import Top.Types
import Data.List (intersperse)

-- * Classes

-- | storing things about specific nodes in 'AExpr'
class RepairInfo info where
    -- | extract type information
    getType :: info -> Maybe Tp
    -- | set type information
    makeInfo  :: Tp -> info
    emptyInfo :: info

-- | currently only type information is recorded    
instance RepairInfo (Maybe Tp) where
    getType   = id
    makeInfo  = Just
    emptyInfo = Nothing

    
-- * Types   

-- | transformation function
type Transform info = AExpr info -> [AExpr info]


-- * Data types

-- | abstract syntax tree       
data AExpr info = 
        App info (AExpr info) [AExpr info] -- ^ application node
    |   If info (AExpr info) (AExpr info) (AExpr info) -- ^ if-then-else node
    |   Tup info [AExpr info] -- ^ tuple node
    |   Lst info [AExpr info] -- ^ list node
    |   Var info String -- ^ variable node
    |   Blk info -- ^ unmutable block node
    deriving (Eq,Show)

-- * Functions

-- | extract info from expression    
getInfo :: AExpr info -> info
getInfo (App info _ _) = info
getInfo (If info _ _ _) = info
getInfo (Lst info _) = info
getInfo (Tup info _) = info
getInfo (Var info _) = info
getInfo (Blk info) = info

getInfos :: AExpr info -> [info]
getInfos aexpr = 
   getInfo aexpr : concatMap getInfos (subexpressions aexpr)

instance Functor AExpr where
   fmap f aexpr = 
      case aexpr of
         App a fun args -> App (f a) (fmap f fun) (map (fmap f) args)
         If a e1 e2 e3  -> If  (f a) (fmap f e1) (fmap f e2) (fmap f e3)
         Lst a elts     -> Lst (f a) (map (fmap f) elts)            
         Var a s        -> Var (f a) s
         Blk a          -> Blk (f a)
            
mapAExprM :: Monad m => (a -> m b) -> AExpr a -> m (AExpr b)
mapAExprM f aexpr = 
   case aexpr of
      App a fun args -> 
         do b     <- f a
            fun'  <- mapAExprM f fun
            args' <- mapM (mapAExprM f) args
            return (App b fun' args')
      If a e1 e2 e3 -> 
         do b   <- f a
            e1' <- mapAExprM f e1
            e2' <- mapAExprM f e2
            e3' <- mapAExprM f e3
            return (If b e1' e2' e3')
      Lst a elts -> 
         do b     <- f a
            elts' <- mapM (mapAExprM f) elts
            return (Lst b elts')                
      Var a s -> 
         do b <- f a
            return (Var b s)
      Blk a -> 
         do b <- f a
            return (Blk b)

subexpressions :: AExpr a -> [AExpr a]
subexpressions aexpr =
   case aexpr of
      App _ fun args -> fun : args
      If _ e1 e2 e3  -> [e1, e2, e3]
      Lst _ elts     -> elts             
      Var _ _        -> []
      Blk _          -> []
            
isBlock :: AExpr a -> Bool
isBlock (Blk _) = True
isBlock _       = False

-- * Pretty printing functions

-- | convert the abstract expression into human readable form
showSimple :: AExpr info -> String
showSimple expr =
    case expr of
        App _ f es -> "(" ++ concat (intersperse " " (map showSimple (f:es))) ++ ")"
        If _ c t e -> concat ["if ", showSimple c, " then ", showSimple t, " else ", showSimple e]
        Lst _ es   -> "[" ++ concat (intersperse "," (map showSimple es)) ++ "]"
        Var _ s    -> s
        Blk _      -> "???"
        Tup _ es   -> "(" ++ concat (intersperse "," (map showSimple es)) ++ ")"


-- | print the abstract syntax tree in dot-format
prettyPrintDot :: RepairInfo info => AExpr (Maybe info) -> [Char]
prettyPrintDot aexp = "digraph typetree\n{" ++ (prettyPrintAExpDot aexp "a") ++ "}"



-- * Auxiliary functions

prettyPrintAExpDot :: RepairInfo info => AExpr (Maybe info) -> [Char] -> [Char]
prettyPrintAExpDot (App _ l r) nodename = 
    nodename ++ " [shape=box,label=\"App\"];\n" ++ 
    (prettyPrintAExpDot l (nodename ++ "l")) ++
    nodename ++ "c" ++ " [shape=box, label=\"args\"];\n" ++ 
    printArgs r (nodename ++ "r") (nodename ++ "c") ++
    nodename ++ "->" ++ (nodename++"l") ++ "\n" ++ 
    nodename ++ "->" ++ (nodename++"c") ++ "\n"
prettyPrintAExpDot (If _ g l r) nodename = 
    nodename ++ " [shape=box,label=\"If\"];\n" ++ 
    (prettyPrintAExpDot g (nodename ++ "g")) ++
    (prettyPrintAExpDot l (nodename ++ "l")) ++
    (prettyPrintAExpDot r (nodename ++ "r")) ++
    nodename ++ "->" ++ (nodename++"g") ++ "\n" ++ 
    nodename ++ "->" ++ (nodename++"l") ++ "\n" ++ 
    nodename ++ "->" ++ (nodename++"r") ++ "\n"
prettyPrintAExpDot (Lst _ elems) nodename = 
    nodename ++ " [shape=box,label=\"Lst\"];\n" ++ 
    printArgs elems (nodename ++ "e") nodename ++
    nodename ++ "->" ++ (nodename++"e") ++ "\n"
prettyPrintAExpDot (Tup _ elems) nodename = 
    nodename ++ " [shape=box,label=\"Tup\"];\n" ++ 
    printArgs elems (nodename ++ "e") nodename ++
    nodename ++ "->" ++ (nodename++"e") ++ "\n"
prettyPrintAExpDot (Var _ str) nodename = 
    nodename ++ " [shape=box,label=\"Var " ++ str ++ "\"];\n"
prettyPrintAExpDot (Blk _) nodename = 
    nodename ++ " [shape=box,label=\"Blk\"];\n"

printArgs :: RepairInfo info => [AExpr (Maybe info)] -> [Char] -> [Char] -> [Char]
printArgs (arg:args) nodename parent = 
    (prettyPrintAExpDot arg nodename) ++ 
    parent ++ "->" ++ nodename ++ "\n" ++ 
    printArgs args (nodename ++ "r") parent
printArgs [] _ _ = ""

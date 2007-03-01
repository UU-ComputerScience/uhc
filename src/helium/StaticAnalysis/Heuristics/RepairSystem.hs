module Helium.StaticAnalysis.Heuristics.RepairSystem (repairSystem) where

import Top.Types
import Top.States.BasicState (printMessage)
import Top.TypeGraph.Basics (EdgeId, VertexId(..))
import Top.TypeGraph.Heuristics
import Top.TypeGraph.TypeGraphState
import Top.Repair.Repair (repair)
import Top.Repair.AExpr

import Helium.StaticAnalysis.Miscellaneous.ConstraintInfo
import Helium.StaticAnalysis.Miscellaneous.DoublyLinkedTree
import Helium.Syntax.UHA
import Helium.StaticAnalysis.Miscellaneous.UHA_Source
import Helium.Utils.Utils (internalError)

import Data.Maybe (isJust, fromJust)

type HeliumRepairInfo = (Maybe Tp, Maybe UHA_Source)

instance RepairInfo HeliumRepairInfo where
   getType = fst
   makeInfo tp = (Just tp, Nothing)
   emptyInfo = (Nothing, Nothing)

repairSystem :: Heuristic ConstraintInfo
repairSystem = Heuristic (Filter "Repair System" handleList)
 where
   handleList :: HasTypeGraph m ConstraintInfo => [(EdgeId, ConstraintInfo)] -> m [(EdgeId, ConstraintInfo)]
   handleList xs = 
      do aexprs <- recList xs
         printMessage "\n========== Begin of Repair System ===========\n"
         printMessage $ unlines (map (unlines . repair) aexprs)
         printMessage "========== End of Repair System ===========\n"
         return xs
   
   recList :: HasTypeGraph m ConstraintInfo => [(EdgeId, ConstraintInfo)] -> m [AExpr HeliumRepairInfo]
   recList []     = return []
   recList (x:xs) = 
      do mPair <- handleOne x
         case mPair of
            Just (edges, aexpr) ->
               do aexprs <- recList (filter ((`notElem` edges) . fst) xs)
                  return (aexpr:aexprs)
            Nothing ->
               recList xs
   
   handleOne :: HasTypeGraph m ConstraintInfo => (EdgeId, ConstraintInfo) -> m (Maybe ([EdgeId], AExpr HeliumRepairInfo))
   handleOne (edgeId, info)
      | isBlock aexpr = return Nothing
      | otherwise = 
           do edges <- whichEdges aexpr
              doWithoutEdges edges $
                 do mexpr <- substituteLocalType aexpr
                    return (fmap (\expr -> (map fst edges, expr)) mexpr)
    where
      aexpr = makeAExpr (rootOfInfoTree (localInfo info))
               
substituteLocalType :: HasTypeGraph m ConstraintInfo => AExpr LocalInfo -> m (Maybe (AExpr HeliumRepairInfo))
substituteLocalType aexpr = 
   do subExpr <- mapAExprM toRepairInfo aexpr
      return (change subExpr)
 where
   change :: AExpr (Maybe HeliumRepairInfo) -> Maybe (AExpr HeliumRepairInfo)
   change aexpr
      | all isJust (getInfos aexpr) = Just (fmap fromJust aexpr)
      | otherwise                   = Nothing
 
   toRepairInfo :: HasTypeGraph m ConstraintInfo => LocalInfo -> m (Maybe HeliumRepairInfo)
   toRepairInfo info = 
      case assignedType info of
         Just tp ->
            do mtp <- substituteTypeSafe tp
               return (fmap (\tp -> (Just tp, Just (self info))) mtp)
         Nothing ->
            return Nothing
            
rootOfInfoTree :: InfoTree -> InfoTree
rootOfInfoTree infoTree
   | isBlock (makeAExpr infoTree) = infoTree
   | otherwise = rec infoTree
 where
   rec thisTree = 
      case parent thisTree of
         Just it | not . isBlock . makeAExpr $ it
            -> rec it
         _  -> thisTree

makeAExpr :: InfoTree -> AExpr LocalInfo
makeAExpr infoTree =
   case self (attribute infoTree) of
      UHA_Expr e -> make e
      _          -> Blk info
 where
   info   = attribute infoTree
   aexprs = map makeAExpr (children infoTree)
   
   make :: Expression -> AExpr LocalInfo
   make expression = 
      case expression of
         Expression_If _ _ _ _ ->
            case aexprs of
               [ae1, ae2, ae3] -> If info ae1 ae2 ae3
               _ -> internalError "RepairSystem.hs" "makeAExpr" "Cannot make conditional"
         Expression_NormalApplication _ _ _ ->
            case aexprs of
               fun:a:as -> App info fun (a:as)
               _ -> internalError "RepairSystem.hs" "makeAExpr" "Cannot make (normal) application"
         Expression_List _ _ ->
            Lst info aexprs
         Expression_Variable _ name -> 
            Var info (show name)
         Expression_Constructor _ name ->
            Var info (show name)
         _ -> Blk info
            
whichEdges :: HasTypeGraph m ConstraintInfo => AExpr LocalInfo -> m [(EdgeId, ConstraintInfo)]
whichEdges aexpr
   | isBlock aexpr = return []
   | otherwise = 
        do let vs = [ v | Just v <- map infoToVar (aexpr : subexpressions aexpr) ]
           allEdges <- mapM edgesFrom vs
           let edges = filter (fromTheSameLocation aexpr) (concat allEdges)
           rest <- mapM whichEdges (subexpressions aexpr)
           return (edges ++ concat rest)
 where
   infoToVar :: AExpr LocalInfo -> Maybe VertexId
   infoToVar = fmap (VertexId . head . ftv) . assignedType . getInfo

   fromTheSameLocation :: AExpr LocalInfo -> (EdgeId, ConstraintInfo) -> Bool
   fromTheSameLocation aexpr (_, info) = 
      assignedType (getInfo aexpr) == assignedType (attribute (localInfo info))

------------------------------------------------------------------
-- This part should be in the Top library
{-
data AExpr info = 
        App info (AExpr info) [AExpr info] -- ^ application node
    |   If info (AExpr info) (AExpr info) (AExpr info) -- ^ if-then-else node
    |   Lst info [AExpr info] -- ^ list node
    |   Var info String -- ^ variable node
    |   Blk info -- ^ unmutable block node
 deriving Show

getInfo :: AExpr info -> info
getInfo (App info _ _) = info
getInfo (If info _ _ _) = info
getInfo (Lst info _) = info
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
-}
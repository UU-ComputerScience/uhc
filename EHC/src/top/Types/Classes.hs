-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  portable
--
-- Type classes and the standard reduction instances. A part of the code
-- was taken from the paper "Typing Haskell in Haskell".
--
-----------------------------------------------------------------------------

module Top.Types.Classes where

import Top.Types.Primitive
import Top.Types.Substitution
import Top.Types.Unification
import Top.Types.Synonym
import Top.Types.Qualification
import Control.Monad
import qualified Data.Map as M

----------------------------------------------------------------------  
-- * Class predicates

type Predicates = [Predicate]
data Predicate  = Predicate String Tp deriving Eq

instance Show Predicate where
   show (Predicate s tp) = if priorityOfType tp == 2 
                             then s ++ " " ++ show tp
                             else s ++ " (" ++ show tp ++ ")"                       

instance Substitutable Predicate where
   sub |-> (Predicate s tp) = Predicate s (sub |-> tp)
   ftv     (Predicate _ tp) = ftv tp

instance HasTypes Predicate where
   getTypes      (Predicate _ tp) = [tp] 
   changeTypes f (Predicate s tp) = Predicate s (f tp)

instance ShowQualifiers Predicate
 
----------------------------------------------------------------------  
-- * Class environments and instances

type ClassEnvironment = M.Map String Class
type Class            = ([String], Instances)
type Instances        = [Instance]
type Instance         = (Predicate, Predicates)

-- |The empty class environment
emptyClassEnvironment :: ClassEnvironment
emptyClassEnvironment = M.empty

matchPredicates :: OrderedTypeSynonyms -> Predicate -> Predicate -> Maybe MapSubstitution
matchPredicates synonyms (Predicate s1 t1) (Predicate s2 t2)
   | s1 == s2 = case mguWithTypeSynonyms synonyms (freezeVariablesInType t1) t2 of
        Left _       -> Nothing
        Right (_, s) -> Just (M.map unfreezeVariablesInType s)
   | otherwise = Nothing

insertInstance :: String -> Instance -> ClassEnvironment -> ClassEnvironment 
insertInstance className inst env = 
    case M.lookup className env of
        Nothing -> M.insert className ([], [inst]) env
        Just (parents, insts) -> M.insert className (parents, inst:insts) env

---------------------------------------------------------------------- 
-- * Class environment

inClassEnvironment :: String -> ClassEnvironment -> Bool
inClassEnvironment = M.member

superclassPaths :: String -> String -> ClassEnvironment -> [[String]]
superclassPaths from to cs 
   | from == to = [[to]]
   | otherwise  = [ from : path | sc <- superclasses from cs, path <- superclassPaths sc to cs ]

-- |For example, Eq is a superclass of Ord
superclasses :: String -> ClassEnvironment -> [String]
superclasses s cs = maybe [] fst (M.lookup s cs)

instances :: String -> ClassEnvironment -> Instances 
instances s cs = maybe [] snd (M.lookup s cs)

---------------------------------------------------------------------- 
-- * Head normal form

inHeadNormalForm :: Predicate -> Bool
inHeadNormalForm (Predicate _ tp) = hnf tp
   where hnf (TVar _)   = True
         hnf (TCon _)   = False
         hnf (TApp t _) = hnf t

listToHeadNormalForm :: OrderedTypeSynonyms -> ClassEnvironment -> Predicates -> Maybe Predicates
listToHeadNormalForm synonyms classes ps = 
   do pss <- mapM (toHeadNormalForm synonyms classes) ps
      return (concat pss)
          
toHeadNormalForm :: OrderedTypeSynonyms -> ClassEnvironment -> Predicate -> Maybe Predicates         
toHeadNormalForm synonyms classes p
   | inHeadNormalForm p = Just [p]
   | otherwise          = do ps <- byInstance synonyms classes p
                             listToHeadNormalForm synonyms classes ps   

---------------------------------------------------------------------- 
-- * Entailment

bySuperclass :: ClassEnvironment -> Predicate -> Predicates
bySuperclass classes p@(Predicate s tp) =
   p : concat [ bySuperclass classes (Predicate s' tp) | s' <- superclasses s classes ]

byInstance :: OrderedTypeSynonyms -> ClassEnvironment -> Predicate -> Maybe Predicates
byInstance synonyms classes p@(Predicate s _) =
   let tryInstance (p',list) = do sub <- matchPredicates synonyms p p'
                                  Just (sub |-> list)
   in msum [ tryInstance it | it <- instances s classes ]

entail :: OrderedTypeSynonyms -> ClassEnvironment -> Predicates -> Predicate -> Bool
entail synonyms classes ps p = 
   scEntail classes ps p ||
   case byInstance synonyms classes p of
      Nothing -> False
      Just qs -> all (entail synonyms classes ps) qs

entailList :: OrderedTypeSynonyms -> ClassEnvironment -> Predicates -> Predicates -> Bool
entailList synonyms classes ps = all (entail synonyms classes ps)

scEntail :: ClassEnvironment -> Predicates -> Predicate -> Bool
scEntail classes ps p = any (p `elem`) (map (bySuperclass classes) ps)

---------------------------------------------------------------------- 
-- * Context reduction

newtype ReductionError a = ReductionError a
   deriving Show

contextReduction :: OrderedTypeSynonyms -> ClassEnvironment -> Predicates -> 
                       (Predicates, [ReductionError Predicate])
contextReduction synonyms classes ps = 
   let op p (a,b) = case toHeadNormalForm synonyms classes p of
                       Just qs -> (qs++a,b)
                       Nothing -> (a,ReductionError p : b)                       
       (predicates, errors) = foldr op ([], []) ps
       
       loop rs []                                   = rs
       loop rs (x:xs) | scEntail classes (rs++xs) x = loop rs xs
                      | otherwise                   = loop (x:rs) xs  
                           
   in (loop [] predicates, errors)
   
associatedContextReduction :: OrderedTypeSynonyms -> ClassEnvironment -> [(Predicate, a)] -> 
                                 ([(Predicate,a)], [ReductionError (Predicate, a)])
associatedContextReduction synonyms classes ps = 
   let op (predicate, a) (reduced, es) = 
          case toHeadNormalForm synonyms classes predicate of
             Just qs -> ([(p,a) | p <- qs]++reduced,es)
             Nothing -> (reduced,ReductionError (predicate, a) : es)                       
       (predicates, errors) = foldr op ([], []) ps
       
       loop rs []                 = rs
       loop rs (q:qs) | entailed  = loop rs qs
                      | otherwise = loop (q:rs) qs  
          where entailed = scEntail classes (map fst (rs++qs)) (fst q)                      
                           
   in (loop [] predicates, errors)
                             
---------------------------------------------------------------------- 
-- * Standard class environment

-- This environment is only used at three places:
--   o  MiscErrors.ag
--   o  Warnings.ag
--   o  Collect.ag  (initialization in import environment)
standardClasses :: ClassEnvironment
standardClasses = M.fromList $ 

   -- only two instances for Num: Int and Float
   ( "Num",  
     ( ["Eq","Show"] -- superclasses
     , [ (Predicate "Num" intType  , []) -- instances
       , (Predicate "Num" floatType, [])
       ]
     )
   ) :
   ( "Enum", ([], [ (Predicate "Enum" tp, []) | tp <- [voidType, charType, intType, floatType, boolType]])
   ) :
   -- Eq, Ord and Show all have the same instances
   [ ("Eq" ,  ([]    , makeInstances "Eq"  ))
   , ("Ord",  (["Eq"], makeInstances "Ord" ))
   , ("Show", ([],     makeInstances "Show"))
   ]
   
   where 
     makeInstances className = 
        let basicTypes = [intType, floatType, boolType, charType]
            makeTupleInstance i = 
               ( Predicate className (tupleType [ TVar n | n <- [1..i] ])
               , [ Predicate className (TVar n) | n <- [1..i] ]
               ) 
        in (Predicate className (listType (TVar 0)), [Predicate className (TVar 0)]) -- instance for Lists
           :  [ (Predicate className tp, []) | tp <- basicTypes ]
           ++ map makeTupleInstance (0 : [2..10])

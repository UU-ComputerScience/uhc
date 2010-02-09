{-| Module      :  DictionaryEnvironment
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.ModuleSystem.DictionaryEnvironment 
   ( DictionaryEnvironment, DictionaryTree(..) 
   , emptyDictionaryEnvironment, addForDeclaration, addForVariable
   , getPredicateForDecl, getDictionaryTrees
   , makeDictionaryTree, makeDictionaryTrees
   ) where

import qualified Data.Map as M
import Helium.Syntax.UHA (Name)
import Helium.Syntax.UHA_Utils (NameWithRange(..) )
import Helium.Utils.Utils (internalError)
import Top.Types

data DictionaryEnvironment = 
     DEnv { declMap :: M.Map NameWithRange Predicates
          , varMap  :: M.Map NameWithRange [DictionaryTree]
          }
          
data DictionaryTree = ByPredicate Predicate
                    | ByInstance String {- class name -} String {- instance name -} [DictionaryTree]
                    | BySuperClass String {- sub -} String {- super -} DictionaryTree
   deriving Show
   
instance Show DictionaryEnvironment where
   show denv = 
      "{ declMap = " ++ show (M.assocs $ declMap denv) ++
      ", varMap = "  ++ show (M.assocs $ varMap denv) ++ "}"
       
emptyDictionaryEnvironment :: DictionaryEnvironment
emptyDictionaryEnvironment = 
   DEnv { declMap = M.empty, varMap = M.empty }
 
addForDeclaration :: Name -> Predicates -> DictionaryEnvironment -> DictionaryEnvironment
addForDeclaration name predicates dEnv
   | null predicates = dEnv
   | otherwise       = dEnv { declMap = M.insert (NameWithRange name) predicates (declMap dEnv) }
   
addForVariable :: Name -> [DictionaryTree] -> DictionaryEnvironment -> DictionaryEnvironment
addForVariable name trees dEnv
  | null trees = dEnv  
  | otherwise  = dEnv { varMap = M.insert (NameWithRange name) trees (varMap dEnv) }

getPredicateForDecl :: Name -> DictionaryEnvironment -> Predicates
getPredicateForDecl name dEnv =
   case M.lookup (NameWithRange name) (declMap dEnv) of
      Just ps -> ps
      Nothing -> []

getDictionaryTrees :: Name -> DictionaryEnvironment -> [DictionaryTree]
getDictionaryTrees name dEnv = 
   case M.lookup (NameWithRange name) (varMap dEnv) of
      Just dt -> dt
      Nothing -> []

makeDictionaryTrees :: ClassEnvironment -> Predicates -> Predicates -> Maybe [DictionaryTree]
makeDictionaryTrees classEnv ps = mapM (makeDictionaryTree classEnv ps)

makeDictionaryTree :: ClassEnvironment -> Predicates -> Predicate -> Maybe DictionaryTree
makeDictionaryTree classEnv availablePredicates p@(Predicate className tp) =      
   case tp of
      TVar _ | p `elem` availablePredicates -> Just (ByPredicate p)
             | otherwise -> case [ (path, availablePredicate)
                                 | availablePredicate@(Predicate c t) <- availablePredicates
                                 , t == tp
                                 , path <- superclassPaths c className classEnv
                                 ] of
                             []     -> Nothing
                             (path,fromPredicate):_ -> 
                                let list = reverse (zip path (tail path))
                                    tree = foldr (uncurry BySuperClass) (ByPredicate fromPredicate) list
                                in Just tree 
                                
      _      -> case byInstance noOrderedTypeSynonyms classEnv p of
                   Nothing -> internalError "DictionaryEnvironment" "makeDictionaryTree" ("reduction error" ++ show (M.assocs classEnv))
                   Just predicates -> 
                      do let (TCon instanceName, _) = leftSpine tp
                         trees <- makeDictionaryTrees classEnv availablePredicates predicates
                         return (ByInstance className instanceName trees)

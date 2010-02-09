{-| Module      :  ImportEnvironment
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.ModuleSystem.ImportEnvironment where

import qualified Data.Map as M
import Helium.Utils.Utils (internalError)
import Helium.Syntax.UHA  (Name)
import Helium.Syntax.UHA_Utils
import Top.Types
import Helium.Parser.OperatorTable
import Helium.StaticAnalysis.Messages.Messages () -- instance Show Name
import Helium.StaticAnalysis.Heuristics.RepairHeuristics (Siblings)
import Helium.StaticAnalysis.Directives.TS_Core
import Data.List 
import Data.Maybe (catMaybes)

type TypeEnvironment             = M.Map Name TpScheme
type ValueConstructorEnvironment = M.Map Name TpScheme
type TypeConstructorEnvironment  = M.Map Name Int
type TypeSynonymEnvironment      = M.Map Name (Int, Tps -> Tp)

type ImportEnvironments = [ImportEnvironment]
data ImportEnvironment  = 
     ImportEnvironment { -- types
                         typeConstructors  :: TypeConstructorEnvironment
                       , typeSynonyms      :: TypeSynonymEnvironment
                       , typeEnvironment   :: TypeEnvironment       
                         -- values
                       , valueConstructors :: ValueConstructorEnvironment
                       , operatorTable     :: OperatorTable
--                         -- type classes
--                       , classEnvironment  :: ClassEnvironment
                         -- other
                       , typingStrategies  :: Core_TypingStrategies 
                       }

emptyEnvironment :: ImportEnvironment
emptyEnvironment = ImportEnvironment 
   { typeConstructors  = M.empty
   , typeSynonyms      = M.empty
   , typeEnvironment   = M.empty
   , valueConstructors = M.empty
   , operatorTable     = M.empty
--   , classEnvironment  = emptyClassEnvironment
   , typingStrategies  = [] 
   }
                                              
addTypeConstructor :: Name -> Int -> ImportEnvironment -> ImportEnvironment                      
addTypeConstructor name int importenv = 
   importenv {typeConstructors = M.insert name int (typeConstructors importenv)} 

-- add a type synonym also to the type constructor environment   
addTypeSynonym :: Name -> (Int,Tps -> Tp) -> ImportEnvironment -> ImportEnvironment                      
addTypeSynonym name (arity, function) importenv = 
   importenv { typeSynonyms     = M.insert name (arity, function) (typeSynonyms importenv)
             , typeConstructors = M.insert name arity (typeConstructors importenv)
             } 

addType :: Name -> TpScheme -> ImportEnvironment -> ImportEnvironment                      
addType name tpscheme importenv = 
   importenv {typeEnvironment = M.insert name tpscheme (typeEnvironment importenv)}

addToTypeEnvironment :: TypeEnvironment -> ImportEnvironment -> ImportEnvironment
addToTypeEnvironment new importenv =
   importenv {typeEnvironment = typeEnvironment importenv `M.union` new} 
   
addValueConstructor :: Name -> TpScheme -> ImportEnvironment -> ImportEnvironment                      
addValueConstructor name tpscheme importenv = 
   importenv {valueConstructors = M.insert name tpscheme (valueConstructors importenv)}

addOperator :: Name -> (Int,Assoc) -> ImportEnvironment -> ImportEnvironment  
addOperator name pair importenv = 
   importenv {operatorTable = M.insert name pair (operatorTable importenv) } 
   
setValueConstructors :: M.Map Name TpScheme -> ImportEnvironment -> ImportEnvironment  
setValueConstructors new importenv = importenv {valueConstructors = new} 

setTypeConstructors :: M.Map Name Int -> ImportEnvironment -> ImportEnvironment     
setTypeConstructors new importenv = importenv {typeConstructors = new}

setTypeSynonyms :: M.Map Name (Int,Tps -> Tp) -> ImportEnvironment -> ImportEnvironment  
setTypeSynonyms new importenv = importenv {typeSynonyms = new}

setTypeEnvironment :: M.Map Name TpScheme -> ImportEnvironment -> ImportEnvironment 
setTypeEnvironment new importenv = importenv {typeEnvironment = new}

setOperatorTable :: OperatorTable -> ImportEnvironment -> ImportEnvironment 
setOperatorTable new importenv = importenv {operatorTable = new}

getOrderedTypeSynonyms :: ImportEnvironment -> OrderedTypeSynonyms
getOrderedTypeSynonyms importEnvironment = 
   let synonyms = let insert name = M.insert (show name)
                  in M.foldWithKey insert M.empty (typeSynonyms importEnvironment)
       ordering = fst (getTypeSynonymOrdering synonyms)
   in (ordering, synonyms)

{-
setClassEnvironment :: ClassEnvironment -> ImportEnvironment -> ImportEnvironment
setClassEnvironment new importenv = importenv { classEnvironment = new }
-}

addTypingStrategies :: Core_TypingStrategies -> ImportEnvironment -> ImportEnvironment  
addTypingStrategies new importenv = importenv {typingStrategies = new ++ typingStrategies importenv}

removeTypingStrategies :: ImportEnvironment -> ImportEnvironment  
removeTypingStrategies importenv = importenv {typingStrategies = []}

getSiblingGroups :: ImportEnvironment -> [[String]]
getSiblingGroups importenv = 
   [ xs | Core_TypingStrategy_Siblings xs <- typingStrategies importenv ]

getSiblings :: ImportEnvironment -> Siblings
getSiblings importenv =
   let f s = [ (s, ts) | ts <- findTpScheme (nameFromString s) ]
       findTpScheme n = 
          catMaybes [ M.lookup n (valueConstructors importenv)
                    , M.lookup n (typeEnvironment   importenv)
                    ]
   in map (concatMap f) (getSiblingGroups importenv) 
         
combineImportEnvironments :: ImportEnvironment -> ImportEnvironment -> ImportEnvironment
combineImportEnvironments (ImportEnvironment tcs1 tss1 te1 vcs1 ot1 xs1) (ImportEnvironment tcs2 tss2 te2 vcs2 ot2 xs2) = 
   ImportEnvironment 
      (tcs1 `exclusiveUnion` tcs2) 
      (tss1 `exclusiveUnion` tss2)
      (te1  `exclusiveUnion` te2 )
      (vcs1 `exclusiveUnion` vcs2)
      (ot1  `exclusiveUnion` ot2)
      (xs1 ++ xs2)

exclusiveUnion :: Ord key => M.Map key a -> M.Map key a -> M.Map key a
exclusiveUnion m1 m2 =
   let keys = M.keys (M.intersection m1 m2)
       f m  = foldr (M.update (const Nothing)) m keys
   in f m1 `M.union` f m2

{-
-- Bastiaan:
-- For the moment, this function combines class-environments.
-- The only instances that are added to the standard instances 
-- are the derived Show instances (Show has no superclasses).
-- If other instances are added too, then the class environment
-- should be split into a class declaration environment, and an
-- instance environment.
combineClassDecls :: ([[Char]],[(Predicate,[Predicate])]) -> 
                     ([[Char]],[(Predicate,[Predicate])]) ->
                     ([[Char]],[(Predicate,[Predicate])])
combineClassDecls (super1, inst1) (super2, inst2)
   | super1 == super2 = (super1, inst1 ++ inst2)
   | otherwise        = internalError "ImportEnvironment.hs" "combineClassDecls" "cannot combine class environments"
-}

-- Bastiaan:
-- Create a class environment from the dictionaries in the import environment
createClassEnvironment :: ImportEnvironment -> ClassEnvironment
createClassEnvironment importenv = 
    let  dicts = map (drop (length dictPrefix) . show) 
               . M.keys 
               . M.filterWithKey isDict 
               $ typeEnvironment importenv
         isDict n _ = dictPrefix `isPrefixOf` show n
         dictPrefix = "$dict"
         classes = ["Eq","Num","Ord","Enum","Show"]
         -- TODO: put $ between class name and type in dictionary name
         --  i.e. $dictEq$Int instead of $dictEqInt
         splitDictName ('E':'q':t) = ("Eq", t)
         splitDictName ('N':'u':'m':t) = ("Num", t)
         splitDictName ('O':'r':'d':t) = ("Ord", t)
         splitDictName ('E':'n':'u':'m':t) = ("Enum", t)
         splitDictName ('S':'h':'o':'w':t) = ("Show", t)
         splitDictName x = internalError "ImportEnvironment" "splitDictName" ("illegal dictionary: " ++ show x)
         arity s | s == "()" = 0
                 | isTupleConstructor s = length s - 1
                 | otherwise = M.findWithDefault
                                  (internalError "ImportEnvironment" "splitDictName" ("unknown type constructor: " ++ show s))                            
                                  (nameFromString s)
                                  (typeConstructors importenv) 
         dictTuples = [ (c, makeInstance c (arity t) t) 
                      | d <- dicts, let (c, t) = splitDictName d 
                      ]
         
         classEnv = foldr 
                    (\(className, inst) e -> insertInstance className inst e) 
                    superClassRelation 
                    dictTuples
    in classEnv

superClassRelation :: ClassEnvironment
superClassRelation = M.fromList $ 
   [ ("Num",  ( ["Eq","Show"],   []))
   , ("Enum", ( [],              []))
   , ("Eq" ,  ( [],              []))
   , ("Ord",  ( ["Eq"],          []))
   , ("Show", ( [],              []))
   ]

makeInstance :: String -> Int -> String -> Instance
makeInstance className nrOfArgs tp =
   let tps = take nrOfArgs [ TVar i | i <- [0..] ] 
   in ( Predicate className (foldl TApp (TCon tp) tps)
      , [ Predicate className x | x <- tps ] 
      )

instance Show ImportEnvironment where
   show (ImportEnvironment tcs tss te vcs ot _) = 
      unlines (concat [ fixities
                      , datatypes
                      , typesynonyms
                      , valueConstructors
                      , functions
                      ])
    where
    
       fixities =    
          let sorted  = let cmp (name, (prio, assoc)) = (10 - prio, assoc, not (isOperatorName name), name)
                        in sortBy (\x y -> cmp x `compare` cmp y) (M.assocs ot)
              grouped = groupBy (\x y -> snd x == snd y) sorted
              list = let f ((name, (prio, assoc)) : rest) =
                            let names  = name : map fst rest 
                                prefix = (case assoc of
                                             AssocRight -> "infixr"
                                             AssocLeft  -> "infixl"
                                             AssocNone  -> "infix "
                                         )++" "++ show prio ++ " "
                            in prefix ++ foldr1 (\x y -> x++", "++y) (map showNameAsOperator names)
                     in map f grouped          
          in showWithTitle "Fixity declarations" list
       
       datatypes = 
          let allDatas = filter ((`notElem` M.keys tss). fst) (M.assocs tcs)
              (xs, ys) = partition (isIdentifierName . fst) allDatas
              list     = map f (ys++xs)
              f (n,i)  = unwords ("data" : (showNameAsVariable n) : take i variableList)
          in showWithTitle "Data types" list
       
       typesynonyms =
          let (xs, ys)    = partition (isIdentifierName . fst) (M.assocs tss)
              list        = map f (ys++xs)
              f (n,(i,g)) = let tcons =  take i (map TCon variableList)
                            in unwords ("type" : showNameAsVariable n : map show tcons ++ ["=", show (g tcons)])               
          in showWithTitle "Type synonyms" list  
                 
       valueConstructors =
          let (xs, ys) = partition (isIdentifierName . fst) (M.assocs vcs)
              list     = map (\(n,t) -> showNameAsVariable n ++ " :: "++show t) (ys++xs)         
          in showWithTitle "Value constructors" list    
                 
       functions = 
          let (xs, ys) = partition (isIdentifierName . fst) (M.assocs te)
              list     = map (\(n,t) -> showNameAsVariable n ++ " :: "++show t) (ys++xs)
          in showWithTitle "Functions" list                  
       
       showWithTitle title xs
          | null xs   = []
          | otherwise = (title++":") : map ("   "++) xs
       
instance Ord Assoc where
  x <= y = let f AssocLeft  = 0
               f AssocRight = 1
               f AssocNone  = 2
           in f x <= f y
